## This script uses the paper list produced by 01_collab_journals to build a comparison set of authors.  

library(tidyverse)
n_authors = 1000 # Target size for comparison set

## Load data --------------------
## Papers in the Sloan collaboration
collab_df = read_csv('../MoBE-data/00_Sloan.csv')
## All papers from the same journals
load('../MoBE-data/01_all_papers.Rdata')

## Drop journals with a high number of publications
## Assumption here is that these are more general
count(all_papers_df, container.title) %>%
    arrange(desc(n)) %>% 
    filter(n >= 10000) %>%
    pull(n) %>% sum()

all_papers_df = all_papers_df %>%
    add_count(container.title) %>%
    filter(n < 10000)

all_papers_df = all_papers_df %>%
    mutate(in_collab = doi %in% collab_df$DOI) %>%
    unnest()

## Count papers by author -----
author_counts = all_papers_df %>%
    filter(!is.na(given), !is.na(family)) %>%
    group_by(given, family) %>%
    summarize(n = n(), in_collab = sum(in_collab) > 0) %>%
    ungroup()
    
ggplot(author_counts, aes(n, color = in_collab)) + 
    stat_ecdf() + 
    scale_x_log10()

table(author_counts$in_collab)

## Comparison set -----
## The most prolific authors in the same journals who were not part of the collaboration
comparison_set = author_counts %>%
    filter(!in_collab) %>%
    arrange(desc(n)) %>%
    slice(1:n_authors)

## A df of papers covering the authors in the comparison set
## These have been selected to efficiently cover all the authors; 
## they should not be used except to identify Scopus author IDs
comparison_papers = all_papers_df %>% 
    inner_join(comparison_set, by = c('given', 'family')) %>%
    select(container.title:title, given, family) %>%
    add_count(doi) %>%
    arrange(desc(n)) %>%
    group_by(given, family) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    nest(given, family)

write_rds(comparison_papers, '../MoBE-data/02_comparison_papers.Rds')


