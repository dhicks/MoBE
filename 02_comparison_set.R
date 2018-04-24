library(tidyverse)

## Load data --------------------
## Papers in the Sloan collaboration
collab_df = read_csv('../../Eisen-data/00_Sloan.csv')
## All papers from these journals
load('../../Eisen-data/05_all_papers.Rdata')

## Drop journals with a high number of publications
## Assumption here is that these are more general
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
## The 1k most prolific authors in the same journals who were not part of the collaboration
comparison_set = author_counts %>%
    filter(!in_collab) %>%
    arrange(desc(n)) %>%
    slice(1:1000)

all_papers_df %>% 
    inner_join(comparison_set, by = c('given', 'family')) %>%
    select(container.title:title, given, family) %>%
    add_count(doi) %>%
    arrange(desc(n)) %>%
    group_by(given, family) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    nest(given, family)


