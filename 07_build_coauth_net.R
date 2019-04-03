## Setup ----
library(tidyverse)
library(lubridate)
library(igraph)
library(tidygraph)

library(assertthat)

# sloan_authors = read_rds('../MoBE-data/03_sloan_authors.Rds') %>%
#     filter(n_collaboration > 1) %>%
#     unnest(auid) %>%
#     pull(auid)

## NB logic here should be same as 04
comparison_df_unfltd = read_rds('../MoBE-data/03_comparison_authors.Rds')
sloan_authors = read_rds('../MoBE-data/03_sloan_authors.Rds') %>%
    unnest() %>%
    mutate(in_comparison = auid %in% unlist(comparison_df_unfltd$auid)) %>%
    filter(n_collaboration > 1 | in_comparison) %>%
    pull(auid)
comparison_authors = read_rds('../MoBE-data/03_comparison_authors.Rds') %>%
    unnest() %>%
    ungroup() %>%
    filter(!(auid %in% sloan_authors)) %>%
    pull(auid)

assert_that(is_empty(intersect(sloan_authors, comparison_authors)), 
                        msg = 'Author lists overlap')

sloan_papers = read_csv('../MoBE-data/00_Sloan.csv') %>%
    filter(!is.na(DOI)) %>%
    pull(DOI) %>%
    unique()

abstracts_df = read_rds('../MoBE-data/05_abstracts.Rds')  %>%
    select(-keywords, -references) %>%
    mutate(date = ymd(date), 
           year = year(date)) %>%
    unnest(authors, .drop = FALSE) %>%
    mutate(sloan_paper = doi %in% sloan_papers, 
           sloan_author = auid %in% sloan_authors)

## 05 filters out auids that aren't in the list to retrieve, so non-Sloan authors == comparison authors
abstracts_df %>%
    filter(!sloan_author) %>%
    pull(auid) %>%
    unique() %>%
    length() %>%
    are_equal(length(comparison_authors), 
                          msg = 'Length mismatch between non-Sloan and comparison authors')


## Paper count table ----
n_distinct(abstracts_df$scopus_id)

abstracts_df %>% 
    group_by(scopus_id) %>% 
    summarize(mobe_paper = any(sloan_paper), 
              mobe_authors = any(sloan_author), 
              comp_authors = any(!sloan_author)) %>% 
    mutate(author_case = case_when(mobe_authors & comp_authors ~ 'mixed', 
                                   mobe_authors & !comp_authors ~ 'mobe only', 
                                   !mobe_authors & comp_authors ~ 'comp only', 
                                   TRUE ~ NA_character_)) %>% 
    count(mobe_paper, author_case) %>% 
    arrange(desc(n)) %>% 
    ## Confirm that paper counts add up correctly
    pull(n) %>% 
    sum() %>% 
    are_equal(n_distinct(abstracts_df$scopus_id))


## Node and edge dfs ----
## Edges: docs w/ more than 2 authors in the dataset
edges_unsimp = abstracts_df %>%
    select(scopus_id:date, year, sloan_paper, auid, sloan_author) %>%
    ## Make pairs by joining on everything other than auids
    full_join(., ., 
              by = names(.)[!(names(.) %in% c('auid', 'sloan_author'))]) %>%
    filter(auid.x < auid.y) %>%
    ## Put the auids on the left side of the df and rename
    select(auid.x, auid.y, everything()) %>%
    mutate(heterophily = sloan_author.x != sloan_author.y)

## Simplify
edges = edges_unsimp %>%
    count(auid.x, auid.y, year, sloan_paper, heterophily) %>%
    mutate(year_g = cut(year, 4))

## Nodes:  authors
authors = abstracts_df %>%
    count(auid, surname, given_name, sloan_author) %>%
    group_by(auid) %>%
    summarize(surname = first(surname[which(n == max(n))]), 
              given_name = first(given_name[which(n == max(n))]), 
              sloan_author = sum(sloan_author) > 0, 
              n = sum(n))


## Build network ----
net = as_tbl_graph(edges, directed = FALSE) %>% 
    activate(nodes) %>%
    inner_join(authors, by = c('name' = 'auid')) %>%
    mutate(component = group_components())

## Size of connected components
## GC contains 97% of all nodes
net %>%
    activate(nodes) %>%
    as_tibble() %>%
    count(component) %>%
    mutate(frac = n / sum(n))

net_gc = net %>%
    morph(to_components) %>%
    mutate(component_size = graph_order()) %>%
    unmorph() %>% 
    filter(component_size == max(component_size)) %>%
    select(-component, -component_size)

## Authors who were dropped bc they didn't have collaborations in the data
authors %>% 
    filter(! auid %in% V(net)$name) %>% 
    count(sloan_author)

## Save output ----
write_rds(net, '../MoBE-data/07_net_full.Rds')
write_rds(net_gc, '../MoBE-data/07_net_gc.Rds')
