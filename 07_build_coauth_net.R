## TODO: save net, net_gc
## Setup ----
library(tidyverse)
library(lubridate)
library(igraph)
library(tidygraph)

sloan_authors = read_rds('../Eisen-data/03_sloan_authors.Rds') %>%
    unnest(auid) %>%
    pull(auid)
sloan_papers = read_csv('../Eisen-data/00_Sloan.csv') %>% 
    filter(!is.na(DOI)) %>%
    pull(DOI) %>%
    unique()


abstracts_df = read_rds('../Eisen-data/05_abstracts.Rds')  %>%
    select(-keywords, -references) %>%
    mutate(date = ymd(date), 
           year = year(date)) %>%
    unnest(authors, .drop = FALSE) %>%
    mutate(sloan_paper = doi %in% sloan_papers, 
           sloan_author = auid %in% sloan_authors)


## Node and edge dfs ----
## Edges: docs w/ more than 2 authors in the dataset
edges = abstracts_df %>%
    select(scopus_id:date, year, sloan_paper, auid) %>%
    ## Make pairs by joining on everything other than auids
    full_join(., ., 
              by = names(.)[!(names(.) %in% 'auid')]) %>%
    filter(auid.x < auid.y) %>%
    ## Put the auids on the left side of the df and rename
    select(auid.x, auid.y, everything()) %>%
    ## Simplify
    count(auid.x, auid.y, year, sloan_paper) %>%
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
    mutate(frac = nn / sum(nn))

net_gc = net %>%
    morph(to_components) %>%
    mutate(component_size = graph_order()) %>%
    unmorph() %>% 
    filter(component_size == max(component_size)) %>%
    select(-component, -component_size)

## Save output ----
write_rds(net, '07_net_full.Rds')
write_rds(net_gc, '07_net_gc.Rds')
