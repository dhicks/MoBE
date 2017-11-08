library(tidyverse)
library(cowplot)
library(lubridate)

load('../../Eisen-data/04_abstracts.Rdata')
load('../../Eisen-data/06_lda_dfs.Rdata')
load('../../Eisen-data/02_Scopus.Rdata')
abstracts_df = abstracts_df %>%
    mutate(in_collab = scopus_id %in% scopus_data$sid)
rm(scopus_data)

## Combine LDA output with metadata
docs_df = left_join(documents_lda, abstracts_df) %>%
    mutate(date = ymd(date), 
           year = year(date))

## Reshape into author-level info
authors_df = docs_df %>%
    select(-keywords) %>%
    unnest() %>% 
    group_by(auids) %>%
    summarize_at(vars(starts_with('topic')), mean) %>%
    ungroup()

## Means for the 2-topic model very cleanly separate the two groups of authors
ggplot(authors_df, aes(topic_1)) + 
    geom_density() +
    geom_rug()

## Docs w/ more than 2 authors (in the dataset)
edges = docs_df %>%
    select(scopus_id, topic_1, doi, year, in_collab, 
           auids) %>%
    unnest() %>%
    full_join(., ., by = c('scopus_id', 'topic_1', 'doi', 'year', 
                           'in_collab')) %>%
    filter(auids.x < auids.y) %>%
    select(auids.x, auids.y, everything())

## Build network
library(igraph)
library(ggraph)
net = graph_from_data_frame(edges, directed = FALSE, 
                            vertices = authors_df)

net_simp = simplify(net)

## Plot
set.seed(123)
ggraph(net) +
    # geom_node_label(aes(label = surnames, fill = topic_1),
    #                 color = 'white') +
    geom_node_point(aes(color = topic_1)) +
    geom_edge_fan(aes(color = topic_1), alpha = .5, spread = 4) +
    facet_wrap(~ in_collab) +
    theme_graph(foreground = 'black', strip_text_colour = 'white')


thing = left_join(edges, authors_df, by = c('auids.x' = 'auids')) %>%
    select(-topic_2) %>%
    rename(topic_1.paper = topic_1.x, 
           topic_1.auth1 = topic_1.y) %>%
    left_join(authors_df, by = c('auids.y' = 'auids')) %>%
    select(-topic_2) %>%
    rename(topic_1.auth2 = topic_1)

thing %>%
    filter(year >= 2010) %>%
    ggplot(aes(topic_1.auth1, topic_1.auth2)) + 
    geom_point(aes(color = topic_1.paper)) + 
    geom_smooth(method = 'lm', color = 'red') + 
    facet_grid(in_collab ~ year)
