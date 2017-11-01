library(tidyverse)
library(cowplot)
library(lubridate)

load('../../Eisen-data/02_abstracts.Rdata')
abstracts_df = abstracts_df %>%
    select(-raw)
load('../../Eisen-data/04_lda_dfs.Rdata')
load('../../Eisen-data/06_Scopus.Rdata')
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
    group_by(author_ids, surnames, given_names, author_groups) %>%
    summarize_at(vars(starts_with('topic')), mean) %>%
    ungroup()

## Means for the 2-topic model very cleanly separate the two groups of authors
ggplot(authors_df, aes(author_groups, topic_1)) + 
    geom_point()

## Docs w/ more than 2 authors (in the dataset)
edges = docs_df %>%
    select(scopus_id, topic_1, doi, year, in_collab, 
           author_ids) %>%
    unnest() %>%
    full_join(., ., by = c('scopus_id', 'topic_1', 'doi', 'year', 
                           'in_collab')) %>%
    filter(author_ids.x < author_ids.y) %>%
    select(author_ids.x, author_ids.y, everything())

## Build network
library(igraph)
library(ggraph)
net = graph_from_data_frame(edges, directed = FALSE, 
                            vertices = authors_df)

## Plot
set.seed(123)
ggraph(net) +
    geom_node_label(aes(label = surnames, fill = topic_1),
                    color = 'white') +
    geom_edge_fan(aes(color = topic_1), alpha = .5, spread = 4) +
    facet_wrap(~ in_collab) +
    theme_graph(foreground = 'black', strip_text_colour = 'white')
