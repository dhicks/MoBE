library(tidyverse)
library(cowplot)
library(lubridate)

load('../../Eisen-data/04_abstracts.Rdata')
load('../../Eisen-data/06_lda_dfs.Rdata')
load('../../Eisen-data/02_Scopus.Rdata')
abstracts_df = abstracts_df %>%
    mutate(in_collab = scopus_id %in% scopus_data$sid)
rm(scopus_data)

## Document-wise entropy
## Base-2 log, with log 0 = 0 for convenience
lg0 = function (x) {
    if (x > 0) {
        return(log2(x))
    } else {
        return(0)
    }
}
lg0 = Vectorize(lg0)
documents_lda = documents_lda %>%
    gather(topic, gamma, -scopus_id) %>%
    group_by(scopus_id) %>%
    summarize(H = -sum(gamma * lg0(gamma))) %>%
    left_join(documents_lda)

## Combine LDA output with metadata
docs_df = left_join(documents_lda, abstracts_df) %>%
    mutate(date = ymd(date),
           year = year(date))


## Distribution of document-wise entropy
## The 2-topic model does an okay job of classifying papers outside the collaboration, but doesn't cleanly classify papers inside the collaboration
ggplot(docs_df, aes(H, fill = in_collab)) + 
    geom_density(alpha = .5)


## Reshape into author-level info
authors_df = docs_df %>%
    select(-keywords) %>%
    unnest() %>% 
    group_by(auids) %>%
    summarize_at(vars(starts_with('topic')), mean) %>%
    ungroup()
## Author-wise entropy
authors_df = authors_df %>%
    gather(topic, gamma, -auids) %>%
    group_by(auids) %>%
    summarize(H = -sum(gamma * lg0(gamma))) %>%
    left_join(authors_df)

## Means for the 2-topic model do a poor job of separating the two groups of authors
ggplot(authors_df, aes(H)) + geom_density() + geom_rug()


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
    geom_edge_fan(aes(color = topic_1.paper), alpha = .5, 
                  spread = 4) +
    facet_wrap(~ in_collab) +
    theme_graph(foreground = 'black', strip_text_colour = 'white')


edges %>%
    mutate(post2010 = ifelse(year >= 2010, '≥2010', '<2010')) %>%
    # group_by(post2010, in_collab) %>%
    # summarize(n = n(),
    #           median = median(topic_1.diff), 
    #           eightyth = quantile(topic_1.diff, probs = .8), 
    #           crosses = 1 - ecdf(topic_1.diff)(.5))
    ggplot(aes(topic_1.diff)) + 
    geom_density() +
    geom_rug() +
    facet_grid( ~ in_collab)
    
ggplot(edges, aes(scopus_id, topic_1.diff)) + geom_point()
