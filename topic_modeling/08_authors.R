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
    select(scopus_id, H, starts_with('topic'), 
           doi, year, in_collab, 
           auids) %>%
    unnest() %>%
    ## Make pairs by joining on everything other than auids
    full_join(., ., 
              by = names(.)[!(names(.) %in% 'auids')]) %>%
    filter(auids.x < auids.y) %>%
    ## Put the auids on the left side of the df and rename
    select(auid.x = auids.x, auid.y = auids.y, everything()) %>%
    ## Join w/ author topic distributions
    left_join(authors_df, by = c('auid.x' = 'auids'), 
              suffix = c('', '.x')) %>%
    left_join(authors_df, by = c('auid.y' = 'auids'),
              suffix = c('.paper', '.y'))
    
## Hellinger distance https://en.wikipedia.org/wiki/Hellinger_distance#Discrete_distributions
## h^2(p, q) = 1 - sum sqrt(p_i * q_i)
authors_hellinger = edges %>%
    ## Work w/ just auids and topics
    select(auid.x, auid.y, matches('topic_[0-9]+\\.[xy]')) %>%
    filter(!duplicated(.)) %>%
    ## Reshape to align corresponding topics
    gather(topic.x, gamma.x, matches('topic_[0-9]+\\.x')) %>%
    gather(topic.y, gamma.y, matches('topic_[0-9]+\\.y')) %>%
    mutate_at(vars(starts_with('topic')), 
              funs(str_extract(., '[0-9]+'))) %>%
    filter(topic.x == topic.y) %>%
    ## Calculate Hellinger distance
    mutate(sqrt_pq = sqrt(gamma.x * gamma.y)) %>%
    group_by(auid.x, auid.y) %>%
    summarize(h_sqr = 1 - sum(sqrt_pq)) %>%
    ungroup() %>%
    mutate(h_authors = sqrt(h_sqr))

edges = left_join(edges, authors_hellinger)

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
    ggplot(aes(h_authors)) + 
    geom_density() +
    geom_rug() +
    facet_grid(post2010 ~ in_collab)
