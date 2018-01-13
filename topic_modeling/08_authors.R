library(tidyverse)
library(cowplot)
library(lubridate)

## Base-2 log, with log 0 = 0 for convenience
lg0 = function (x) {
    if (x > 0) {
        return(log2(x))
    } else {
        return(0)
    }
}
lg0 = Vectorize(lg0)


## Document metadata
load('../../Eisen-data/02_Scopus.Rdata')
load('../../Eisen-data/04_abstracts.Rdata')
abstracts_df = abstracts_df %>%
    select(-keywords, -abstract) %>%
    mutate(in_collab = scopus_id %in% scopus_data$sid)
abstracts_df$date = ymd(abstracts_df$date)
abstracts_df$year = year(abstracts_df$date)
rm(scopus_data)

## LDA output
lda = read_csv('../../Eisen-data/05-lda.csv') %>%
    mutate(auid = as.character(auid), 
           group_est = ifelse(topic_2 < .5, 'microbial', 'building'))
## Author-level entropy
lda = lda %>%
    gather(topic, gamma, topic_0:topic_2) %>%
    group_by(auid) %>%
    summarize(H = -sum(gamma * lg0(gamma))) %>%
    left_join(lda)

## --------------------
## Exploring LDA output
ggplot(lda, aes(topic_0, topic_1, fill = group)) + 
    geom_point(shape = 21, color = 'black', size = 4, stroke = 1, 
        aes(alpha = !is.na(group))) +
    scale_fill_brewer(palette = 'Set1', na.value = 'yellow') +
    scale_alpha_discrete(range = c(.2, 1), guide = FALSE) +
    stat_function(fun = function (x) .5 - x, color = 'black', xlim = c(0, .5))

lda %>%
    ggplot(aes(group, topic_2)) + 
    geom_jitter(width = .1) + 
    geom_hline(aes(yintercept = .5), linetype = 'dashed')

ggplot(lda, aes(H, color = group)) + geom_density() + geom_rug()


## ------------------
## Edges: docs w/ more than 2 authors in the dataset
edges = abstracts_df %>%
    unnest() %>%
    ## Make pairs by joining on everything other than auids
    full_join(., ., 
              by = names(.)[!(names(.) %in% 'auids')]) %>%
    filter(auids.x < auids.y) %>%
    ## Put the auids on the left side of the df and rename
    select(auid.x = auids.x, auid.y = auids.y, everything()) %>%
    ## Join w/ author topic distributions
    left_join(lda, by = c('auid.x' = 'auid'), 
              suffix = c('', '.x')) %>%
    left_join(lda, by = c('auid.y' = 'auid'),
              suffix = c('.x', '.y')) %>%
    ## Distance wrt topic 2
    mutate(topic_2_dist = abs(topic_2.x - topic_2.y)) %>%
    ungroup() %>%
    ## Bundle years
    mutate(year_g = cut(year, 3))
    
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

## Distribution of Hellinger distance between authors
edges %>%
    ggplot(aes(h_authors, color = in_collab)) + 
    geom_density() +
    geom_rug() +
    scale_x_continuous(trans = 'identity') +
    facet_grid(year_g ~ .)

## Distribution of topic_2 distances
ggplot(edges, aes(topic_2_dist, color = in_collab)) + 
    geom_density() +
    geom_rug() +
    scale_x_continuous(trans = 'identity') + 
    facet_grid(year_g ~ .)

edges %>%
    group_by(in_collab, year_g) %>%
    summarize_at(vars(h_authors, topic_2_dist), funs(mean, median))


## --------------------
## Build network
library(igraph)
library(ggraph)
net = graph_from_data_frame(edges, directed = FALSE, 
                            vertices = lda)

# net_simp = simplify(net)

## Plot
set.seed(123)
# ggraph(net) + 
#     geom_node_point(aes(color = topic_2), alpha = .7) +
#     # scale_color_brewer(palette = 'Set1', na.value = 'yellow') +
#     scale_color_gradient(low = 'blue', high = 'red') +
#     geom_edge_fan(aes(), spread = 4, alpha = .5) +
#     # scale_edge_color_gradient(low = 'red', high = 'blue') +
#     scale_edge_alpha_continuous(range = c(0, .5), 
#                                 trans = 'identity') +
#     facet_edges(~ in_collab, ncol = 2) +
#     theme_graph(foreground = 'black', strip_text_colour = 'white')

## looks like consolidation of the subcommunities into the giant component happened around the time the collaboration was getting started

nets = unique(E(net)$year_g) %>%
    map(~ subgraph.edges(net, E(net)[E(net)$year_g == .x])) %>%
    `names<-`(unique(E(net)$year_g))

nets_over_time = map2(nets, names(nets),
    ~ {ggraph(.x) + 
        geom_node_point(aes(color = topic_2), alpha = .7) +
        # scale_color_brewer(palette = 'Set1', na.value = 'yellow') +
        scale_color_gradient(low = 'blue', high = 'red') +
        geom_edge_fan(alpha = .3, spread = 4) +
        # scale_edge_color_gradient(low = 'red', high = 'blue') +
        scale_edge_alpha_continuous(range = c(0, .5), 
                                    trans = 'identity') +
        facet_edges(~ in_collab, ncol = 2) +
        theme_graph(foreground = 'black', 
                    strip_text_colour = 'white') + 
        ggtitle(.y)})

# plot_grid(plotlist = nets_over_time, ncol = 2)


## Calculate network statistics over time
net_by_year = cross_df(list(year = unique(E(net)$year), 
                            in_collab = c(TRUE, FALSE))) %>%
    mutate(net = map2(year, in_collab, ~ subgraph.edges(net, which((E(net)$year == .x) & (E(net)$in_collab == .y))))) %>%
    mutate(size = unlist(map(net, ~ length(V(.x))))) %>%
    filter(size > 0)
net_by_year = net_by_year$net %>%
    map_dfr(~ tibble(n_comp = components(.x)$no,
                     gc = {components(.x)$csize %>% {./sum(.)} %>% {.[which(. == max(.))]}}, 
                     H = {components(.x)$csize %>% {./sum(.)} %>% {-sum(. * log2(.))}},
                     # mean_distance = mean_distance(.x), 
                     # diameter = diameter(.x), 
                     transitivity = transitivity(.x),
                     # density = length(E(.x)) / (length(V(.x))*(length(V(.x))-1)/2) 
                     topic_2_dist = mean(E(.)$topic_2_dist)
    )) %>%
    bind_cols(net_by_year, .) %>%
    # mutate_at(vars(gc, mean_distance, diameter), funs(. / size)) %>%
    # mutate(diameter = log10(diameter)) %>%
    select(-net)

net_by_year %>%
    gather(statistic, value, -year, -in_collab) %>%
    ggplot(aes(year, value, color = in_collab)) + 
    geom_line() +
    geom_hline(yintercept = 0, color = 'grey80') +
    # scale_y_log10() +
    facet_wrap(~ statistic, scales = 'free')

