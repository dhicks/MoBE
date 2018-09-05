library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

net = read_rds('../Eisen-data/07_net_full.Rds')
net_gc = read_rds('../Eisen-data/07_net_gc.Rds')

## Plot GC over time ----
## - GC includes 97% of authors
net %>%
    activate(nodes) %>%
    as_tibble() %>%
    count(component) %>%
    mutate(frac = nn / sum(nn))

## - Only 2 components contain Sloan authors; 99.5% in GC
net %>%
    activate(nodes) %>%
    as_tibble() %>%
    count(component, sloan_author) %>%
    arrange(desc(sloan_author), component) %>%
    group_by(sloan_author) %>%
    mutate(frac = nn / sum(nn))

## - Consolidation of Sloan authors as collaboration develops
plots = net_gc %>%
    activate(edges) %>%
    ## Split into year groups
    morph(to_split, year_g, split_by = 'edges') %>%
    ## Remove isolated nodes
    activate(nodes) %>%
    mutate(degree = centrality_degree()) %>%
    filter(degree > 1) %>%
    crystallize() %>%
    ## Generate plots for each year group
    mutate(years = str_extract(name, '\\(.*\\]'),
           plot = map2(graph, years,
                       ~ ggraph(.x, layout = 'fr') +
                           geom_edge_fan(aes(color = sloan_paper), #width = n), 
                                         spread = 4, alpha = .25) +
                           geom_node_point(aes(color = sloan_author, #size = n, 
                                               alpha = .5,
                                               label = surname)) +  ## switch to geom_node_point to display author names
                           scale_edge_color_brewer(palette = 'Set1') +
                           scale_color_brewer(palette = 'Set1') +
                           # scale_edge_width(range = c(.5, 1)) +
                           # scale_size(range = c(.5, 2)) +
                           ggtitle(.y) +
                           theme_graph(title_size = 8) +
                           theme(legend.position = 'none')))
# plots$plot[[4]]
cowplot::plot_grid(plotlist = plots$plot)
ggsave('08_nets.png', height = 6, width = 6)
cowplot::plot_grid(plotlist = plots$plot, nrow = 1)
ggsave('08_nets_wide.png', height = 4, width = 12)
## Caption:  Evolution of the extended collaboration network over time.  Points are authors; edges are annual publication collections.  Node and edge color distinguish Sloan authors and papers (blue) from authors/papers outside the collaboration (red).  Only authors in the giant component of the full (non-dynamic) network are included; only authors with at least 1 coauthor are included in each panel.  97% of all authors are in the giant component; 99.5% of Sloan collaboration authors are in the giant component.  Network layouts are calculated separately for each panel using the Fruchterman-Reingold algorithm with default parameters. 


## Network statistics over time ----
## Heterophily over time
net_gc %>%
    activate(edges) %>%
    as_tibble() %>%
    group_by(year, heterophily) %>%
    summarize(n = sum(n)) %>%
    ungroup() %>%
    spread(heterophily, n) %>%
    mutate(frac_heterophily = `TRUE` / (`TRUE` + `FALSE`)) %>%
    ggplot(aes(year, frac_heterophily)) +
    geom_line() +
    theme_minimal()

## These do the same thing; filter down edges, but don't remove any nodes
# net %>%
#     activate(edges) %>%
#     convert(to_subgraph, year <= c(2004))
# 
# net %>%
#     activate(edges) %>%
#     filter(year <= 2004)


## NB whole network, not just GC
net_by_year = cross_df(list(year = as.integer(unique(E(net)$year)), 
                            sloan_author = c(TRUE, FALSE))) %>%
    arrange(year, sloan_author) %>%
    mutate(net = map2(year, sloan_author, 
                      ~ {net %>%
                              subgraph.edges(which((E(net)$year <= .x))) %>%
                              induced_subgraph(., which((V(.)$sloan_author == .y)))})) %>%
    mutate(size = map_int(net, gorder)) %>%
    filter(size > 0) %>%
    mutate(n_comp = map_dbl(net, count_components), 
           gc = map_dbl(net, ~ components(.)$csize %>% {./sum(.)} %>% max()),
           H = map_dbl(net, ~ {components(.)$csize %>% {./sum(.)} %>% {-sum(.*log2(.))}}), 
           mean_distance = map_dbl(net, mean_distance), 
           diameter = map_dbl(net, diameter), 
           transitivity = map_dbl(net, transitivity), 
           density = map_dbl(net, ~ edge_density(simplify(.))))

stat_labels = tribble(
    ~ statistic, ~ pretty_name, 
    'size', 'num. authors', 
    'n_comp', 'num. components', 
    'gc', 'giant component coverage', 
    'H', 'H', 
    'diameter', 'diameter', 
    'mean_distance', 'mean distance', 
    'density', 'density', 
    'transitivity', 'transitivity'
) %>%
    mutate(pretty_name = fct_inorder(pretty_name))

# ggplot(net_by_year, aes(year, H, color = sloan_author)) + 
#     geom_line() + geom_point()

net_by_year %>%
    select(year, sloan_author, size:density) %>%
    gather(key = statistic, value = value, size:density) %>%
    left_join(stat_labels) %>%
    ggplot(aes(year, value, color = sloan_author)) + 
    geom_vline(xintercept = 2008, color = 'grey40') +
    geom_line() +
    geom_point(size = .5) +
    scale_color_brewer(palette = 'Set1', name = 'author set', labels = c('comparison', 'collaboration')) +
    ylab('') +
    facet_wrap(~ pretty_name, scales = 'free') +
    theme_minimal(base_size = 8)

ggsave('08_net_over_time.png', height = 4, width = 6)



## Novel collaborations ----
## Something like 5/8 of all coauthor dyads have their first pub in the collaboration
# net %>%
#     activate(edges) %>%
#     as_tibble() %>%
#     group_by(auid.x, auid.y, in_collab) %>%
#     summarize(first_year = min(year)) %>%
#     spread(in_collab, first_year) %>%
#     mutate(first_in_collab = is.na(`FALSE`) | (`TRUE` < `FALSE`)) %>%
#     replace_na(list('first_in_collab' = FALSE)) %>%
#     pull(first_in_collab) %>%
#     table()


