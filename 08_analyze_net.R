library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(furrr)

## Options for futures
plan(multiprocess, workers = 3)
options(future.globals.maxSize = 850*1024^2) # 850 MB

## Load data
net = read_rds('../MoBE-data/07_net_full.Rds')
net_gc = read_rds('../MoBE-data/07_net_gc.Rds')

## Paper count threshold to use for filtered variant
filter_threshold = 50

#+ hairy_ball
## Plot GC over time ----
## - GC includes 97% of authors
net %>%
    activate(nodes) %>%
    as_tibble() %>%
    count(component) %>%
    mutate(frac = n / sum(n))

## - Only 2 components contain Sloan authors; 99.5% in GC
net %>%
    activate(nodes) %>%
    as_tibble() %>%
    count(component, sloan_author) %>%
    arrange(desc(sloan_author), component) %>%
    group_by(sloan_author) %>%
    mutate(frac = n / sum(n))

## - No authors w/ degree 0
## This is a side effect of constructing the network using full_join()
net %>%
    mutate(deg = centrality_degree()) %>%
    filter(deg == 0)

## - Authors in small components
net %>%
    as_tibble() %>% 
    add_count(component, name = 'nn') %>% 
    filter(n < 10) %>% 
    rename(scopus_id = name) %>%
    mutate(scopus_url = str_c('https://www.scopus.com/authid/detail.uri?authorId=', scopus_id)) %>% 
    arrange(desc(n), surname, given_name)

## - Consolidation of Sloan authors as collaboration develops
## Convert half-open interval notation into dashed notation
ho_to_dashed = function(interval) {
    ends = str_extract_all(interval, '[0-9]{4}', simplify = TRUE)
    left = as.character(as.integer(ends[,1]) + 1)
    right = ends[,2]
    dashed = str_c(left, '-', right)
    return(dashed)
}

plots = net %>%
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
           years = ho_to_dashed(years),
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


#+ stats_over_time
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

## Split out into cumulative annual networks
## NB whole network, not just GC
net_by_year = cross_df(list(year = as.integer(unique(E(net)$year)), 
                            sloan_author = c(TRUE, FALSE), 
                            filtered = c(TRUE, FALSE))) %>%
    arrange(year, sloan_author, filtered) %>%
    ## Subset by year
    mutate(net = map2(year, sloan_author, 
                      ~ {net %>%
                              subgraph.edges(which((E(net)$year <= .x))) %>%
                              induced_subgraph(., which((V(.)$sloan_author == .y)))})) %>%
    ## Create filtered versions
    mutate(net = map(net, as_tbl_graph),
           net = map2(net, filtered, 
                      ~ifelse(.y, 
                              list(filter(.x, n >= filter_threshold)), 
                              list(.x))), 
           net = flatten(net)) %>% 
    mutate(size = map_int(net, gorder)) %>%
    filter(size > 0) %>%
    select(-size)

#+ rewire, cache = TRUE
## Rewire ----
## Generate rewired networks and calculate statistics
net %>%
    activate(edges) %>%
    as_tibble() %>%
    count(from, to) %>%
    ## from is always < to
    # mutate(lt = from < to) %>%
    # count(lt)
    pull(n) %>%
    summary()

n_sims = 100      # num. rewired networks to generate per year
frac_rewire = .05  # fraction of edges to rewire
set.seed(24680)

net_stats = net_by_year %>%
    # slice(1:10) %>%
    ## Draw rewired networks
    mutate(iteration = list(1:n_sims)) %>%
    unnest(iteration, .drop = FALSE) %>%
    mutate(rewire = future_map(net, 
                               ~ rewire(., 
                                        keeping_degseq(niter = floor(frac_rewire/2 * length(E(.))))), 
                               .progress = TRUE)#,
    ) %>%
    select(-iteration) %>%
    ## Long format
    gather(key = net_type, value = net, net, rewire) %>%
    filter(!duplicated(.)) %>%
    ## Calculate statistics
    mutate(size = future_map_int(net, gorder),
           n_comp = future_map_dbl(net, count_components), 
           gc = future_map_dbl(net, ~ components(.)$csize %>% {./sum(.)} %>% max()),
           H = future_map_dbl(net, ~ {components(.)$csize %>% {./sum(.)} %>% {-sum(.*log2(.))}}), 
           mean_distance = future_map_dbl(net, mean_distance), 
           diameter = future_map_dbl(net, diameter), 
           transitivity = future_map_dbl(net, transitivity), 
           density = future_map_dbl(net, ~ edge_density(simplify(.))))

#+ stat_plots
## Nice labels for the statistics
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

net_stats %>%
    select(-net) %>%
    gather(statistic, value, size:density) %>%
    # filter(statistic == 'n_comp') %>%
    left_join(stat_labels) %>%
    ggplot(aes(year, value)) +
    geom_vline(xintercept = 2008, color = 'grey40') +
    stat_summary(aes(fill = sloan_author),
                 geom = 'ribbon',
                 data = function(x) subset(x, net_type == 'rewire'),
                 fun.y = mean,
                 fun.ymax = function (y) {mean(y) + qnorm(.95)*sd(y)},
                 fun.ymin = function (y) {mean(y) + qnorm(.05)*sd(y)},
                 alpha = .25,
                 position = position_dodge(width = .5)) +
    geom_line(aes(color = sloan_author, linetype = filtered), 
              data = function(x) subset(x, net_type == 'net'), 
              size = .5) +
    scale_color_brewer(palette = 'Set1', name = 'author set', labels = c('comparison', 'collaboration')) +
    scale_fill_brewer(palette = 'Set1', name = 'author set', labels = c('comparison', 'collaboration')) +
    ylab('') +
    scale_linetype_discrete(name = 'filtering', 
                            labels = c('none', 
                                       str_c('≥', 
                                             filter_threshold, 
                                             ' papers'))) +
    facet_wrap(~ pretty_name, scales = 'free') +
    theme_minimal(base_size = 8)

ggsave('08_net_over_time.png', height = 4, width = 6)
ggsave('08_net_over_time.tiff', height = 4, width = 6, 
       compression = 'lzw')
## Caption:  Network statistics over time.  See text for explanation of the different statistics calculated here.  Solid lines correspond to observed values; shaded ribbons correspond to 90% confidence intervals on rewired networks, where 5% of the observed edges are randomly rewired while maintaining each node's degree distributions.  100 rewired networks are generated for each author set-year combination.  Blue corresponds to the MoBE collaboration; red corresponds to the peer comparison set of authors.  


#+ novel_collab, include = FALSE
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


