library(tidyverse)
library(cowplot)
library(stringr)

load('../quick_scopus_09_15.Rdata')

## Citation counts
## --------------------
scopus_data %>%
    select(title, cite_count, journal, doi) %>%
    arrange(desc(cite_count)) %>%
    head()

ggplot(scopus_data, aes(cite_count)) + 
    # stat_ecdf() +
    stat_density() +
    geom_rug() +
    scale_x_log10()

scopus_data %>%
    group_by(journal) %>%
    summarize(n_papers = n(),
              total_citations = sum(cite_count),
              rel_citations = total_citations / n_papers) %>%
    arrange(desc(total_citations)) %>%
    head()

sum(scopus_data$cite_count, na.rm = TRUE)
median(scopus_data$cite_count, na.rm = TRUE)


## Coauthor network
## --------------------
## Full network contains 1688 distinct authors and ~50k coauthor relations
## Filter to authors w/ >= x publications in the dataset
library(igraph)
library(ggraph)
library(jsonlite)

auths_df = scopus_data %>%
    filter(!is.na(sid)) %>%
    select(auid, surnames) %>%
    unnest() %>%
    group_by(auid) %>%
    summarize(surname = first(surnames), 
              n_papers = n())

ggplot(auths_df, aes(n_papers)) + 
    stat_ecdf() +
    geom_rug() +
    geom_vline(aes(xintercept = 2))

auths_to_keep = auths_df %>%
    filter(n_papers >= 1) %>%
    .$auid

coauths_df = scopus_data %>%
    filter(!is.na(sid)) %>%
    select(sid, auid, surnames) %>%
    unnest() %>%
    filter(auid %in% auths_to_keep) %>%
    full_join(., ., by = 'sid') %>%
    filter(auid.x < auid.y) %>%
    group_by(auid.x, auid.y) %>%
    summarize(n_copapers = n(), 
              surname.x = first(surnames.x), 
              surname.y = first(surnames.y)) %>%
    ungroup()

## Put everything into the network to calculate centrality correctly
coauths_net = coauths_df %>%
    select(auid.x, auid.y, n_copapers) %>%
    graph_from_data_frame(directed = FALSE,
                          vertices = filter(auths_df,
                                            auid %in% auths_to_keep))

V(coauths_net)$centrality = betweenness(coauths_net, 
                                        directed = FALSE, 
                                        normalized = TRUE,
                                        weights = E(coauths_net)$n_copapers)

## Then filter down to authors w/ >= x papers
coauths_net_fltd = coauths_net %>%
    induced_subgraph(V(coauths_net)[V(coauths_net)$n_papers >= 8])

coauths_plot = coauths_net %>%
    induced_subgraph(V(coauths_net)[V(coauths_net)$n_papers >= 2]) %>%
    ggraph() +
    geom_node_label(aes(label = surname, size = centrality),
                    label.padding = unit(.1, 'lines')) +
    geom_edge_link(aes(width = n_copapers), alpha = .25) +
    scale_edge_width_continuous(range = c(.1, 3), guide = FALSE) +
    scale_size(guide = FALSE) +
    theme_graph()

ggsave('quick_coauths.pdf', coauths_plot, 
       width = 12, height = 9)

library(networkD3)
V(coauths_net_fltd)$name = V(coauths_net_fltd)$surname
thing = igraph_to_networkD3(coauths_net_fltd)
thing$nodes$size = V(coauths_net_fltd)$centrality*1000

forceNetwork(Links = thing$links, Nodes = thing$nodes,
             Source = 'source', Target = 'target',
             NodeID = 'name', Group = 1,
             Nodesize = 'size',
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             zoom = TRUE, 
             fontSize = 16,
             charge = -20) %>%
    saveNetwork('collab.html')

## Output for paste-into-D3 version
# coauths_df %>%
#     ## Replace surnames from individual paper metadata w/ 
#     ## "canonical" surnames from auths_df
#     select(-surname.x, -surname.y) %>%
#     left_join(auths_df, by = c('auid.x' = 'auid')) %>%
#     left_join(auths_df, by = c('auid.y' = 'auid')) %>% 
#     select(surname.x, surname.y, n_copapers) %>%
#     arrange(surname.x, surname.y) %>%
#     as.matrix() %>%
#     toJSON() %>%
#     write_lines('quick_coauths.json')
# 
# auths_df_filtered = auths_df %>%
#     filter(auid %in% auths_to_keep) %>%
#     arrange(surname)
# auths_df_filtered = RColorBrewer::brewer.pal(11, 'RdBu') %>%
#     rep_along(auths_to_keep, .) %>%
#     cbind(auths_df_filtered, color = .)
# 
# auths_df_filtered %>%
#     .$surname %>%
#     jsonlite::toJSON() %>%
#     write_lines('quick_auths.json')
# 
# auths_colors_list = split(as.character(auths_df_filtered$color),
#                           seq(nrow(auths_df_filtered))) %>%
#     setNames(auths_df_filtered$surname)
# auths_colors_list %>%
#     jsonlite::toJSON(auto_unbox = TRUE) %>%
#     write_lines('quick_colors.json')
