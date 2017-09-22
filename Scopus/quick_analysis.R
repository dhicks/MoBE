library(tidyverse)
library(lubridate)
library(cowplot)
library(stringr)

load('../quick_scopus_09_15.Rdata')

# read_csv('../Eisen_zotero_09_14.csv') %>%
#     filter(DOI %in% filter(scopus_data, is.na(sid))$doi) %>% 
#     write_csv('Scopus-missing.csv')
    
scopus_data = scopus_data %>%
    filter(!is.na(sid)) %>%
    select(-raw) %>%
    mutate(pub_date = ymd(pub_date), 
           pub_year = year(pub_date))


## Publication dates
## --------------------

ggplot(scopus_data, aes(pub_year)) + 
    geom_bar() +
    scale_x_continuous(breaks = seq(2006, 2017, by = 2))

## Citation counts
## --------------------
sum(scopus_data$cite_count, na.rm = TRUE)
median(scopus_data$cite_count, na.rm = TRUE)

## Most-cited articles
scopus_data %>%
    select(title, cite_count, journal, doi, pub_year) %>%
    arrange(desc(cite_count)) %>%
    View()
    # write_csv('quick-most-cited-articles.csv')

ggplot(scopus_data, aes(cite_count+1)) + 
    # stat_ecdf() +
    stat_density() +
    geom_rug() +
    scale_x_log10()

ggplot(scopus_data, aes(pub_date, cite_count+1)) +
    geom_point() +
    stat_smooth(method = 'lm') +
    scale_y_log10()

## Journals with the most citations
scopus_data %>%
    group_by(journal) %>%
    summarize(n_papers = n(),
              total_citations = sum(cite_count),
              rel_citations = total_citations / n_papers) %>%
    arrange(journal) %>%
    View()
    # write_csv('quick-journals.csv')

## Collaboration network
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
    summarize(surname = min(surnames), 
              n_papers = n())

ggplot(auths_df, aes(n_papers)) + 
    stat_ecdf() +
    geom_rug() +
    geom_vline(aes(xintercept = 2))

coauths_df = scopus_data %>%
    filter(!is.na(sid)) %>%
    select(sid, auid, surnames, pub_year) %>%
    unnest() %>%
    full_join(., ., by = c('sid', 'pub_year')) %>%
    filter(auid.x < auid.y) %>%
    group_by(auid.x, auid.y, pub_year) %>%
    summarize(n_copapers = n(), 
              surname.x = first(surnames.x), 
              surname.y = first(surnames.y)) %>%
    ungroup()

## Put everything into the network to calculate centrality correctly
coauths_net = coauths_df %>%
    select(auid.x, auid.y, n_copapers, pub_year) %>%
    graph_from_data_frame(directed = FALSE,
                          vertices = filter(auths_df))

V(coauths_net)$centrality = betweenness(coauths_net, 
                                        directed = FALSE, 
                                        normalized = TRUE,
                                        weights = E(coauths_net)$n_copapers)

## Static plot as used in meeting
coauths_plot = coauths_net %>%
    induced_subgraph(V(coauths_net)[V(coauths_net)$n_papers >= 5]) %>%
    ggraph() +
    geom_node_label(aes(label = surname, size = centrality),
                    label.padding = unit(.1, 'lines')) +
    geom_edge_fan(aes(width = n_copapers), alpha = .25) +
    scale_edge_width_continuous(range = c(.1, 3), guide = FALSE) +
    scale_size(guide = FALSE) +
    theme_graph()

ggsave('quick-coauths.pdf', coauths_plot, 
       width = 12, height = 9)

## D3 interactive plot as used in meeting
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
    saveNetwork('quick-coauths.html')

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


## Breakout grid/animation
coauths_net_fltd = coauths_net %>%
    induced_subgraph(V(coauths_net)[V(coauths_net)$n_papers >= 5])

layout = layout_nicely(coauths_net_fltd)

V(coauths_net_fltd)$x = layout[,1]
V(coauths_net_fltd)$y = layout[,2]

x_min = min(layout[,1])
x_max = max(layout[,1])
y_min = min(layout[,2])
y_max = max(layout[,2])

net_plots = coauths_net_fltd %>%
{E(.)$pub_year} %>%
    unique() %>%
    sort() %>%
    # .[1:4] %>%
    `names<-`(., .) %>%
    map(~ subgraph.edges(coauths_net_fltd, 
                         which(E(coauths_net_fltd)$pub_year <= .x))) %>%
    map2(names(.), 
         ~ {ggraph(.x, 
                   layout = 'manual', 
                   node.positions = tibble(x = V(.x)$x, y = V(.x)$y)) + 
                 # geom_node_label(aes(label = surname, size = centrality),
                 #                 label.padding = unit(.1, 'lines')) +
                 geom_node_point() +
                 geom_edge_link(aes(width = n_copapers, 
                                    color = pub_year != max(pub_year)), 
                                alpha = .5) +
                 scale_edge_width_continuous(range = c(.1, 3), 
                                             guide = FALSE) +
                 scale_edge_color_brewer(palette = 'Set1', 
                                         guide = FALSE) +
                 xlim(x_min, x_max) + ylim(y_min, y_max) + 
                 theme_graph() +
                 ggtitle(.y)})

plot_grid(plotlist = net_plots)

ani.options(interval = .5, ani.dev = 'png')
saveGIF(print(net_plots), movie.name = 'quick-animation.gif')
saveHTML(print(net_plots), 
         htmlfile = 'quick-animation.html')

