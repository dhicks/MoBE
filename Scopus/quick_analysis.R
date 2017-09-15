library(tidyverse)
library(cowplot)

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
## Filter to authors w/ >= 5 publications in the dataset
library(igraph)
library(ggraph)

auths_df = scopus_data %>%
    filter(!is.na(sid)) %>%
    select(auid, surnames) %>%
    unnest() %>%
    group_by(auid) %>%
    summarize(surname = first(surnames), 
              n_papers = n())

auths_to_keep = auths_df %>%
    filter(n_papers >= 5) %>%
    .$auid

coauths_df = scopus_data %>%
    filter(!is.na(sid)) %>%
    select(sid, auid) %>%
    unnest() %>%
    filter(auid %in% auths_to_keep) %>%
    full_join(., ., by = 'sid') %>%
    filter(auid.x < auid.y) %>%
    group_by(auid.x, auid.y) %>%
    summarize(n_copapers = n())

coauths_net = coauths_df %>%
    select(auid.x, auid.y, n_copapers) %>%
    graph_from_data_frame(directed = FALSE, 
                          vertices = filter(auths_df, 
                                            auid %in% auths_to_keep))

ggraph(coauths_net) + 
    geom_edge_link(aes(width = n_copapers)) +
    scale_edge_width_continuous(range = c(.25, 3)) +
    geom_node_label(aes(label = surname, size = n_papers))

