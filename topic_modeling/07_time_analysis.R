library(tidyverse)
library(cowplot)
library(lubridate)

load('../../Eisen-data/02_abstracts.Rdata')
dataf = dataf %>%
    select(-raw)
load('../../Eisen-data/04_lda_dfs.Rdata')

## To load LDA visualization
# lda_json = read_file('../../Eisen-data/03_lda.json')
LDAvis::serVis(lda_json)

## Combine LDA output with metadata
dataf = left_join(documents_lda, abstracts_df) %>%
    mutate(date = ymd(date), 
           year = year(date))

## Information gain of each document
## Base-2 log, with log 0 = 0
lg0 = function (x) {
    if (x > 0) {
        return(log2(x))
    } else {
        return(0)
    }
}
lg0 = Vectorize(lg0)

k = 5 # num. topics
max_H = -k * 1/k * lg0(1/k)

dataf = dataf %>%
    select(scopus_id, starts_with('topic')) %>%
    gather(topic, gamma, -scopus_id) %>%
    group_by(scopus_id) %>%
    summarize(H = -sum(gamma * lg0(gamma)), 
              delta_H = max_H - H) %>%
    left_join(dataf)

ggplot(dataf, aes(year, delta_H)) + 
    geom_point(alpha = .75, position = 'jitter') + 
    geom_smooth() +
    geom_hline(yintercept = max_H + (1:5) * 1/(1:5) * lg0(1/(1:5)))

#' This plot shows information gain for the gamma distribution of each 
#' document.  Information gain measures the "pointedness" of the 
#' distribution:  greater information gain means that more of the 
#' distribution is concentrated in a smaller number of topics.  
#' The horizontal lines correspond to some reference distributions: 
#' documents that are entirely associated with 1 topic (top line) 
#' to documents evenly distributed across all 5 topics (bottom line 
#' at delta_H = 0).  
#' 
#' The plot indicates that most documents have high information gain, 
#' and are primarily associated with 1-2 topics.  This means that the 
#' model separates the documents into distinct, non-overlapping clusters.  


## Prevalence of topics over time
## Topics 2 and 5 are prominent in the first half of the data
## Topic 4 emerges a bit in 2009-2015
## Topic 3 emerges around 2011 and continues to grow
gamma_year_plot = dataf %>%
    select(topic_1:topic_5, year) %>%
    gather(topic, gamma, -year) %>%
    ggplot(aes(year, gamma, color = topic, shape = topic)) + 
    scale_color_brewer(palette = 'Set1')
plot_grid(gamma_year_plot + geom_smooth(), 
          gamma_year_plot + stat_summary(geom = 'line', fun.y = 'median'))

# geom_point(alpha = .25, position = 'jitter') +
    geom_smooth() +
    # stat_summary(geom = 'line', fun.y = 'median') +
    scale_color_brewer(palette = 'Set1')

## Pairwise correlation of topics over time
## Nothing stands out as super interesting to me here
dataf %>%
    select(year, scopus_id, starts_with('topic')) %>%
    gather(topic, gamma, -year, -scopus_id) %>%
    inner_join(., ., by = c('year', 'scopus_id')) %>%
    # filter(topic.x < topic.y) %>%
    filter(topic.x == 'topic_3', topic.y != 'topic_3') %>%
    group_by(year, topic.x, topic.y) %>%
    summarize(cor = cor(gamma.x, gamma.y)) %>%
    ggplot(aes(year, cor, color = topic.x, shape = topic.y, linetype = topic.y)) + 
    geom_point() +
    # geom_smooth(se = FALSE)
    geom_line()

## Relationship between collaboration and topics
## Building is associated with topics 1, 2, and 5
## Microbial is associated with topics 3 and 4
## The collaboration is associated with topic 2
## NB Only 15 papers count as collaboration with these data
dataf = dataf %>% 
    mutate(microbial = {map(author_groups, ~ ('microbial' %in% .x)) %>% unlist()}, 
           building = {map(author_groups, ~ ('building' %in% .x)) %>% unlist()}, 
           collaboration = microbial & building)

# dataf %>%
#     select(starts_with('topic'), collaboration) %>%
#     gather(topic, gamma, -collaboration) %>%
#     ggplot(aes(collaboration, gamma)) + 
#     # geom_boxplot() + 
#     stat_summary(color = 'red') +
#     geom_point(alpha = .25, position = 'jitter') + 
#     facet_wrap(~ topic)

## Topics over year, faceted by author
dataf %>%
    unnest(author_ids) %>%
    rename(author_id = author_ids) %>%
    select(topic_1:topic_5, year, author_id) %>%
    gather(topic, gamma, -year, -author_id) %>%
    ggplot(aes(year, gamma, color = topic, shape = topic)) + 
    # geom_point() + 
    geom_smooth() +
    # stat_summary(geom = 'line', fun.y = 'median') +
    geom_vline(xintercept = 2008) +
    facet_wrap(~ author_id) +
    scale_color_brewer(palette = 'Set1')



