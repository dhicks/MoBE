#' # Eisen lab project update #
#' ### Prepared by DJH ###
#' 
#' ## Summary ##
#' The topic model does a good job of separating the two high-level groups of researchers in the dataset ("microbial" and "building") and clustering the individual documents (papers).  However, there is no obvious association between any of the topics and the collaboration.  I suggest we meet next week to discuss ways to modify the model and analysis.  
#' 
#' ## Data ##
#' The data used to construct the topic model comprise 2,326 papers by 24 authors involved in the collaboration.  Up to 200 papers published since 1998 are included for each author.  To identify papers funded as part of the collaboration, these papers were linked to papers in the Zotero catalogue.  
#' 
#' A key data limitation is *author duplication*.  Author duplication occurs when Scopus splits the bibliography of one actual individual author between two (or more) author records; i.e., a single individual appears in Scopus as two different authors.  When author duplication occurs and is not identified, some of the author's papers will be missing from the data, or some coauthor relationships will be misidentified.  
#' 
#' Another key limitation is that some papers are missing from Scopus.  In the first outputs that we shared with Jonathan and David, David pointed out that we were missing papers in one or two key journals.  I found that these journals are quite new, and therefore not indexed by Scopus.  Hence they are not automatically retrieved when we construct author histories.  
#' 

#+ message = FALSE, warning = FALSE
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

#'
#' ## Model ##
#' The LDA model is constructed in the script `03_topic_modeling.R` using the call `LDA(dtm, 5)`. 
#'  

## To load LDA visualization
# lda_json = read_file('../../Eisen-data/03_lda.json')
# LDAvis::serVis(lda_json)

## Combine LDA output with metadata
dataf = left_join(documents_lda, abstracts_df) %>%
    mutate(date = ymd(date), 
           year = year(date))

#' ## Document-wise entropy/information gain ##
## Base-2 log, with log 0 = 0 for convenience
lg0 = function (x) {
    if (x > 0) {
        return(log2(x))
    } else {
        return(0)
    }
}
lg0 = Vectorize(lg0)

k = sum(grepl('topic', names(documents_lda)))  # num. topics
max_H = -k * 1/k * lg0(1/k)  # entropy of a uniform distribution

dataf = dataf %>%
    select(scopus_id, starts_with('topic')) %>%
    gather(topic, gamma, -scopus_id) %>%
    group_by(scopus_id) %>%
    summarize(H = -sum(gamma * lg0(gamma)), 
              delta_H = max_H - H) %>%
    left_join(dataf)

ggplot(dataf, aes(year, delta_H, color = in_collab)) + 
    geom_point(alpha = .25, position = 'jitter') + 
    geom_smooth(method = 'loess') +
    geom_hline(yintercept = max_H + (1:k) * 1/(1:k) * lg0(1/(1:k)), 
               linetype = 'dashed') +
    scale_color_brewer(palette = 'Set1')

#' 
#' This plot shows information gain $\Delta H$ for the distribution
#' $\gamma = Pr(\tau | d)$ of topics $\tau$ for each document $d$. 
#' Information gain measures the "pointedness" of the distribution: 
#' greater information gain means that more of the distribution is
#' concentrated in a smaller number of topics. The horizontal lines
#' correspond to some reference distributions: documents that are
#' entirely associated with 1 topic (top line) to documents evenly
#' distributed across all 5 topics (bottom line at delta_H = 0).
#' 
#' The plot indicates that most documents have high information gain, 
#' and are primarily associated with 1-2 topics.  This means that the 
#' model separates the documents into distinct, non-overlapping 
#' clusters. This is the case for documents both in and out of the
#' collaboration; though the collaboration documents are somewhat
#' closer to 2 topics.
#' 


#' ## Prevalence of topics over time ##
#' 
#+ fig.width = 12
gamma_year_plot = dataf %>%
    select(starts_with('topic'), year, in_collab) %>%
    gather(topic, gamma, -year, -in_collab) %>%
    ggplot(aes(year, gamma, color = topic, linetype = topic)) + 
    # scale_color_brewer(palette = 'Set1')
    scale_color_manual(values = rep_len(RColorBrewer::brewer.pal(5, 'Set1'), length.out = k)) +
    scale_linetype_manual(values = rep_len(1:4, length.out = k))
plot_grid(gamma_year_plot + geom_smooth(), 
          gamma_year_plot + stat_summary(geom = 'line', fun.y = 'median'))

#' This plot shows the prevalence of all 5 topics over time, using a GAM smoother (left) and median (right) on the $\gamma$ distributions for documents published in a given year.  We note that
#' 
#' - Topics 2 and 5 are prominent in the first half of the data
#' - Topic 4 emerges a bit in 2009-2015
#' - Topic 3 emerges around 2011 and continues to grow
#' 

## Pairwise correlation of topics over time
dataf %>%
    select(year, scopus_id, starts_with('topic')) %>%
    gather(topic, gamma, -year, -scopus_id) %>%
    inner_join(., ., by = c('year', 'scopus_id')) %>%
    filter(topic.x < topic.y) %>%
    # filter(topic.x == 'topic_3', topic.y != 'topic_3') %>%
    group_by(year, topic.x, topic.y) %>%
    summarize(cor = cor(gamma.x, gamma.y)) %>%
    ggplot(aes(year, cor, color = topic.x, shape = topic.y, linetype = topic.y)) + 
    geom_point() +
    # geom_smooth(se = FALSE)
    geom_line()

#' This plot shows pairwise correlations of topics over time.  I don't see any striking patterns here.  

#' 
#' ## Relationship between collaboration and topics ##
#' 

#+ warning = FALSE, fig.width = 12, fig.height = 7
## Topics over year, faceted by author
dataf %>%
    unnest(author_ids) %>%
    rename(author_id = author_ids) %>%
    select(starts_with('topic'), year, author_id) %>%
    gather(topic, gamma, -year, -author_id) %>%
    ggplot(aes(year, gamma, color = topic, shape = topic)) + 
    # geom_point() + 
    geom_smooth() +
    # stat_summary(geom = 'line', fun.y = 'median') +
    geom_vline(xintercept = 2010) +
    facet_wrap(~ author_id) +
    # scale_color_brewer(palette = 'Set1')
    scale_color_manual(values = rep_len(RColorBrewer::brewer.pal(5, 'Set1'), length.out = k))

#' Each of the 24 authors was given an internal ID indicating whether they were "building" or "microbial."  By plotting topics over time for each author, we can get a sense of how topics are related to the two major research communities.  In this plot, the vertical line corresponds to the first publications by the collaboration in 2010.  
#' 
#' Focusing on building researchers first, topics 2 (blue), 5 (orange), and to a limited extent 1 (red) appear to be prominent in this community.  Author b01 shows an increase in topic 5 after the collaboration; but for all other authors and topics the trend is either flat or downward.  
#' 
#' For microbial researchers, topics 3 (green) and 4 (purple) are generally more prominent.  However, topic 5 is prominent in the work of m01.  A number of microbial researchers have no or few publications prior to the collaboration.  For those researchers who do have substantial publication records prior to 2010, there are few positive trends.  Perhaps, among researchers m06, m10, and m12, there is an increase in topic 3 (green).  In all other cases, the trend is either flat or downward.  
#' 
#' All together, the model seems to separate the two research communities, with topics 1, 2, and 5 for building and topics 3 and 4 for microbial.  I do not see evidence of a cross-cutting or interdisciplinary topic here, especially after the collaboration begins.  
#' 

#+ fig.width = 10, fig.height = 10
plot_grid(gamma_year_plot + geom_smooth(method = 'loess') + 
              facet_grid(~ in_collab) + coord_cartesian(xlim = c(2007, 2018), ylim = c(0, .5)), 
          gamma_year_plot + stat_summary(geom = 'line', fun.y = 'median') +
              facet_grid(~ in_collab) + coord_cartesian(xlim = c(2007, 2018), ylim = c(0, .5)), 
          ggplot(dataf, aes(year)) + 
              geom_bar() + 
              facet_wrap(~ in_collab, scales = 'free_y') +
              coord_cartesian(xlim = c(2007, 2018)) +
              theme(plot.margin = margin(r = 80)),
          nrow = 3)

#' This plot shows (top two panels) topic prevalence over time and (bottom panel) document counts, both within (TRUE, right) and outside (FALSE, left) the collaboration.  Within the collaboration, prevalence is noisy for the first few years, due to the small number of documents.  After 2013, topics 2, 3, and 5 are prominent in the collaboration, though 3 is increasingly prominent outside the collaboration as well.  
#' 
dataf %>%
    select(topic_2, topic_4, topic_12, in_collab) %>%
    mutate(combined = topic_2 + topic_4 + topic_12) %>%
    gather(topic, gamma, -in_collab) %>%
    ggplot(aes(in_collab, gamma)) +
    # geom_boxplot() +
    geom_point(alpha = .1, position = 'jitter') +
    stat_summary(color = 'red', fun.args = list(mult = 2)) +
    facet_wrap(~ topic)
#' 
#' This plot collapses time, plotting the $\gamma$ distribution for each document, broken down by outside (left columns, FALSE) or within (right columns, TRUE) the collaboration.  Red points give means; red bars indicate 2 standard errors (~ 95% CIs).  These means indicate that topics 2 and 5 are somewhat more associated with the collaboration, and topics 1 and 4 are somewhat less associated.  
#' 
#' All together, there is not a clear signal here of a distinctive topic emerging in the collaboration that cuts across both research communities.  
#' 


#+ 
sessionInfo()
