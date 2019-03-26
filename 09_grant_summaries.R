## This script generates some summaries of the individual grants/awards
library(tidyverse)
library(lubridate)

library(cowplot)
theme_set(theme_minimal())

data_dir = '../MoBE-data/'

## Load data ----
papers_df = read_csv(str_c(data_dir, '00_Sloan.csv')) %>% 
    rename_all(tolower)

awards_df = readxl::read_excel(str_c(data_dir, 
                                     '00_MoBE grants with categories.xls'), 
                               sheet = 1, 
                               skip = 1, 
                               .name_repair = 'universal') %>% 
    rename_all(tolower) %>% 
    mutate(start_year = year(start.date))


## Table of organizations that received >= 3 awards ----
count(awards_df, organization) %>% 
    arrange(desc(n)) %>% 
    filter(n >= 3) %>% 
    knitr::kable(format = 'latex', booktabs = TRUE,
                 label = 'mobe_orgs',
                 caption = 'Organizations that received 3 or more awards under the MoBE program.  Awards include research funding as well as funds for meeting organization, data infrastructure development, outreach, and other categories.  n: Number of awards received.') %>% 
    write_file(path = '09_orgs.tex')


## Plots ----
## New awards by year
new_awards = ggplot(awards_df, aes(start_year, fill = category)) +
    geom_bar(width = 1) +
    geom_vline(xintercept = 2017.5, color = 'grey60') +
    xlab('starting year') +
    ylab('new awards') +
    scale_fill_viridis_d(name = 'award\ncategory', 
                         guide = guide_legend(byrow = TRUE)) +
    theme(legend.position = 'bottom')

# ggsave('09_awards_bar.png')

## Active awards by year
active_awards = awards_df %>% 
    rowwise() %>% 
    mutate(end_year = year(end.date), 
           active_years = list(start_year:end_year)) %>% 
    unnest(active_years) %>% 
    ggplot(aes(active_years, fill = category)) +
    geom_bar(width = 1) +
    geom_vline(xintercept = 2017.5, color = 'grey60') +
    xlab('year') +
    ylab('active awards') +
    scale_fill_viridis_d(name = 'award category')

## Papers by year
papers = ggplot(papers_df, aes(`publication year`)) +
    geom_bar(width = 1, aes(fill = as.factor(1)), 
             show.legend = FALSE) +
    geom_vline(xintercept = 2017.5, color = 'grey60') +
    xlim(2004, 2020) +
    xlab('publication year') +
    ylab('publications') +
    scale_fill_viridis_d()

## Combine
legend = get_legend(new_awards)

upper = plot_grid(new_awards + theme(legend.position = 'none'), 
          active_awards + theme(legend.position = 'none'), 
          papers,
          # legend, 
          labels = c('A', 'B', 'C'),
          ncol = 3
          # rel_widths = c(1, 1, 1), 
          # rel_heights = c(1, .5)
          )
plot_grid(upper, legend, 
          ncol = 1, 
          align = 'v', axis = 'l',
          rel_heights = c(1, .25))

ggsave('09_awards.png', height = 2.5, width = 6, scale = 1.75)
