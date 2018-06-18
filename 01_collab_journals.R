## This script uses CrossRef to build a list of papers published in the same journals as the Sloan collaboration. 

library(tidyverse)
## devtools::install_github('ropensci/rcrossref')
library(rcrossref)
library(lubridate)

## Load list of papers in the collaboration --------------------
collab_df = read_csv('../Eisen-data/00_Sloan.csv')

## Get metadata --------------------
## At one level, thi isn't necessary, because 00_Sloan.csv contains the metadata
## However, running through crossref gets us more variant ISSNs
## And shortens the final journal list by ~20 journals
cr_df = collab_df$DOI %>%
    cr_works(.progress = 'time')
cr_df = cr_df$data
cr_df = cr_df %>%
    mutate(pub_date = parse_date_time(issued,
                                      orders = c('ymd', 'ym', 'y')),
           issn = str_split(issn, ','))

## Construct canonical ISSNs --------------------
canonical_issns = cr_df %>%
    pull(issn) %>%
    keep(~ length(unique(.x)) > 1) %>%
    transpose() %>%
    `names<-`(c('issn1', 'issn2')) %>%
    as_tibble() %>%
    mutate(issn1 = simplify(issn1), 
           issn2 = simplify(issn2)) %>% 
    rowwise() %>%
    mutate(issn_canonical = min(issn1, issn2), 
           issn_list = list(sort(c(issn1, issn2)))) %>%
    gather(key, issn, issn1:issn2) %>%
    select(-key) %>%
    filter(!duplicated(.))

journals_df = cr_df %>%
    ## Replace ISSNs with canonical ISSNs
    mutate(issn = str_extract(issn, '[0-9X\\-]+')) %>% 
    left_join(canonical_issns) %>% 
    mutate(issn_canonical = ifelse(!is.na(issn_canonical), 
                                   issn_canonical, 
                                   issn)) %>% 
    select(-issn, -issn_list) %>%
    rename(issn = issn_canonical) %>%
    ## Count papers
    count(issn, journal = container.title) %>%
    group_by(issn) %>%
    summarize(journal = first(journal[which(n == max(n))]), 
              collab_papers = sum(n)) %>% 
    ## Join w/ canonical_issns again to get list of ISSN variants
    left_join(canonical_issns) %>%
    select(-issn_canonical) %>%
    filter(!duplicated(.)) %>% 
    arrange(desc(collab_papers))

# ggplot(journals_df, aes(collab_papers)) + stat_ecdf()

first_year = cr_df %>%
    pull(pub_date) %>%
    min() %>%
    year()
last_year = cr_df %>%
    pull(pub_date) %>%
    max() %>%
    year()

## Retrieve all papers from these journals during the same time period --------------------
## NB Very very slow! 
all_papers_cr = journals_df %>%
    # filter(issn %in% c('1758-0463', '2196-3010', '2058-5276')) %>%
    filter(issn != '1932-6203') %>% # Drop PLoS ONE
    pull(issn) %>%
    safely(cr_journals)(works = TRUE, cursor = '*', 
                limit = 1000, cursor_max = 2000000, 
                filter = c(from_pub_date = str_c(first_year, '-01-01'),
                           until_pub_date = str_c(last_year, '-12-31')), 
                .progress = 'time')

# warns = all_papers_cr$warnings
errors = all_papers_cr$error

all_papers_df = all_papers_cr$result$data %>%
    select(-link, -funder) %>%
    ## Remove rows w/ NULL author values
    filter(!simplify(map(.$author, is.null))) %>%
    select(one_of(c('container.title', 'created', 
                    'doi', 'issn', 
                    'title', 'author')))

## Write results --------------------
write_lines(errors, '../../Eisen-data/01_errors.txt')
save(all_papers_df, file = '../../Eisen-data/01_all_papers.Rdata')
