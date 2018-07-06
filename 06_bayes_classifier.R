library(tidyverse)
library(lubridate)
library(tidytext)
library(widyr)
library(caret)

## Author self-classification
threshold = 75
self_classify = read_csv('../Eisen-data/00_authorlist.csv') %>%
    rename(micro = `% Micro`, 
           building = `% Building`, 
           other = `% Other`, 
           auid = s) %>%
    filter(!is.na(micro)) %>%
    mutate(auid = as.character(auid),
           category = case_when(micro >= threshold    ~ "micro", 
                                building >= threshold ~ "building", 
                                other >= threshold    ~ "other", 
                                TRUE                  ~ NA_character_), 
           category = as_factor(category))

# ggplot(self_classify, aes(micro, building, size = n, alpha = n)) + 
#     geom_point(position = 'jitter')

## Paper abstracts
abstracts_df = read_rds('../Eisen-data/05_abstracts.Rds')  %>%
    mutate(date = ymd(date), 
           year = year(date))

abstracts_2008 = abstracts_df %>%
    filter(!is.na(abstract), year < 2008) %>%
    select(scopus_id, journal, auids, abstract) %>%
    unnest(auids) %>%
    rename(auid = auids) %>%
    inner_join(self_classify) %>%
    filter(!is.na(category))

tokens_2008 = unnest_tokens(abstracts_2008, word, abstract)
titles_2008 = rename(abstracts_2008, word = journal)
tokens_titles_2008 = bind_rows(tokens_2008, titles_2008) %>%
    select(scopus_id, auid, category, word)

set.seed(42)
train_auids = self_classify %>%
    filter(!is.na(category)) %>%
    pull(category) %>%
    createDataPartition %>%
    .$Resample1 %>%
    {self_classify[., ]} %>%
    pull(auid)
test_auids = self_classify %>%
    filter(!(auid %in% train_auids), !is.na(category)) %>%
    pull(auid)
    

train = tokens_titles_2008 %>%
    filter(auid %in% train_auids)
test = tokens_titles_2008 %>%
    filter(auid %in% test_auids)


## High information words ----
word_information = train %>%
    count(category, word) %>%
    rename(word_category = n) %>%
    group_by(word) %>%
    mutate(word_tot = sum(word_category),
           prob_cat_word = word_category / word_tot,
           H = log2(3) - -sum(prob_cat_word * log2(prob_cat_word)), 
           lgnH = log2(word_tot)*H) %>%
    filter(word_category == max(word_category)) %>%
    ungroup() %>%
    arrange(desc(lgnH)) #%>% 
    # mutate(row_idx = row_number())

## NB Very few high information words are associated w/ other

vocab = word_information %>%
    # group_by(category) %>%
    slice(1:600) %>%
    pull(word)



prob_cat = train %>%
    select(scopus_id, category) %>%
    filter(!duplicated(.)) %>%
    count(category) %>%
    mutate(log_prob_cat = log10(n+1) - log10(sum(n+1))) %>%
    select(-n)

prob_word_cat = train %>%
    count(category, word) %>%
    rename(word_category = n) %>%
    spread(key = category, value = word_category, fill = 0) %>%
    gather(key = category, value = word_category, micro:building) %>%
    group_by(category) %>%
    mutate(log_prob_word_cat = log10(word_category+1) - log10(sum(word_category+1))) %>%
    ungroup() %>%
    select(-word_category)

# train %>%
#     count(category, word) %>%
#     rename(word_category = n) %>%
#     group_by(word) %>%
#     mutate(prob_cat_word = word_category / sum(word_category), 
#            H = -sum(prob_cat_word * log2(prob_cat_word))) %>%
#     arrange(H, word) %>%
#     filter(word_category > 5) %>% View

prob_word = train %>%
    count(word) %>%
    mutate(log_prob_word = log10(n+1) - log10(sum(n+1))) %>%
    select(-n)

## Confusion matrix for document classification
test %>%
    select(-category) %>%
    inner_join(prob_word_cat) %>%
    inner_join(prob_word) %>%
    mutate(ll_ev = log_prob_word_cat - log_prob_word) %>% 
    group_by(scopus_id, category) %>%
    summarize(ll_ev = sum(ll_ev)) %>% 
    ungroup() %>%
    left_join(prob_cat) %>%
    mutate(log_post = ll_ev + log_prob_cat) %>%
    # mutate(log_post = ll_ev) %>%
    select(scopus_id, category.pred = category, log_post) %>%
    group_by(scopus_id) %>%
    filter(log_post == max(log_post)) %>%
    ungroup() %>%
    left_join(abstracts_2008) %>%
    {table(.$category, .$category.pred)}

## Confusion matrix for author classification
test %>%
    select(-category) %>%
    inner_join(prob_word_cat) %>%
    inner_join(prob_word) %>%
    mutate(ll_ev = log_prob_word_cat - log_prob_word) %>%
    group_by(auid, category) %>%
    summarize(ll_ev = sum(ll_ev)) %>%
    ungroup() %>%
    left_join(prob_cat) %>%
    mutate(log_post = ll_ev) %>%
    select(auid, category.pred = category, log_post) %>%
    left_join(self_classify) %>% 
    group_by(auid) %>%
    filter(log_post == max(log_post)) %>%
    ungroup() %>%
    {table(.$category, .$category.pred)}


