## This first pass works okay with imputed discrete categories
## TODO: 
## - split based on predictors:  <https://topepo.github.io/caret/data-splitting.html#predictors>
## - use KNN to impute values across whole author list
## - then use high-confidence KNN imputed values for training + testing RF
## - consider an ensemble approach

## Setup ----
library(tidyverse)
library(lubridate)
library(tidytext)
library(widyr)

## Author self-classification
self_classify = read_csv('../Eisen-data/00_authorlist.csv') %>%
    rename(micro = `% Micro`, 
           building = `% Building`, 
           other = `% Other`, 
           auid = s) %>%
    filter(!is.na(micro)) %>%
    mutate(category = ifelse(micro > building & micro > other, 'micro', 
                      ifelse(building > micro & building > other, 'building', 'other')))

# ggplot(self_classify, aes(micro, building, size = n, alpha = n)) + 
#     geom_point(position = 'jitter')

## Paper abstracts
abstracts_df = read_rds('../Eisen-data/05_abstracts.Rds')  %>%
    mutate(date = ymd(date), 
           year = year(date))
auid_count = abstracts_df %>%
    unnest(auids) %>%
    count(auid = auids) %>%
    mutate(auid = as.numeric(auid))

## Construct author vectors ----
## ~100 sec w/ 22k abstracts
## ~290 sec w/ 84k abstracts
## ~42 sec w/ year filter
tictoc::tic()
tidy_pmi = abstracts_df %>%
    filter(!is.na(abstract), year < 2008) %>%
    ## Unnest tokens
    unnest_tokens(word, abstract) %>% 
    add_count(word) %>% 
    select(-n) %>%
    ## Unnest authors
    unnest(auids) %>%
    rename(auid = auids) %>%
    filter(!duplicated(.)) %>%
    pairwise_pmi(auid, word)
tictoc::toc()

nv = 8  # number of vectors to keep in the decomposition
tidy_av = widely_svd(tidy_pmi, item1, item2, pmi, 
                     nv = nv, maxit = 1000) %>%
    transmute(auid = item1, 
              dimension = str_c('av_', dimension), 
              value = value)

## Author vectors in wide format
author_vecs = tidy_av %>%
    spread(key = dimension, value = value) %>%
    mutate(auid = as.numeric(auid)) %>%
    left_join(self_classify) %>%
    mutate(not.is.na.micro = !is.na(micro)) %>%
    select(-n) %>%
    left_join(auid_count)

GGally::ggpairs(author_vecs, columns = 1:nv+1,
                aes(color = category, alpha = not.is.na.micro))
ggplot(author_vecs, aes(av_1, av_4, alpha = !is.na(micro), 
                        color = category, size = n)) +
    geom_point() +
    theme_bw()

## Let's try imputing discrete categories by getting the 5 nearest neighbors for each self-classified point
nearest_auid = function(tidy_autvec, target_auid, n = 5) {
    tidy_autvec %>%
        widely(~ . %*% (.[target_auid, ]), 
               sort = TRUE)(auid, dimension, value) %>%
        select(auid = item1, -item2, distance) %>%
        head(n)
}

imputed_categories = author_vecs %>%
    filter(!is.na(category)) %>%
    pull(auid) %>%
    as.character() %>%
    `names<-`(., .) %>% 
    map(~ nearest_auid(tidy_av, .)) %>%
    map(rename, nbr_auid = auid) %>%
    bind_rows(.id = 'auid') %>%
    mutate(auid = as.numeric(auid)) %>%
    left_join(self_classify) %>%
    select(auid = nbr_auid, category) %>%
    count(auid, category) %>%
    mutate(auid = as.numeric(auid)) %>%
    group_by(auid) %>%
    summarize(category.imp = first(category[n == max(n)]))
    # count(auid) %>% pull(n) %>% table()

author_vecs.imp = author_vecs %>%
    left_join(imputed_categories) %>%
    mutate(category = as.factor(ifelse(!is.na(category), category, category.imp)))

GGally::ggpairs(author_vecs.imp, columns = 1:nv+1,
                aes(color = category, alpha = not.is.na.micro))
ggplot(author_vecs.imp, aes(av_1, av_4, alpha = !is.na(category), 
                        color = category, size = n)) +
    geom_point() +
    theme_bw()

## Classifiers ----
## Train and test sets
set.seed(423)
train_auids = author_vecs.imp %>%
    filter(!is.na(category)) %>%
    sample_frac(size = .5) %>%
    pull(auid)
train =  filter(author_vecs.imp, auid %in% train_auids)
test = filter(author_vecs.imp, !is.na(category), 
              !(auid %in% train_auids))

## Discrete categories
cat_rf = randomForest::randomForest(category ~ av_1 + av_2 + av_3 + 
                                        av_4 + av_5 + av_6 + av_7 + av_8, 
                                    data = train)
tibble(prediction = predict(cat_rf, test), 
       actual = test$category, 
       n = test$n, 
       imputed = is.na(test$micro)) %>%
    ggplot(aes(actual, prediction, size = n, alpha = n, color = imputed)) +
    geom_point(position = position_jitter(height = .2, width = .2)) +
    theme_bw()

## Microbial
micro_rf = randomForest::randomForest(micro ~ av_1 + av_2 + av_3 + 
                                          av_4 + av_5 + av_6 + av_7 + av_8, 
                                      data = train)
micro_rf

tibble(prediction = predict(micro_rf, test), 
       actual = test$micro, 
       n = test$n) %>%
    ggplot(aes(actual, prediction, size = n, alpha = n)) +
    geom_point(position = 'jitter')

## Building
build_rf = randomForest::randomForest(building ~ av_1 + av_2 + av_3 + 
                                          av_4 + av_5 + av_6 + av_7 + av_8, 
                                      data = train)
build_rf

tibble(prediction = predict(build_rf, test), 
       actual = test$building, 
       n = test$n) %>%
    ggplot(aes(actual, prediction, size = n, alpha = n)) +
    geom_point(position = 'jitter')

## Other
other_rf = randomForest::randomForest(other ~ av_1 + av_2 + av_3 + 
                                          av_4 + av_5 + av_6 + av_7 + av_8, 
                                      data = train)
other_rf

tibble(prediction = predict(other_rf, test), 
       actual = test$other, 
       n = test$n) %>%
    ggplot(aes(actual, prediction, size = n, alpha = n)) +
    geom_point(position = 'jitter')
