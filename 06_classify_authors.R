## JOURNAL TITLE AS TERM


## This first pass works okay with imputed discrete categories
## TODO: 
## - split based on predictors:  <https://topepo.github.io/caret/data-splitting.html#predictors>
## - use KNN to impute values across whole author list
## - then use high-confidence KNN imputed values for training + testing RF
## - consider an ensemble approach

## Duncan suggests multinomial regression
## https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
## https://en.wikipedia.org/wiki/Multinomial_logistic_regression


## Setup ----
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
auid_count = abstracts_df %>%
    unnest(auids) %>%
    count(auid = auids)

## Construct author vectors ----
## ~100 sec w/ 22k abstracts
## ~290 sec w/ 84k abstracts
## ~42 sec w/ year filter
abstracts_2008 = filter(abstracts_df, !is.na(abstract), year < 2008)
tokens_2008 = unnest_tokens(abstracts_2008, word, abstract)
titles_2008 = rename(abstracts_2008, word = journal)
tokens_titles_2008 = bind_rows(tokens_2008, titles_2008)

tictoc::tic()
tidy_pmi = tokens_titles_2008 %>% 
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
    left_join(self_classify) %>%
    mutate(not.is.na.micro = !is.na(micro)) %>%
    select(-n) %>%
    left_join(auid_count)

## Which self-classified authors were excluded by the filters above?  
## Most of these have only a few papers
## Jack Gilbert has many (233 papers total); but only 1 paper in 2007, and that one has NA abstract
# anti_join(self_classify, author_vecs, by = 'auid')

GGally::ggpairs(author_vecs, columns = 1:nv+1,
                aes(color = category, alpha = not.is.na.micro))
ggplot(author_vecs, aes(av_5, av_8, alpha = !is.na(category), 
                        color = category, size = n)) +
    geom_point() +
    theme_bw()


## ML round I:  KNN ----
## Setup ----
avs = str_subset(names(author_vecs), 'av_')
round_1_df = author_vecs %>%
    filter(!is.na(category)) %>%
    select(auid, av_1:av_8, micro:other, category) %>%
    mutate(row_idx = row_number())
set.seed(13579)
round_1_seed = round_1_df %>%
    filter(micro == 100 | building == 100 | other == 100) %>%
    sample_frac(.3)
round_1_add = maxDissim(round_1_seed[,avs], round_1_df[,avs], n = floor(.7*nrow(round_1_df)) - nrow(round_1_seed))
round_1_train = slice(round_1_df, round_1_add) %>%
    bind_rows(round_1_seed)
round_1_test = filter(round_1_df, !(row_idx %in% round_1_train$row_idx))

## Location of testing and training sets in topic space
ggplot(round_1_train, aes(micro, building)) + 
    geom_point(aes(color = 'train'), position = 'jitter') + 
    geom_point(data = round_1_test, aes(color = 'test'), position = 'jitter')
## And author vector space
ggplot(round_1_train, aes(av_1, av_4)) + 
    geom_point(aes(color = 'train'), position = 'jitter') + 
    geom_point(data = round_1_test, aes(color = 'test'), position = 'jitter')


## Model ----
library(quantregForest)

round_1_model = train(x = round_1_train[,avs], y = round_1_train[['micro']], 
                      method = 'qrf', fitBest = FALSE, returnData = TRUE, tuneGrid = data.frame(mtry = 2:4))

## Test set accuracy
extractPrediction(list(round_1_model), round_1_test[,avs], round_1_test[['micro']], what = 'sd') %>%
    ggplot(aes(obs, pred)) + 
    geom_point()







## Let's try imputing discrete categories by getting the 5 nearest neighbors for each self-classified point
cos = function(vec1, vec2) {
    norm1 = sqrt(sum(vec1^2))
    norm2 = sqrt(sum(vec2^2))
    return(vec1 %*% vec2 / (norm1 * norm2))
}
sim = function(tidy_autvec, auid1, auid2) {
    tidy_autvec %>%
        widely(~ cos(.[auid1, ], .[auid2, ]))(auid, dimension, value) %>%
        pull(value)
}




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
    left_join(self_classify) %>%
    select(auid = nbr_auid, category) %>%
    count(auid, category) %>%
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
author_vecs_nna = filter(author_vecs, !is.na(category))
train = author_vecs %>%
    filter(!is.na(category)) %>%
    pull(category) %>%
    createDataPartition() %>%
    unlist() %>%
    author_vecs_nna[., ]
test = filter(author_vecs_nna, !(auid %in% train$auid))

## Discrete categories
cat_rf = randomForest::randomForest(category ~ av_1 + av_2 + av_3 + 
                                        av_4 + av_5 + av_6 + av_7 + av_8, 
                                    data = train)
tibble(prediction = predict(cat_rf, test), 
       actual = test$category, 
       n = test$n) %>%
    ggplot(aes(actual, prediction, size = n, alpha = n)) +
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
