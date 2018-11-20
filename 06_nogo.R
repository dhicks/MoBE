#' This script examines the relationship between self-classified microbial-building-other values and potential predictors that might be used by a ML algorithm to predict those values.  
#' 
#' Note that this script depends on a datafile that is **not** included in the public repository.  

stop('This script is included for documentary purposes and is not intended to be run in a reproduction of the original study.')

## Setup ----
library(tidyverse)
library(lubridate)
library(ggrepel)
library(tidytext)

## Author self-classification
threshold = 75
self_classify = read_csv('../MoBE-data/00_authorlist.csv') %>%
    rename(micro = `% Micro`, 
           building = `% Building`, 
           other = `% Other`, 
           auid = s) %>%
    filter(!is.na(micro)) %>%
    mutate(auid = as.character(auid),
           category = case_when(micro >= threshold    ~ "micro", 
                                building >= threshold ~ "building", 
                                other >= threshold    ~ "other", 
                                TRUE                  ~ NA_character_))

## Paper abstracts + self-classification
abstracts_df = read_rds('../MoBE-data/05_abstracts.Rds')  %>%
    select(-keywords, -references) %>%
    mutate(date = ymd(date), 
           year = year(date)) %>%
    filter(year < 2008) %>%
    unnest(authors, .drop = FALSE) %>%
    left_join(self_classify)

## Num. papers for self-classified authors
abstracts_df %>%
    filter(!is.na(micro)) %>%
    select(-n) %>%
    count(category, auid, surnames, given_names, micro, building, other) %>%
    arrange(category, surnames) %>%
    knitr::kable()

## Distribution of self-classified authors in micro-building-other space
abstracts_df %>%
    filter(!is.na(micro)) %>%
    select(-n) %>%
    count(auid, surnames, given_names, micro, building, other, category) %>%
    ggplot(aes(micro, building, color = category, size = n)) + 
    geom_point(position = 'jitter', alpha = .5) +
    geom_text_repel(aes(label = surnames)) +
    theme_bw()


## AJSC subject areas ----
#' # ASJC subject areas #
#' The All Science Journal Classification [ASJC] subject areas are defined by Scopus.  Journals are manually tagged with one or more subject area codes, and papers inherit the codes from the journals they're published in.  There are ~30 top-level codes, which do a decent job of discriminating different areas of natural science and engineering.  The top-level codes take the form of four letters, most of which have fairly clear interpretations in English (BIOC is biology, ENGI is engineering, and so on).  
#' 
#' To use ASJC subject areas to classify authors, we first count the number of times each author has received each code, for all authors in the dataset (both with and without self-classification responses).  (Since papers can have multiple codes, the total here isn't the same as the number of publications.)  We then extract the first 6 principal components from the author-code matrix, and plot these against the discrete micro-building-other self-classifications.  

## Author-code matrix
acm = abstracts_df %>%
    unnest(subjects) %>%
    count(auid, subject_area) %>%
    spread(key = subject_area, value = nn, fill = 0) %>%
    as.data.frame() %>%
    column_to_rownames(var = 'auid')

subject_area_pc = prcomp(acm, center = TRUE, scale. = TRUE, rank. = 6)

#+ plot_subject_area, warning = FALSE
plot(subject_area_pc)
biplot(subject_area_pc, choices = 1:2)
biplot(subject_area_pc, choices = 3:4)
biplot(subject_area_pc, choices = 5:6)

#' The biplots suggest some separation of various disciplines, but not biology (BIOC, AGRI) from engineering (ENGI).  Next, we plot pairs for these 6 principal components as well as the micro-building-other self-classification values.  

#+ plot_subject_area_2, warning = FALSE
subject_area_pc$x %>%
    as.data.frame() %>%
    rownames_to_column(var = 'auid') %>%
    as_tibble() %>%
    left_join(self_classify) %>%
    mutate(has_cat = !is.na(category)) %>%
    GGally::ggpairs(mapping = aes(color = category, alpha = has_cat), 
                    columns = c(1:6+1, ## PCs
                                11:13)) + ## MBO values
    theme_bw()

#' Examining the plots in the lower-left shows that none of the first 6 PCs separates any of the micro-building-other categories or values.  Indeed, the self-classification respondents all fall in a narrow range of scores for each PC.  


## All words ----
#' # All words #
#' The next check looks at all words in the abstracts, without trying to select only high-information words to use as features.  Again, we first construct a matrix of author-term counts, then extract the first 6 principal components.  

#+ extract_pc, cache = TRUE
## Author-term matrix
## ~ 15 sec w/out cache
tictoc::tic()
atm = abstracts_df %>%
    unnest_tokens(word, abstract) %>%
    count(auid, word) %>%
    spread(key = word, value = nn, fill = 0) %>%
    as.data.frame() %>%
    column_to_rownames(var = 'auid')
tictoc::toc()

## ~ 2 min w/out cache
tictoc::tic()
all_words_pc = prcomp(atm, center = TRUE, scale. = TRUE, rank. = 6)
tictoc::toc()

#+ plot_all_words, warning = FALSE
plot(all_words_pc)
## Because we only used `biplot()` to plot the subject area axes above, and we don't care about that at the level of individual words, we'll skip `biplot()` here.  
# biplot(all_words_pc, choices = 1:2)
# biplot(all_words_pc, choices = 3:4)
# biplot(all_words_pc, choices = 5:6)

all_words_pc$x %>%
    as.data.frame() %>%
    rownames_to_column(var = 'auid') %>%
    as_tibble() %>%
    left_join(self_classify) %>%
    mutate(has_cat = !is.na(category)) %>%
    GGally::ggpairs(mapping = aes(color = category, alpha = has_cat), 
                    columns = c(1:6+1, ## PCs
                                11:13)) + ## MBO values
    theme_bw()

#' It's a little hard to tell because of the way the extreme values compress the meaningful range of values.  But again this doesn't look promising.  We'll zoom in on the meaningful range of values for PC5 and PC6 to confirm.  

all_words_pc$x %>%
    as.data.frame() %>%
    rownames_to_column(var = 'auid') %>%
    as_tibble() %>%
    left_join(self_classify) %>%
    ggplot(aes(PC5, PC6, color = category, alpha = !is.na(category))) + 
    geom_point(size = 3) +
    ylim(-20, 10) +
    xlim(-25, 40) +
    theme_bw()


## High-information words ----
#' # High-information words #
#' Finally we'll check ~1,000 high-information words, defined as words that have both a high information gain for the discrete self-classification categories and occur frequently across the entire corpus. 

## Calculate word information
high_inform_words = abstracts_df %>%
    filter(!is.na(category)) %>%
    unnest_tokens(word, abstract) %>%
    ## Word frequency in each category
    count(category, word) %>%
    rename(word_category = nn) %>%
    group_by(word) %>%
    mutate(word_tot = sum(word_category), ## Word freq. across all cats
           prob_cat_word = word_category / word_tot, ## p(category | word)
           H = log2(3) - -sum(prob_cat_word * log2(prob_cat_word)), ## information gain wrt p(category) = 1/3
           lgnH = log2(word_tot)*H) %>% 
    filter(word_category == max(word_category)) %>%
    ungroup() %>%
    arrange(desc(lgnH))

head(high_inform_words)

## Select ~1k highest-information words
vocabulary = high_inform_words %>%
    top_n(1000, lgnH) %>%
    pull(word)

## Use the author-term matrix constructed above
atm_hi = atm[, vocabulary]

hi_words_pc = prcomp(atm_hi, center = TRUE, scale. = TRUE, rank. = 6)

#+ plot_hi_words, warning = FALSE
plot(hi_words_pc)

hi_words_pc$x %>%
    as.data.frame() %>%
    rownames_to_column(var = 'auid') %>%
    as_tibble() %>%
    left_join(self_classify) %>%
    mutate(has_cat = !is.na(category)) %>%
    GGally::ggpairs(mapping = aes(color = category, alpha = has_cat), 
                    columns = c(1:6+1, ## PCs
                                11:13)) + ## MBO values
    theme_bw()

hi_words_pc$x %>%
    as.data.frame() %>%
    rownames_to_column(var = 'auid') %>%
    as_tibble() %>%
    left_join(self_classify) %>%
    ggplot(aes(PC1, PC2, color = category, alpha = !is.na(category), 
               text = surnames)) + 
    geom_point(size = 3) +
    theme_bw() +
    coord_equal()

#' This is the most promising biplot, with some signs of separation between the discrete categories.  However, the boundary between the categories is narrow, especially as we approach the tightly-packed points near the origin.  A machine learning algorithm would require much more data in this area to work reliably.  
#' 
#' Alternatively, distance along the first principal component extracted from high-information words might make a good metric for distances between researchers.  
