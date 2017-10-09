library(tidyverse)
library(stringr)

load('../../Eisen-data/03_lda.Rdata')

terms_lda = lda@beta %>%
    t() %>%
    as_tibble() %>%
    rename_all(funs(str_replace(., 'V', 'topic_'))) %>%
    mutate(term = lda@terms) %>%
    select(term, everything())

## Currently the document identifiers don't correspond to anything in our metadata
documents_lda = lda@gamma %>% 
    as_tibble() %>%
    rename_all(funs(str_replace(., 'V', 'topic_'))) %>%
    mutate(scopus_id = lda@documents) %>%
    select(scopus_id, everything())

save(terms_lda, documents_lda, file = '../../Eisen-data/04_lda_dfs.Rdata')