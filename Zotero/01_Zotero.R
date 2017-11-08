library(tidyverse)
library(stringr)
library(jsonlite)
library(RCurl)

## https://www.zotero.org/groups/1658064/sloan_mobe_public
group_url = 'https://api.zotero.org/groups/1658064/items'

total_items = getURL(str_c(group_url, '?', 
                                  'itemType=journalArticle'),
        .opts = list(header = TRUE)) %>%
    str_match('Total-Results: ([0-9]+)') %>%
    .[,2] %>%
    as.integer()

query_limit = 100

n_queries = ceiling(total_items / query_limit)

data = tibble()
for (i in 1:n_queries) {
    new_data = fromJSON(str_c(group_url, '?',
                              'itemType=journalArticle', '&',
                              'start=',(i-1)*query_limit, '&',
                              'limit=', query_limit)) %>% 
        .$data %>%
        select(title, publicationTitle, DOI)
    data = bind_rows(data, new_data)
}

write_csv(data, '../../Eisen-data/01_zotero.csv')
