library(tidyverse)
library(RCurl)
library(xml2)
library(stringr)

source('api_key.R')

## Load the Scopus metadata for the Zotero library papers
load('../../Eisen-data/02_Scopus.Rdata')

## Reshape scopus_data into one-row-per-auid-paper
## For now ignore the false duplicates issue:  
## https://github.com/dhicks/Eisen/issues/1
authors_unfltd = scopus_data %>%
    select(-raw) %>%
    filter(!is.na(sid)) %>%
    unnest() %>%
    group_by(auid) %>%
    summarize(surnames = list(unique(surnames)),
              given_names = list(unique(given_names)), 
              n = n()) %>%
    arrange(desc(n))

## Distribution of paper counts in the Zotero library
ggplot(authors_unfltd, aes(n)) + 
    geom_bar()
## Something like 2/3rds of the authors have only 1 paper
authors = filter(authors_unfltd, n > 1)


## Function to retrieve all* papers for a given auid
## all* = 200 most recent papers published 2003-2018
scrape = function (this_auid) {
    base_url = 'https://api.elsevier.com/content/search/scopus?'
    query_url = str_c(base_url, 
                      'query=au-id(', this_auid, ')', '&',
                      'apiKey=', api_key, '&',
                      'count=200', '&',
                      'date=2003-2018', '&',
                      'httpAccept=application/xml')
    
    raw = getURL(query_url)
    xml = read_xml(raw)
    xml_ns_strip(xml)
    
    entries = xml_find_all(xml, 'entry')
    
    eids = entries %>%
        xml_find_first('eid') %>%
        xml_text()
    scopus_ids = entries %>% 
        xml_find_first('dc:identifier') %>%
        xml_text() %>%
        str_extract('[0-9]+')
    titles = entries %>%
        xml_find_first('dc:title') %>%
        xml_text()
    journals = entries %>%
        xml_find_first('prism:publicationName') %>%
        xml_text()
    dates = entries %>%
        xml_find_first('prism:coverDate') %>%
        xml_text()
    dois = entries %>%
        xml_find_first('prism:doi') %>%
        xml_text()
    
    tibble(auid = this_auid, 
           eid = eids,
           scopus_id = scopus_ids, 
           title = titles, 
           journal = journals, 
           date = dates,
           doi = dois, 
           raw = raw)
}

papers_by_auid =  plyr::ldply(authors$auid, scrape, .progress = 'text')

papers_raw = select(papers_by_auid, scopus_id, raw)
papers_by_auid = select(papers_by_auid, -raw)

save(authors, papers_by_auid, file = '../../Eisen-data/03_authors_and_papers.Rdata')
save(papers_raw, file = '../../Eisen-data/03_papers_raw.Rdata')

