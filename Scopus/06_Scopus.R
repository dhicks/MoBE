library(tidyverse)
library(RCurl)
library(stringr)
library(readr)
library(xml2)

source('api_key.R')

dataf = read_csv('../../Eisen-data/05_Zotero.csv')

# this_doi = dataf$DOI[[1]]

scopus_data = tibble()
for (this_doi in dataf$DOI) {
    ## First, use abstract retrieval to get the Scopus ID
    doi_base_url = 'https://api.elsevier.com/content/abstract/doi/'
    doi_query_url = str_c(doi_base_url, 
                          this_doi, '?', 
                          'apiKey=', api_key)
    this_sid = NA
    tryCatch({
        doi_response = read_xml(doi_query_url)
        xml_ns_strip(doi_response)
        this_sid = doi_response %>%
            xml_find_first('//dc:identifier') %>%
            xml_text() %>%
            str_extract('[0-9]+')
    },
    error = function (e) e)
    
    ## Then use the Scopus ID, since for some reason that query gives you all the authors, etc.
    if (is.na(this_sid)) {
        new_data = tibble(doi = this_doi)
    } else {
        scopus_base_url = 'https://api.elsevier.com/content/abstract/scopus_id/'
        scopus_query_url = str_c(scopus_base_url, 
                                 this_sid, '?', 
                                 'apiKey=', api_key)
        tryCatch({
            scopus_response_raw = getURL(scopus_query_url)
            scopus_response = read_xml(scopus_response_raw) 
            xml_ns_strip(scopus_response)
            
            this_cite_count = scopus_response %>%
                xml_find_all('//citedby-count') %>%
                xml_text() %>%
                as.integer()
            this_pub_date = scopus_response %>% 
                xml_find_all('//prism:coverDate') %>% 
                xml_text()
            this_title = scopus_response %>%
                xml_find_first('//dc:title') %>%
                xml_text()
            this_journal = scopus_response %>%
                xml_find_first('//prism:publicationName') %>%
                xml_text()
            
            author_list = scopus_response %>%
                xml_find_all('//author-group/author')
            these_auids = author_list %>%
                xml_attr('auid')
            these_surnames = author_list %>%
                xml_find_first('ce:surname') %>%
                xml_text()
            these_given_names = author_list %>%
                xml_find_first('ce:given-name') %>%
                xml_text()
            
            new_data = tibble(doi = this_doi, 
                              sid = this_sid,
                              cite_count = this_cite_count, 
                              pub_date = this_pub_date, 
                              title = this_title, 
                              journal = this_journal,
                              auid = list(these_auids), 
                              surnames = list(these_surnames), 
                              given_names = list(these_given_names), 
                              raw = scopus_response_raw)
        },
        error = function (e) {
            new_data = tibble(doi = this_doi, 
                              sid = this_sid,
                              error = e)
            return(new_data)})
        
    }
    scopus_data = bind_rows(scopus_data, new_data)
}

save(scopus_data, file = '../../Eisen-data/06_Scopus.Rdata')
