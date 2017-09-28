library(tidyverse)
library(RCurl)
library(xml2)
library(stringr)

source('api_key.R')

load('01_papers_by_auid.Rdata')

scrape = function (this_sid) {
    base_url = 'https://api.elsevier.com/content/abstract/scopus_id/'
    query_url = str_c(base_url, 
                      this_sid, '?',
                      'apiKey=', api_key)
    
    raw = getURL(query_url)
    xml = read_xml(raw)
    xml_ns_strip(xml)
    
    doi = xml %>%
        xml_find_first('//prism:doi') %>%
        xml_text()
    title = xml %>%
        xml_find_first('//dc:title') %>%
        xml_text()
    journal = xml %>%
        xml_find_first('//prism:publicationName') %>%
        xml_text()
    date = xml %>%
        xml_find_first('//prism:coverDate') %>%
        xml_text()
    abstract = xml %>%
        xml_find_first('//abstract//ce:para') %>%
        xml_text()
    keywords = xml %>%
        xml_find_all('//authkeywords//author-keyword') %>%
        xml_text() %>%
        list()
    
    author_nodes = xml %>%
        xml_find_all('//authors/author')
    auids = author_nodes %>%
        xml_attr('auid') %>%
        list()
    surnames = author_nodes %>%
        xml_find_first('ce:surname') %>%
        xml_text()
    given_names = author_nodes %>%
        xml_find_first('ce:given-name') %>%
        xml_text() %>%
        list()
    
    tibble(scopus_id = this_sid, 
           doi = doi, 
           title = title, 
           journal = journal, 
           date = date, 
           abstract = abstract, 
           keywords = keywords, 
           auids = auids, 
           surnames = surnames, 
           given_names = given_names,
           raw = raw)
}

abstracts_df = papers_by_auid$scopus_id %>%
    unique() %>%
    # head() %>%
    plyr::ldply(scrape, 
                .progress = 'text')

abstracts_df %>%
    select(-raw) %>%
    jsonlite::toJSON() %>%
    write_lines('02_abstracts.json')
