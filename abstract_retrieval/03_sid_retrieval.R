library(tidyverse)
library(RCurl)
library(xml2)
library(stringr)

source('api_key.R')

authors = readxl::read_excel('../../Eisen-data/00_author_list.xlsx')

authors = authors %>%
    mutate(auid = str_extract(scopus_page, '[0-9]+')) %>%
    select(-scopus_page) %>%
    group_by(group, id, surname, given_name) %>%
    summarize(auids = list(auid)) %>%
    ungroup()

scrape = function (this_auid) {
    base_url = 'https://api.elsevier.com/content/search/scopus?'
    query_url = str_c(base_url, 
                      'query=au-id(', this_auid, ')', '&',
                      'apiKey=', api_key, '&',
                      'count=200', '&',
                      'date=1998-2018', '&',
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

papers_by_auid = authors$auids %>%
    unlist() %>%
    unique() %>%
    plyr::ldply(scrape, 
                .progress = 'text')

save(authors, papers_by_auid, file = '01_authors_and_papers.Rdata')


