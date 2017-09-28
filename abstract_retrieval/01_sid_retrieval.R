library(tidyverse)
library(RCurl)
library(xml2)
library(stringr)

source('api_key.R')

authors = read_csv('test_auids.csv')

scrape = function (this_auid) {
    base_url = 'https://api.elsevier.com/content/search/scopus?'
    query_url = str_c(base_url, 
                      'query=au-id(', this_auid, ')', '&',
                      'apiKey=', api_key, '&',
                      'count=200', '&',
                      # 'date=2002-2017', '&',
                      'httpAccept=application/xml')
    
    raw = getURL(query_url)
    xml = read_xml(raw)
    xml_ns_strip(xml)
    
    entries = xml_find_all(xml, 'entry')
    
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
           scopus_id = scopus_ids, 
           title = titles, 
           journal = journals, 
           date = dates,
           doi = dois, 
           raw = raw)
}

papers_by_auid = authors$auid %>%
    unique() %>%
    plyr::ldply(scrape, 
                .progress = 'text')

save(papers_by_auid, file = '01_papers_by_auid.Rdata')


