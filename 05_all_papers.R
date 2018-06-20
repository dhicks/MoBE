library(tidyverse)
library(RCurl)
library(xml2)
library(stringr)

library(foreach)
library(doSNOW)

source('api_key.R')

author_histories = read_rds('../Eisen-data/04_papers_by_auid.Rds')

## Identify papers to retrieve
papers_to_retrieve = author_histories %>%
    filter(!is.na(scopus_id)) %>% 
    pull(scopus_id) %>%
    unique()
# this_paper = papers_to_retrieve[10000]
# get_metadata(this_paper, target_folder)

## Folder to store XML retrieved from API
target_folder = '../Eisen-data/paper_metadata'

## Functions for scraping from API ----
scrape_ = function (this_sid) {
    ## Basically just an abstraction of the RCurl call
    base_url = 'https://api.elsevier.com/content/abstract/scopus_id/'
    query_url = str_c(base_url, 
                      this_sid, '?',
                      'apiKey=', api_key)
    print(query_url)
    raw = getURL(query_url)
    raw
}

scrape = function (this_sid, target_folder) {
    ## Either scrape from the API + save the result OR pass
    target_file_xml = str_c(target_folder, '/', this_sid, '.xml')
    target_file = str_c(target_folder, '/', this_sid, '.xml.zip')
    if (!file.exists(target_file)) {
        raw = scrape_(this_sid)
        write_file(raw, target_file_xml)
        zip(target_file, target_file_xml, flags = '-9Xq')
        unlink(target_file_xml)
        return(TRUE)
    } else {
        return(TRUE)
    }
}

## Scrape from API ----
## ~12 hours if everything needs to be downloaded, using 2 cores
cl = makeCluster(2)
registerDoSNOW(cl)
pb = txtProgressBar(max = length(papers_to_retrieve), style = 3)
progress = function(n) setTxtProgressBar(pb, n)
system.time(
success <- foreach(paper = papers_to_retrieve, 
                        .combine = c, 
                        .multicombine = TRUE, 
                        .packages = c('tidyverse', 'RCurl', 'stringr'),
                        .options.snow = list(progress = progress),
                        .verbose = FALSE) %dopar%
    scrape(paper, target_folder)
)
stopCluster(cl)



## Parser ----
parse = function (raw) {
    xml = read_xml(raw)
    xml = xml_ns_strip(xml)
    
    scopus_id = xml %>%
        xml_find_first('//dc:identifier') %>%
        xml_text() %>%
        str_extract('[0-9]+')
    
    doi = xml %>%
        xml_find_first('//prism:doi') %>%
        xml_text()
    title = xml %>%
        xml_find_first('//dc:title') %>%
        xml_text()
    journal = xml %>%
        xml_find_first('//prism:publicationName') %>%
        xml_text()
    issn = xml %>%
        xml_find_first('//prism:issn') %>%
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
    # surnames = author_nodes %>%
    #     xml_find_first('ce:surname') %>%
    #     xml_text() %>%
    #     list()
    # given_names = author_nodes %>%
    #     xml_find_first('ce:given-name') %>%
    #     xml_text() %>%
    #     list()
    
    references = xml %>%
        xml_find_all('//bibliography/reference') %>%
        xml_attr('id')

    tibble(scopus_id, doi, title, journal, issn,
           date, abstract, keywords, 
           auids, references = list(references))
}

## ~ 3.3 hours
## NB ~10% of papers in target_folder don't match one of our author IDs and are just filtered out in the next step.  It would probably be (a little) more efficient to parse just those identified in papers_to_retrieve.  
xml_to_parse = str_subset(dir(target_folder), '.xml.zip')
cl = makeCluster(2)
registerDoSNOW(cl)
pb = txtProgressBar(max = length(papers_to_retrieve), style = 3)
progress = function(n) setTxtProgressBar(pb, n)
system.time(
abstracts_df <- foreach(xml_file = xml_to_parse, 
                       .combine = bind_rows, 
                       .multicombine = TRUE, 
                       .packages = c('tidyverse', 'xml2', 'stringr'),
                       .options.snow = list(progress = progress),
                       .verbose = FALSE) %dopar% {
                           full_path = str_c(target_folder, '/', xml_file)
                           raw = read_file(full_path)
                           parse(raw)
                       }
)
stopCluster(cl)


## Filter down to auids in our list of authors
## ~ 15 sec
tictoc::tic()
abstracts_df = abstracts_df %>%
    unnest(auids, .drop = FALSE) %>%
    filter(auids %in% author_histories$auid) %>% 
    group_by(scopus_id, doi, title, journal, issn, date) %>%
    summarize(abstract = first(abstract),
              keywords = list(first(keywords)),
              auids = list(auids)) %>%
    ungroup()
tictoc::toc()


## Save results ----
write_rds(abstracts_df, '../Eisen-data/05_abstracts.Rds')

abstracts_df %>%
    jsonlite::toJSON() %>%
    write_lines('../Eisen-data/05_abstracts.json')
