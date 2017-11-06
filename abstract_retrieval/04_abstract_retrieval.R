library(tidyverse)
library(RCurl)
library(xml2)
library(stringr)

library(foreach)
library(doSNOW)

source('api_key.R')

load('../../Eisen-data/03_authors_and_papers.Rdata')

## Folder to store XML retrieved from API
target_folder = '../../Eisen-data/04-abstracts'
if (!dir.exists(target_folder)) {
    dir.create(target_folder)
}


## Functions used in the metadata retrieval process
scrape_ = function (this_sid) {
    ## Basically just an abstraction of the RCurl call
    base_url = 'https://api.elsevier.com/content/abstract/scopus_id/'
    query_url = str_c(base_url, 
                      this_sid, '?',
                      'apiKey=', api_key)
    
    raw = getURL(query_url)
    raw
}

scrape = function (this_sid, target_folder) {
    ## Either scrape from the API + save the result OR load the saved result
    target_file_xml = str_c(target_folder, '/', this_sid, '.xml')
    target_file = str_c(target_folder, '/', this_sid, '.xml.zip')
    if (!file.exists(target_file)) {
        raw = scrape_(this_sid)
        write_file(raw, target_file_xml)
        zip(target_file, target_file_xml, flags = '-9Xq')
        unlink(target_file_xml)
    } else {
        raw = read_file(target_file)
    }
    return(raw)
}

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
    
    tibble(scopus_id, doi, title, journal, 
           date, abstract, keywords, 
           auids)
}

get_metadata = function (this_sid, target_folder) {
    raw = scrape(this_sid, target_folder)
    dataf = parse(raw)
    
    return(dataf)
}


## Identify papers to retrieve
papers_to_retrieve = unique(papers_by_auid$scopus_id)
# papers_to_retrieve = papers_to_retrieve[1:200]
  

## Run metadata retrieval
cl = makeCluster(2)
registerDoSNOW(cl)
pb = txtProgressBar(max = length(papers_to_retrieve), style = 3)
progress = function(n) setTxtProgressBar(pb, n)
system.time(
abstracts_df <- foreach(paper = papers_to_retrieve, 
        .combine = bind_rows, 
        .multicombine = TRUE, 
        .packages = c('tidyverse', 'RCurl', 'xml2', 'stringr'),
        .options.snow = list(progress = progress),
        .verbose = FALSE) %dopar%
    get_metadata(paper, target_folder)
)
stopCluster(cl)


## Filter down to auids in our list of authors
abstracts_df = abstracts_df %>%
    unnest(auids, .drop = FALSE) %>%
    filter(auids %in% authors$auid) %>% 
    group_by(scopus_id, doi, title, journal, date) %>%
    summarize(abstract = first(abstract),
              keywords = list(first(keywords)),
              auids = list(auids))


## Save results
save(abstracts_df, file = '04_abstracts.Rdata')

abstracts_df %>%
    select(-raw) %>%
    jsonlite::toJSON() %>%
    write_lines('04_abstracts.json')
