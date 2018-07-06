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
parse_ = function (raw) {
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
    
    subject_nodes = xml %>%
        xml_find_all('//subject-areas/subject-area')
    subject_code = subject_nodes %>%
        xml_attr('code')
    subject_name = subject_nodes %>%
        xml_text()
    subject_area = subject_nodes %>%
        xml_attr('abbrev')
    subjects = tibble(subject_code, 
                      subject_name, 
                      subject_area)
    
    author_nodes = xml %>%
        xml_find_all('//authors/author')
    auids = author_nodes %>%
        xml_attr('auid')
    surnames = author_nodes %>%
        xml_find_first('ce:surname') %>%
        xml_text()
    given_names = author_nodes %>%
        xml_find_first('ce:given-name') %>%
        xml_text()
    authors = tibble(auid = auids, 
                     surname = surnames, 
                     given_name = given_names)
    
    references = xml %>%
        xml_find_all('//bibliography/reference') %>%
        xml_attr('id')

    tibble(scopus_id, doi, title, journal, issn,
           date, abstract, keywords, 
           authors = list(authors), 
           subjects = list(subjects),
           references = list(references))
}
parser = function(scopus_id, target_folder) {
    full_path = str_c(target_folder, '/', scopus_id, '.xml.zip')
    raw = read_file(full_path)
    return(parse_(raw))
}

## ~ 2 hours
cl = makeCluster(3)
registerDoSNOW(cl)
pb = txtProgressBar(max = length(papers_to_retrieve), style = 3)
progress = function(n) setTxtProgressBar(pb, n)
tictoc::tic()
abstracts_df_unfltd <- foreach(scopus_id = papers_to_retrieve, 
                       .combine = bind_rows, 
                       .multicombine = TRUE, 
                       .packages = c('tidyverse', 'xml2', 'stringr'),
                       .options.snow = list(progress = progress),
                       .verbose = FALSE) %dopar% 
    parser(scopus_id, target_folder)
tictoc::toc()
stopCluster(cl)


## Filter down to auids in our list of authors
## ~ 19 sec
tictoc::tic()
other_list_cols = abstracts_df_unfltd %>%
    group_by(scopus_id) %>%
    select_if(is.list) %>%
    ungroup() %>%
    select(-authors)

abstracts_df = abstracts_df_unfltd %>%
    unnest(authors, .drop = TRUE) %>%
    filter(auid %in% author_histories$auid) %>% 
    nest(auid:given_name, .key = 'authors') %>%
    left_join(other_list_cols)
tictoc::toc()


## Save results ----
write_rds(abstracts_df, '../Eisen-data/05_abstracts.Rds')

abstracts_df %>%
    jsonlite::toJSON() %>%
    write_lines('../Eisen-data/05_abstracts.json')
