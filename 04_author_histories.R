library(tidyverse)
library(RCurl)
library(xml2)
library(stringr)

source('api_key.R')

## Load dataframes of auids
sloan_df_unfltd = read_rds('../MoBE-data/03_sloan_authors.Rds')
comparison_df_unfltd = read_rds('../MoBE-data/03_comparison_authors.Rds')

sloan_df = sloan_df_unfltd %>%
    unnest() %>%
    mutate(in_comparison = auid %in% unlist(comparison_df_unfltd$auid)) %>%
    filter(n_collaboration > 1 | in_comparison)
comparison_df = comparison_df_unfltd %>%
    unnest() %>%
    ungroup() %>%
    filter(!(auid %in% sloan_df$auid))

auids_to_retrieve = unique(c(sloan_df$auid, comparison_df$auid))


## Download author records ----
## All* papers for each auid
## all* = 200 most recent papers published 1999-2018
target_folder = '../MoBE-data/author_records'

scrape_ = function (this_auid) {
    ## Basically just an abstraction of the RCurl call
    base_url = 'https://api.elsevier.com/content/search/scopus?'
    query_url = str_c(base_url, 
                      'query=au-id(', this_auid, ')', '&',
                      'apiKey=', api_key, '&',
                      'count=200', '&',
                      'date=1999-2018', '&',
                      'httpAccept=application/xml')
    raw = getURL(query_url)
    raw
}
scrape = function (this_auid, target_folder) {
    ## Either scrape from the API + save the result OR pass
    target_file = str_c(target_folder, '/', this_auid, '.xml')
    if (!file.exists(target_file)) {
        raw = scrape_(this_auid)
        write_file(raw, target_file)
        return(TRUE)
    } else {
        return(TRUE)
    }
}

## ~60 minutes if all author histories need to be downloaded
plyr::l_ply(auids_to_retrieve, scrape, target_folder, .progress = 'time')


## Parse author records ----
parse = function (this_auid, target_folder) {
    target_file = str_c(target_folder, '/', this_auid, '.xml')
    # print(target_file)
    raw = read_file(target_file)
    
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
           doi = dois)
}

## ~6 minutes
papers_by_auid =  plyr::ldply(auids_to_retrieve, parse, target_folder, .progress = 'time')

length(unique(papers_by_auid$scopus_id))
write_rds(papers_by_auid, path = '../MoBE-data/04_papers_by_auid.Rds')
