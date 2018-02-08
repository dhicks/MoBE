## Extract references from core papers
library(tidyverse)
library(RCurl)
library(foreach)
library(doSNOW)

cl = makeCluster(2)
registerDoSNOW(cl)

## ------------------------------
## Core paper references
## If the dataframe of core paper references has already been built, 
## then just load it.  
## Otherwise we need to parse the core paper xml files.  
references_file = '../../Eisen-data/05_core_refs.Rdata'
if (file.exists(references_file)) {
    target_folder = '../../Eisen-data/04-abstracts'
    xml_to_parse = str_subset(dir(target_folder), '.xml.zip')
    load(references_file)
} else {
    parse = parse = function (raw) {
        xml = read_xml(raw)
        xml = xml_ns_strip(xml)
        
        scopus_id = xml %>%
            xml_find_first('//dc:identifier') %>%
            xml_text() %>%
            str_extract('[0-9]+')
        
        ref_id = xml %>%
            xml_find_all('//bibliography/reference') %>%
            xml_find_first('.//itemid[@idtype="SGR"]') %>%
            xml_text()
        
        return_df = tibble(scopus_id, ref_id)
        return(return_df)
    }
    
    ## NB approx. 45 minutes w/ 1 core to do all 23k papers
    pb = txtProgressBar(max = length(xml_to_parse), style = 3)
    progress = function(n) setTxtProgressBar(pb, n)
    system.time({
        core_refs_df <- foreach(xml_file = xml_to_parse[1:200], 
                                 .combine = bind_rows, 
                                 .multicombine = TRUE, 
                                 .packages = c('tidyverse', 'xml2', 'stringr'),
                                 .options.snow = list(progress = progress),
                                 .verbose = FALSE) %dopar% {
                                     full_path = str_c(target_folder, '/', xml_file)
                                     raw = read_file(full_path)
                                     parse(raw)
                                 }
    })
    save(core_refs_df, file = references_file)
}

## Distribution of citation counts for references, eg, 70% of references are only cited 1 time or whatever
core_refs_df %>%
    count(ref_id) %>%
    rename(cite_count = n) %>%
    arrange(desc(cite_count)) %>%
    ggplot(aes(cite_count)) + 
    stat_ecdf()


## ------------------------------
## Forward citation search

sid_to_eid = function(sid) {
    ## Convert a Scopus ID to an EID
    str_c('2-s2.0-', sid)
}

## Where are the forward citation search results going? 
target_folder = '../../Eisen-data/05_forward_cites'
if (!dir.exists(target_folder)) {
    dir.create(target_folder)
}

## Functions to scrape the API
source('api_key.R')
scrape_ = function (this_eid) {
    ## Basically just an abstraction of the RCurl call
    base_url = 'https://api.elsevier.com/content/search/scopus'
    query_url = str_c(base_url, '?query=refeid(', this_eid, ')',
                      '&count=200',  ## max 200 results per query
                      '&httpAccept=application/xml',
                      '&apiKey=', api_key)
    
    raw = getURL(query_url)
    raw
}
scrape = function (this_sid, target_folder) {
    ## Either scrape from the API + save the result OR pass
    target_file_xml = str_c(target_folder, '/', this_sid, '.xml')
    target_file = str_c(target_folder, '/', this_sid, '.xml.zip')
    if (!file.exists(target_file)) {
        this_eid = sid_to_eid(this_sid)
        raw = scrape_(this_eid)
        write_file(raw, target_file_xml)
        return(TRUE)
    } else {
        return(TRUE)
    }
}

sids_to_scrape = core_refs_df %>%
    pull(ref_id) %>%
    unique()

pb = txtProgressBar(max = length(sids_to_scrape), style = 3)
progress = function(n) setTxtProgressBar(pb, n)
system.time(
success <- foreach(this_sid = sids_to_scrape[1:200], 
                   .combine = c, 
                   .multicombine = TRUE, 
                   .packages = c('tidyverse', 'RCurl', 'stringr'),
                   .options.snow = list(progress = progress),
                   .verbose = TRUE) %do%
    scrape(this_sid, target_folder)
)

success_df = tibble(sids_to_scrape, success)
save(success_df, file = '../../Eisen-data/05_forward_search_success.Rdata')
