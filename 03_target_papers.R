## This script retrieves metadata for "target papers," which include (a) papers in the Sloan collaboration, and (b) the papers identified in 02_comparison_set to cover the comparison set of authors.  
## 
## The primary goal is to identify Scopus author IDs for all authors. 

library(tidyverse)
library(RCurl)
library(xml2)

source('api_key.R')

## Sloan collaboration papers
sloan_df = read_csv('../MoBE-data/00_Sloan.csv') %>%
    filter(!is.na(DOI))
## Comparison set
comparison_df = read_rds('../MoBE-data/02_comparison_papers.Rds')

dois = unique(c(sloan_df$DOI, comparison_df$doi))

## DOI-Scopus ID crosswalk ----
sid_doi_xwalk_file = '../MoBE-data/03_doi_sid_xwalk.Rds'
if (!file.exists(sid_doi_xwalk_file)) {
    get_scopus_id = function (this_doi) {
        doi_base_url = 'https://api.elsevier.com/content/abstract/doi/'
        doi_query_url = str_c(doi_base_url, 
                              this_doi, '?', 
                              'apiKey=', api_key)
        this_sid = NA
        tryCatch({
            response = getURL(doi_query_url)
            xml = read_xml(response)
            xml_ns_strip(xml)
            this_sid = xml %>%
                xml_find_first('//dc:identifier') %>%
                xml_text() %>%
                str_extract('[0-9]+')
        },
        error = function (e) e)
        
        return(tibble(doi = this_doi, sid = this_sid))
    }
    
    sids_df = plyr::ldply(dois, get_scopus_id, .progress = 'time')
    write_rds(sids_df, sid_doi_xwalk_file)
} else {
    sids_df = read_rds(sid_doi_xwalk_file)
}


## Download paper metadata ----
sids = sids_df %>%
    filter(!is.na(sid)) %>% 
    pull(sid)

target_folder = '../MoBE-data/paper_metadata'

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

sids_df %>%
    filter(!is.na(sid)) %>%
    pull(sid) %>%
    # head(100) %>%
    plyr::l_ply(scrape, target_folder, .progress = 'time')

## Parse paper metadata -----
parse = function (this_sid, target_folder) {
    target_file = str_c(target_folder, '/', this_sid, '.xml.zip')
    # print(target_file)
    raw = read_file(target_file)
    
    xml = read_xml(raw)
    xml = xml_ns_strip(xml)
    
    author_nodes = xml %>%
        xml_find_all('//authors/author')
    auid = author_nodes %>%
        xml_attr('auid')
    
    given_name = author_nodes %>%
        xml_find_first('ce:given-name') %>%
        xml_text()
    family_name = author_nodes %>%
        xml_find_first('ce:surname') %>%
        xml_text()

    tibble(sid = this_sid, auid, given_name, family_name)
}

## ~3 minutes
doi_auid_df = sids_df %>%
    filter(!is.na(sid)) %>%
    pull(sid) %>%
    # head(50) %>%
    plyr::ldply(parse, target_folder, .progress = 'time') %>%
    as_tibble()

## Sloan collaboration author IDs ----
sloan_authors_df = sloan_df %>%
    ## Join to Scopus data using crosswalk
    inner_join(sids_df, by = c('DOI' = 'doi')) %>%
    inner_join(doi_auid_df) %>%
    select(sid, auid:family_name) %>%
    ## Construct canonical names per author ID
    count(auid, given_name, family_name) %>%
    group_by(auid) %>%
    summarize(given_name = given_name[which.max(n)], 
              family_name = family_name[which.max(n)], 
              n_collaboration = sum(n)) %>%
    ## Then regroup by canonical names to catch some false duplicates
    group_by(given_name, family_name) %>%
    summarize(auid = list(auid), 
              n_collaboration = sum(n_collaboration)) %>%
    ungroup()

## 393 authors with > 1 paper in the collaboration
ggplot(sloan_authors_df, aes(n_collaboration)) + stat_ecdf()
filter(sloan_authors_df, n_collaboration > 1)

write_rds(sloan_authors_df, '../MoBE-data/03_sloan_authors.Rds')

## Comparison set author IDs -----
comparison_authors_df = comparison_df %>%
    unnest() %>%
    inner_join(sids_df) %>%
    inner_join(doi_auid_df) %>%
    filter(family == family_name, given == given_name) %>%
    ## Construct canonical names per author ID
    count(auid, given_name, family_name) %>%
    group_by(auid) %>%
    summarize(given_name = given_name[which.max(nn)], 
              family_name = family_name[which.max(nn)]) %>%
    arrange(family_name, given_name) %>% 
    ## Then regroup to catch false duplicates
    group_by(given_name, family_name) %>%
    summarize(auid = list(auid))

## 18 authors who ended up in both lists
## This is likely due to name variations 
## (recall simple matching was used to construct the comparison set)
intersect(simplify(comparison_authors_df$auid), 
          simplify(sloan_authors_df$auid))

write_rds(comparison_authors_df, '../MoBE-data/03_comparison_authors.Rds')

