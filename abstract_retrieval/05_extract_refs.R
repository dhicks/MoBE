## Extract references from core papers
library(tidyverse)
library(RCurl)
library(foreach)
library(doSNOW)

cl = makeCluster(2)
registerDoSNOW(cl)

## ------------------------------
## Core paper references
## Either load df of core paper references, 
## or parse refs from core paper xml files
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
        core_refs_df <- foreach(xml_file = xml_to_parse, 
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
core_refs_count = core_refs_df %>%
    count(ref_id) %>%
    rename(cite_count = n) %>%
    arrange(desc(cite_count))
ggplot(core_refs_count, aes(cite_count)) + 
    stat_ecdf() +
    scale_x_log10()

threshold = quantile(core_refs_count$cite_count, probs = .999)


## ------------------------------
## Forward citation search

sid_to_eid = function(sid) {
    ## Convert a Scopus ID to an EID
    str_c('2-s2.0-', sid)
}

## Scopus IDs of interest
core_refs_count %>%
    filter(cite_count >= threshold) %>%
    pull(ref_id) %>%
    ## Convert to EIDs
    sid_to_eid %>%
    ## Form Scopus query
    str_c('refeid(', ., ')', collapse = ' OR ') %>%
    str_c('PUBYEAR AFT 2002 AND (', ., ')') %>%
    ## And write to disk
    write_lines('../../Eisen-data/05_forward_search_query.txt')

## This yields about 1M documents
