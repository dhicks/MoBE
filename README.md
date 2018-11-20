This repository contains the scripts for the DSI project analyzing the Sloan-funded Microbiome of the Built Environment (MoBE) collaboration.  

Briefly, the workflow begins with a CSV file of publication metadata, exported from Zotero, for the MoBE-funded documents.  The scripts use the CrossRef API to construct a comparison set of authors, then use the Scopus API to retrieve author histories, and finally build and analyze the coauthor network.  

Scripts are intended to be executed in numerical order per their filenames.  Data are assumed to be stored at the relative path `../MoBE-data/`.   Note that a series of data files are created beneath this path, and in particular nearly 100,000 Scopus API query results are stored in subfolders.  Total storage space required is approximately 1.14 GB.  

The central workflow depends on a single data file, `00_Sloan.csv`, which is included in this repository and should be moved to `../MoBE-data/` before executing the first script.  

Querying the Scopus API requires an API key.  An API key can be generated for free at <https://dev.elsevier.com>.  The key should be assigned in a file `api_key.R`, which is sourced by scripts that access the Scopus API.  Note that some data retrieval steps may exceed the weekly quotas for Scopus API queries.  The scripts are not designed to automatically detect this or pause until the quotas are reset.  Queries after passing the quotas can be detected by monitoring file sizes of the stored query responses.  

One of the original aims of this project was to examine interdisciplinary collaboration.  We attempted to impute disciplinary classifications using self-classifications from a small subset of authors.  Script `06_nogo.R` shows that this imputation cannot be successful.  This script is included to document the attempt; but cannot be run without the data file of self-classification responses, which is not publicly available and is not included here.  A `stop()` call is used at the beginning of this script to prevent it from being run automatically.  

