# Required packages
library(topicmodels)
library(readr)
library(dplyr)
library(stringi)
library(tm)
library(LDAvis)
library(jsonlite)


target_folder <- "../../Eisen-data/"
input_file <- paste0(target_folder, "04_abstracts.json")
output_model_json_path <- paste0(target_folder, "05_lda.json")

data <-fromJSON(input_file)

# Here we extract for the document collection the kind of text we are interested in.
abstracts <- data$abstract
abstracts <- abstracts[!is.na(abstracts)] # Remove NA entries

## topicsmodels::LDA expects a tm::DocumentTermMatrix
corp <- Corpus(VectorSource(abstracts))
control_params <- list(stemming = TRUE, stopwords = TRUE,
		       minWordLength = 2, removeNumbers = TRUE,
		       removePunctuation = TRUE)
dtm <- DocumentTermMatrix(corp, control = control_params)

# Change automatically generated integer ids for document scopus_ids,
# so we can LDA results to document metadata as some later point.
dtm$dimnames$Docs <- data$scopus_id[!is.na(data$abstract)]


lda = LDA(dtm, 2, control = list(seed = 42, verbose = 1))

save(lda, file = '03_lda.Rdata')

## LDAvis::createJSON() raises an error for k=17 w/ default mds.method
## Fix from https://github.com/cpsievert/LDAvis/issues/56#issuecomment-273444822

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
    # This function takes the topicmodels::LDA output, along with its tm::Corpus
    # and tm::DocumentTermMatrix, and extracts from them the JSON object that
    # LDAvis::serVis() expects.
    
    jsPCA_fixed = function (phi) {
        jensenShannon <- function(x, y) {
            m <- 0.5 * (x + y)
            0.5 * sum(ifelse(x==0, 0, x * log(x/m))) + 
                0.5 * sum(ifelse(y==0, 0, y * log(y/m)))
        }
        dist.mat <- proxy::dist(x = phi, method = jensenShannon)
        if (attr(dist.mat, 'Size') > 2) {
            pca.fit <- stats::cmdscale(dist.mat, k = 2)
            data.frame(x = pca.fit[, 1], y = pca.fit[, 2])
        } else {
            pca.fit <- stats::cmdscale(dist.mat, k = 1)
            data.frame(x = pca.fit[, 1], y = 0)
        }
    }
    
    # Find required quantities
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    doc_length <- vector()
    for (i in 1:length(corpus)){ 
        temp <- paste(corpus[[i]]$content, collapse = ' ')
        doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    freq_matrix <- data.frame(ST = colnames(doc_term),
                              Freq = colSums(as.matrix(doc_term)))
    
    # Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                   vocab = vocab,
                                   doc.length = doc_length,
                                   term.frequency = freq_matrix$Freq, 
                                   mds.method = jsPCA_fixed)
    
    return(json_lda)
}

lda_json <- topicmodels_json_ldavis(lda, corp, dtm)

## Save the LDA model to a JSON file to avoid computing every time we visualize
lda_json %>% write_lines(output_model_json_path)

# Generate interactive LDAvis shiny app
serVis(lda_json)
