# Required packages
library(topicmodels)
library(readr)
library(dplyr)
library(stringi)
library(tm)
library(LDAvis)
library(jsonlite)


setwd("/home/hugo/Projects/DSI/Eisen-data/")
input_file <- "02_abstracts.json"
output_model_json_path <- "/home/hugo/Projects/DSI/Eisen/topic_modeling/lda.json"

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
lda = LDA(dtm, 5)

# This function takes the topicmodels::LDA output, along with its tm::Corpus
# and tm::DocumentTermMatrix, and extracts from them the JSON object that
# LDAvis::serVis() expects.
topicmodels_json_ldavis <- function(fitted, corpus, doc_term){

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
				       term.frequency = freq_matrix$Freq)

	return(json_lda)
}

lda_json <- topicmodels_json_ldavis(lda, corp, dtm)

## Save the LDA model to a JSON file to avoid computing every time we visualize
lda_json %>% write_lines(output_model_json_path)

# Generate interactive LDAvis shiny app
serVis(lda_json)
