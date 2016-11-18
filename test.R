# source('lda_stability.R')

library(tidyverse)
library(stringr)
library(tidytext)
library(janeaustenr)

## Set up a test dataf, as in the `tidytext`` vignette
dataf = austen_books() %>%
	group_by(book) %>%
	mutate(chapter = cumsum(str_detect(text, regex('^chapter [\\divxlc]', 
												   ignore_case = TRUE)))) %>%
	ungroup() %>%
	mutate(document = paste(book, chapter)) %>%
	filter(chapter != 0) %>%
	select(-book, -chapter)

## `lda_stability` assumes we have a token counts list
token_counts = dataf %>%
	unnest_tokens(token, text) %>%
	group_by(document, token) %>%
	summarize(token_n = n()) %>%
	## Let's go ahead and remove stopwords
	anti_join(stop_words, by = c('token' = 'word'))

## Now's the time to set up parallel processing
library(doParallel)
cl = makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()

source('lda_stability.R')
agreement_scores = lda_stability(token_counts, tau = 3, k_range = 2:3, t = 5)
