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
	filter(chapter != 0)

## `lda_stability` assumes we have a token counts list
token_counts = dataf %>%
	unnest_tokens(token, text) %>%
	group_by(book, chapter, token) %>%
	summarize(n = n()) %>%
	## Let's go ahead and remove stopwords
	anti_join(stop_words, by = c('token' = 'word'))

