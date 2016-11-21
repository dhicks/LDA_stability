## A simple test/vignette for `lda_stability`

library(tidyverse)
library(stringr)
library(tidytext)
library(janeaustenr)

## Set up a test dataf, as in the `tidytext` vignette
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

## Set up parallel processing
library(doParallel)
cl = makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()

## Generate the agreement scores
source('lda_stability.R')
k_range = 2:12
start.time = proc.time()
agreement_scores = lda_stability(token_counts,   ## token count df
								 beta = .80,     ## fraction of docs to include in each sample
								 tau = 20,       ## num. samples
								 k_range = k_range, ## range of values of k to fit
								 t = 15          ## max num. terms in each ranked list
								 )
end.time = proc.time()

end.time - start.time

## Plot the results
ggplot(agreement_scores, aes(k, agreement)) + 
	geom_point() + 
	stat_summary(geom = 'line') +
	scale_x_continuous(breaks = k_range)

