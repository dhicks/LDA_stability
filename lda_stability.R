## This file defines a function LDA_stability to implement 
## the algorithm described in https://arxiv.org/pdf/1404.4606v3.pdf



# library(tidyverse)
# library(igraph)
# library(reshape2)
# library(SnowballC)
# library(stringr)
# library(tidytext)
# library(topicmodels)
# library(tikzDevice)

library(foreach)

## TODO: 
## - let user pass document, token, n column identifiers
## - helper functions shouldn't be exported

## --------------------
## Helper functions to be used to calculate agreement scores
## Calculate Jaccard similarities; 1 = identical sets
jaccard = function (x, y) {
	length(intersect(x, y)) / length(union(x, y))
}
## Average Jaccard similarities over partial ranked sets
avg_jaccard = function (x, y) {
	t = min(length(x), length(y))
	gammas = sapply(1:t, function (d) jaccard(head(x, d), head(y, d)))
	aj = 1/t * sum(gammas)
	return(aj)
}

## Take the results of LDA and produce a list of ranked sets
extract_rankset = function(tidy_lda, t) {
	tidy_lda %>%
		group_by(topic) %>%
		top_n(t, beta) %>%
		ungroup() %>%
		arrange(topic, -beta) %>%
		split(.$topic) %>%
		lapply(function (x) x$term)
}


## --------------------
## Given two ranksets, calculate their agreement score
agreement = function (s0_rankset, si_rankset) {
	## Construct an edgelist from the two ranksets
	aj_edgelist = expand.grid(s0_rankset, si_rankset) %>% 
		## Calculate the average Jaccard similarity at each pair of lists from the two sets
		mutate(aj = {Map(avg_jaccard, Var1, Var2) %>% unlist}) %>% 
		mutate(Var1 = paste('s0', names(Var1), sep = '.'),
			   Var2 = paste('si', names(Var2), sep = '.')) %>%
		## Arrange them into a matrix
		as.matrix
	## Turn this into a bipartite graph
	aj_graph = graph_from_edgelist(aj_edgelist[,1:2])
	E(aj_graph)$weight = aj_edgelist[,3]
	V(aj_graph)$type = str_detect(V(aj_graph)$name, 's0')
	## Use igraph::max_bipartite_match to extract the pairing with the maximum average agreement
	max_bipartite_match(aj_graph)$matching_weight / k
}


## --------------------
lda_stability = function (token_counts,  ## token count df
						  beta = .80,    ## fraction of docs to include in each sample
						  tau = 20,      ## num. samples
						  k_range = 2:5, ## range of values of k to fit
						  t = 15         ## max num. terms in each ranked list
) {
	library(foreach)
	
	## Number of documents
	n = length(unique(token_counts$document))
	
	print('pre-dtm')
	## Cast token_counts into a document-term matrix
	projects_dtm = cast_dtm(token_counts, document, token, token_n)
	
	print('pre-samples')
	## Draw sample corpora, and convert each to DTM
	set.seed(54321)
	print(beta * n)
	samples = lapply(1:tau, function (x) sample(unique(token_counts$document), 
												size = beta * n)) %>%
		lapply(function (x) filter(token_counts, document %in% x)) %>%
		lapply(function (x) cast_dtm(x, document, token, token_n))
	
	print('pre-agree_scores')
	agree_scores = foreach(k = k_range, 
						   .combine = 'cbind', 
						   .verbose = TRUE, 
						   .packages = 'topicmodels') %do% 
						   {
						   	## 1. Apply the topic modeling algorithm to the complete data set of n documents
						   	##    to generate k topics, and represent the output as the reference ranking set
						   	##    S0.
						   	s0 = LDA(projects_dtm, k = k, control = list(seed = 42)) %>%
						   		tidy()
						   	
						   	## 2. For each sample Xi:
						   	## (a) Apply the topic modeling algorithm to Xi to generate k topics, and
						   	##     represent the output as the ranking set Si.
						   	si = lapply(samples, function (x) {LDA(x, k = k) %>% tidy})
						   	
						   	## (b) Calculate the agreement score agree(S0, Si).
						   	s0_rankset = extract_rankset(s0, t)
						   	si_ranksets = lapply(si, function (x) extract_rankset(x, t))
						   	
						   	agree_scores = foreach(rankset = si_ranksets, 
						   						   .combine = 'cbind', .verbose = TRUE, 
						   						   .packages = c('tidytext', 'dplyr', 'igraph'), 
						   						   .export = 'agreement') %dopar% 
						   						   {agreement(s0_rankset, rankset)}
						   	
						   	agree_scores
						   }						   			  	
	print('agreement_scores')
	agreement_scores = agree_scores %>% 
		as_tibble %>%
		melt %>%
		mutate(variable = {str_replace(variable, 'result.', '') %>%
				as.numeric %>% k_range[.]})
	return(agreement_scores)
}
