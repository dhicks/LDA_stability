## This file defines a function LDA_stability to implement 
## the algorithm described in https://arxiv.org/pdf/1404.4606v3.pdf

library(foreach)

## TODO: 
## - let user pass document, token, n column identifiers
## - helper functions shouldn't be exported
## - set seed externally
## - agreement_rankset, but not agreement_gamma, requires a value of t.  currently, to prevent option errors, agreement_gamma has an inert slot for t.  make this less clunky 

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
extract_rankset = function(lda, t) {
    lda %>% 
        tidy %>%
        group_by(topic) %>%
        top_n(t, beta) %>%
        ungroup() %>%
        arrange(topic, -beta) %>%
        split(.$topic) %>%
        lapply(function (x) x$term)
}


## --------------------
## Calculate an agreement score between two fitted topicmodels using rankset Jaccard agreement
agreement_rankset = function (s0, si, t) {
    ## Extract the ranksets
    s0_rankset = extract_rankset(s0, t)
    si_rankset = extract_rankset(si, t)
    
    k = length(s0_rankset)
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

## Calculate an agreement score between two fitted topicmodels using document assignment [gamma] correlation
agreement_gamma = function(s0, si, t) {
    ## Extract the gammas
    s0_gamma = s0 %>% tidy(matrix = 'gamma') %>% spread(topic, gamma, sep = '.')
    si_gamma = si %>% tidy(matrix = 'gamma') %>% spread(topic, gamma, sep = '.')
    
    k = length(s0_gamma) -1
    
    ## Build an adjacency matrix based on correlations
    aj_adj = right_join(s0_gamma, si_gamma, by = 'document') %>% 
        select(-document) %>%
        cor
    ## Correlations within the same model should be 0
    aj_adj[str_detect(rownames(aj_adj), 'x'),
           str_detect(colnames(aj_adj), 'x')] = 0
    aj_adj[str_detect(rownames(aj_adj), 'y'),
           str_detect(colnames(aj_adj), 'y')] = 0
    
    ## To find the maximum, we build a bipartite graph and 
    ## use a bipartite matching algorithm
    
    ## Build the graph
    aj_graph = graph_from_adjacency_matrix(aj_adj, weighted = TRUE, 
                                           mode = 'upper')
    ## Set types based on model designator
    V(aj_graph)$type = str_detect(V(aj_graph)$name, 'x')
    ## This gives the average correlation for the best match
    max_bipartite_match(aj_graph)$matching_weight / k
}


## --------------------
lda_stability = function (token_counts,  ## token count df
                          beta = .80,    ## fraction of docs to include in each sample
                          tau = 20,      ## num. samples
                          k_range = 2:5, ## range of values of k to fit
                          agreement_fn = agreement_rankset,
                          ## function to use to calculate agreement scores
                          t = 15,         ## max num. terms in each ranked list
                          verbose = TRUE ## verbose output from the foreach loops
) {
    library(foreach)
    
    ## Number of documents
    n = length(unique(token_counts$document))
    
    ## Cast token_counts into a document-term matrix
    projects_dtm = cast_dtm(token_counts, document, token, token_n)
    
    ## Draw sample corpora, and convert each to DTM
    set.seed(54321)
    samples = lapply(1:tau, function (x) sample(unique(token_counts$document), 
                                                size = beta * n)) %>%
        lapply(function (x) filter(token_counts, document %in% x)) %>%
        lapply(function (x) cast_dtm(x, document, token, token_n))
    
    ## Iterate over the values of k
    agree_scores = foreach(k = k_range, 
                           .combine = 'cbind', 
                           .verbose = verbose, 
                           .packages = c('topicmodels', 'tidyr')
    ) %do% 
    {
        ## 1. Apply the topic modeling algorithm to the complete data set of n documents
        s0 = LDA(projects_dtm, k = k, control = list(seed = 42))
        
        ## 2. For each sample Xi:
        ## (a) Apply the topic modeling algorithm to Xi to generate k topics.
        sis = foreach(sample = samples, 
                      .packages = c('topicmodels', 'tidytext', 'tidyr')
        ) %dopar% 
        {LDA(sample, k = k)}
        
        ## (b) Calculate the agreement score agree(S0, Si).
        
        ## What functions need to be exported to the agreement function? 
        if (identical(agreement_fn, agreement_rankset)) {
            fns_to_export = c('extract_rankset', 'avg_jaccard', 'jaccard')
        } else if (identical(agreement_fn, agreement_gamma)) {
            fns_to_export = c()
        } else {
            stop("lda_stability: unknown agreement function")
        }
        
        ## Calculate agreement scores
        agree_scores = foreach(si = sis, 
                               .combine = 'c', .verbose = verbose, 
                               .packages = c('tidytext', 'dplyr', 'igraph', 
                                             'stringr'), 
                               .export = fns_to_export
        ) %dopar% 
        {
            agreement_fn(s0, si, t)
        }
        
        agree_scores
    }
    ## Return the results in a tidy dataframe
    agreement_scores = agree_scores %>% 
        as_tibble %>%
        gather(k, agreement, factor_key = FALSE) %>%
        mutate(k = {str_replace(k, 'result.', '') %>%
                as.numeric %>% k_range[.]})
    return(agreement_scores)
}
