## This file defines a function LDA_stability to implement 
## the algorithm described in https://arxiv.org/pdf/1404.4606v3.pdf



library(tidyverse)
    library(cowplot)
library(igraph)
library(reshape2)
library(SnowballC)
library(stringr)
library(tidytext)
library(topicmodels)
library(tikzDevice)

library(foreach)
library(doParallel)
cl = makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()

## Load the data
load('cleaned awards.Rdata')
dataf = dataf %>% 
    transmute(project.id, 
              ## Handle some encoding errors
              abstract = stringi::stri_enc_toutf8(Abstract, 
                                                  validate = TRUE)) %>%
    ## Select the longest abstract for each project
    filter(!duplicated(.)) %>%
    mutate(length = str_length(abstract)) %>%
    group_by(project.id) %>%
    slice(which.max(length)) %>%
    ungroup() %>%
    select(-length)

## Count tokens across project abstracts
token_counts = dataf %>%
    unnest_tokens(token, abstract) %>% 
    group_by(project.id, token) %>%
    summarize(n = n()) %>%
    ## Remove stopwords
    anti_join(stop_words, by = c('token' = 'word')) %>%
    ## Remove numbers
    filter(!str_detect(token, '^[0-9]')) %>%
    ungroup()
## Remove terms with low IDF
low_idf = token_counts %>% 
    bind_tf_idf(token, project.id, n) %>% 
    filter(tf_idf < quantile(tf_idf, probs = .05)) %>% 
    .$token %>%
    unique()
token_counts = token_counts %>% filter(!(token %in% low_idf))
length(unique(token_counts$token))

## Cast into a document-term matrix
projects_dtm = cast_dtm(token_counts, project.id, token, n)


## Using the stability method from https://arxiv.org/pdf/1404.4606v3.pdf

## 1. Randomly generate τ samples of the data set, each containing β ×n documents.
## Number of documents
n = nrow(dataf)
## Fraction to include in each sample
beta = .8
## Total number of samples
tau = 25

## Draw sample corpora, and convert each to DTM
set.seed(54321)
samples = lapply(1:tau, function (x) sample(1:n, .8 * n)) %>%
    lapply(function (x) filter(token_counts, project.id %in% x)) %>%
    lapply(function (x) cast_dtm(x, project.id, token, n))

## 2. For each value of k ∈ [kmin, kmax] :
## t is the maximum number of terms in each ranked list
t = 20

## k is the range of topic counts
k_range = 2:12
agree_scores = foreach(k = k_range, 
                       .combine = 'cbind', .verbose = TRUE, 
        .packages = c('tidytext', 'topicmodels', 'stringr',
                      'dplyr', 'igraph')) %dopar% {
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
    jaccard = function (x, y) {
        ## Calculate Jaccard similarities; 1 = identical sets
        length(intersect(x, y)) / length(union(x, y))
    }
    avg_jaccard = function (x, y) {
        ## Average Jaccard similarities over partial ranked sets
        t = min(length(x), length(y))
        gammas = sapply(1:t, function (d) jaccard(head(x, d), head(y, d)))
        aj = 1/t * sum(gammas)
        return(aj)
    }
    
    extract_rankset = function(tidy_lda, t) {
        ## Take the results of LDA and produce a list of ranked sets
        tidy_lda %>%
            group_by(topic) %>%
            top_n(t, beta) %>%
            ungroup() %>%
            arrange(topic, -beta) %>%
            split(.$topic) %>%
            lapply(function (x) x$term)
    }
    
    s0_rankset = extract_rankset(s0, t)
    si_ranksets = lapply(si, function (x) extract_rankset(x, t))
    
    ## This version worked, but is too brute-force to be useful
    # agreement = function(s1_rankset, si_rankset) {
    #     ## Calculate the matrix AJ, as in the right panel of Fig. 1 in the paper
    #     aj_matrix = expand.grid(s0_rankset, si_rankset) %>% 
    #         mutate(aj = {Map(avg_jaccard, Var1, Var2) %>% unlist}) %>% 
    #         mutate(Var1 = names(Var1), Var2 = names(Var2)) %>% 
    #         dcast(Var1 ~ Var2)
    #     ## Every permutation of k indices
    #     perms = combinat::permn(k)
    # 
    #     ## Average the average Jaccard score across each permutation
    #     ## See Fig. 1 in the paper
    #     perm_total = function (aj_matrix, perm) {
    #         i = cbind(1:nrow(aj_matrix), perm + 1)
    #         sum(as.numeric(aj_matrix[i])) / nrow(aj_matrix)
    #     }
    #     perm_totals = sapply(perms, function (perm) perm_total(aj_matrix, perm))
    #     
    #     ## The agree score is the maximum permutation score
    #     agree = max(perm_totals)
    #     return(agree)
    # }
    
    agreement = function (s0_rankset, si_rankset) {
        aj_edgelist = expand.grid(s0_rankset, si_rankset) %>% 
            mutate(aj = {Map(avg_jaccard, Var1, Var2) %>% unlist}) %>% 
            mutate(Var1 = paste('s0', names(Var1), sep = '.'),
                   Var2 = paste('si', names(Var2), sep = '.')) %>%
            as.matrix
        aj_graph = graph_from_edgelist(aj_edgelist[,1:2])
        E(aj_graph)$weight = aj_edgelist[,3]
        V(aj_graph)$type = str_detect(V(aj_graph)$name, 's0')
        
        max_bipartite_match(aj_graph)$matching_weight / k
    }
    
    agree_scores = sapply(si_ranksets, function (x) agreement(s0_rankset, x))
    agree_scores
}

agreement_scores = agree_scores %>% 
    as_tibble %>%
    melt %>%
    mutate(variable = {str_replace(variable, 'result.', '') %>%
                        as.numeric %>% k_range[.]})

tikz(height = 5, width = 7,
    file = 'lda.tex', standAlone = TRUE)
ggplot(agreement_scores, aes(variable, value)) + 
    geom_point() + 
    stat_summary(geom = 'line') +
    scale_x_continuous(name = 'k', breaks = k_range) +
    ylab('agreement score')
dev.off()


## One run through LDA
# projects_lda = LDA(projects_dtm, k = 2, control = list(seed = 42))
# projects_lda_td = tidy(projects_lda)
# projects_lda_td %>%
#     group_by(topic) %>%
#     top_n(10, beta) %>%
#     ungroup() %>%
#     arrange(topic, -beta) %>%
#     View
