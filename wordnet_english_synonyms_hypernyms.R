library(tidyverse)
library(textstem)
library(wordnet)
library(igraph)

setDict('wn3.1.dict/dict/')

# Source of list of English words: 
# Balota, D.A., Yap, M.J., Cortese, M.J., Hutchison, K.A., Kessler, B., Loftis, 
# B., Neely, J.H., Nelson, D.L., Simpson, G.B., & Treiman, R. (2007). The 
# English Lexicon Project. Behavior Research Methods, 39, 445-459.
# 
# Query: POS, The complete ELP Lexicon
list = read_csv('Items.csv') %>%
  separate_rows(POS, sep = '\\|') %>%
  filter(POS %in% c('NN', 'VB', 'RB', 'JJ')) %>%
  mutate(lemma = lemmatize_words(Word)) %>%
  distinct(lemma, POS, .keep_all = T) %>%
  arrange(-Freq_HAL)

# 56124
nrow(list)

for (i in 1:56124) {
  lemma = list$lemma[i]
  pos = list$POS[i]
  pos_f = ifelse(pos == 'NN', 'NOUN',
                 ifelse(pos == 'VB', 'VERB',
                        ifelse(pos == 'JJ', 'ADJECTIVE',
                               'ADVERB')))
  
  synonyms = synonyms(lemma, pos_f) %>%
    paste0(collapse = '|')
  
  hypernyms = tryCatch({
    filter = getTermFilter('ExactMatchFilter', lemma, T)
    terms = getIndexTerms(pos_f, 1, filter)
    synsets = getSynsets(terms[[1]])
    
    sapply(synsets, function(s) {
      relatedSynsets = getRelatedSynsets(s, '@')
      
      sapply(relatedSynsets, getWord)
    }) %>%
      unlist() %>%
      unique() %>%
      paste0(collapse = '|')
  }, error = function(e) {
    return(NULL)
  })
  
  write_file('\n', 'english_synonyms_hypernyms.csv', append = T)
  write_file(paste(lemma, pos, synonyms, hypernyms, sep = ','),
             'english_synonyms_hypernyms.csv',
             append = T)
}

results = read_csv('english_synonyms_hypernyms.csv') %>%
  filter(!(is.na(synonyms) & is.na(hypernyms)))

write_csv(results, 'english_synonyms_hypernyms.csv')

# Build -nyms networks

nyms = read_csv('english_synonyms_hypernyms.csv',
                show_col_types = F) %>%
  pivot_longer(c(synonyms, hypernyms), 
               names_to = 'relation', 
               values_to = 'nyms') %>%
  select(-relation) %>%
  filter(!is.na(nyms)) %>%
  separate_rows(nyms, sep = '\\|') %>%
  filter((!nyms %>% str_detect('\\s')) & 
           lemma != nyms) %>%
  unique() %>%
  ungroup()

nyms_network_lemmas = nyms %>%
  select(lemma, nyms) %>%
  mutate(lemma = lemma %>% str_to_lower(),
         nyms = nyms %>% str_to_lower()) %>%
  unique() %>%
  filter(lemma != '' & nyms != '' & lemma != nyms) %>%
  as.matrix() %>% 
  graph_from_edgelist(directed = F) %>%
  simplify()

V(nyms_network_lemmas)$id = V(nyms_network_lemmas)$name

nyms_network_lemmas %>%
  igraph::as_data_frame('vertices') %>%
  write_csv('network_nyms_lemmas_vertices.csv')

nyms_network_lemmas %>%
  igraph::as_data_frame('edges') %>%
  rename(Source = from,
         Target = to) %>%
  mutate(type = 'Undirected') %>%
  write_csv('network_nyms_lemmas_edges.csv')

nyms_network_lemmas %>%
  write_graph('nyms_lemmas.net', 'pajek')

nyms_network_stems = nyms %>%
  select(lemma, nyms) %>%
  mutate(lemma = lemma %>% str_to_lower() %>% stem_words(),
         nyms = nyms %>% str_to_lower() %>% stem_words()) %>%
  unique() %>%
  filter(lemma != '' & nyms != '' & lemma != nyms) %>%
  as.matrix() %>% 
  graph_from_edgelist(directed = F) %>%
  simplify()

V(nyms_network_stems)$id = V(nyms_network_stems)$name

nyms_network_stems %>%
  igraph::as_data_frame('vertices') %>%
  write_csv('network_nyms_stems_vertices.csv')

nyms_network_stems %>%
  igraph::as_data_frame('edges') %>%
  rename(Source = from,
         Target = to) %>%
  mutate(type = 'Undirected') %>%
  write_csv('network_nyms_stems_edges.csv')

nyms_network_stems %>%
  write_graph('nyms_stems.net', 'pajek')
