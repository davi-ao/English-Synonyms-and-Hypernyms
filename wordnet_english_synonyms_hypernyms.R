library(tidyverse)
library(wordnet)

setDict('wn3.1.dict/dict/')

# Source of list of english words: 
# Balota, D.A., Yap, M.J., Cortese, M.J., Hutchison, K.A., Kessler, B., Loftis, 
# B., Neely, J.H., Nelson, D.L., Simpson, G.B., & Treiman, R. (2007). The 
# English Lexicon Project. Behavior Research Methods, 39, 445-459.
# 
# Query: POS, The complete ELP Lexicon
list = read_csv('Items.csv') %>%
  separate_rows(POS, sep = '\\|') %>%
  filter(POS %in% c('NN', 'VB', 'RB', 'JJ')) %>%
  arrange(-Freq_HAL)

# 87708
nrow(list)

for (i in 1:1) {
  i = 1
  pos_f = ifelse(list$POS[i] == 'NN', 'NOUN',
                 ifelse(list$POS[i] == 'VB', 'VERB',
                        ifelse(list$POS[i] == 'JJ', 'ADJECTIVE',
                               'ADVERB')))
  
  synonyms = synonyms(list$Word[i], pos_f) %>%
    paste0(collapse = '|')
  
  hypernyms = tryCatch({
    filter = getTermFilter('ExactMatchFilter', list$Word[i], T)
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
    return(NA)
  })
}