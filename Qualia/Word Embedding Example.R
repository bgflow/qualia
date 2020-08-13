library(wordnet)
setDict("C:/Program Files (x86)/WordNet/2.1/dict")

filter <- getTermFilter("ExactMatchFilter", "screen", TRUE)
terms <- getIndexTerms("NOUN", 100, filter)
synsets <- getSynsets(terms[[1]])
related <- getRelatedSynsets(synsets[[1]], "!")
sapply(related, getWord)

terms
synsets
?getIndexTerms

?getRelatedSynsets()
synsets[[1]]
