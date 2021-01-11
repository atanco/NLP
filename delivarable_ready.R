args<-commandArgs(TRUE)

library(tm)
library(NLP)
library(openNLP) 
library(openNLPmodels.en)

library(sylly.en)

#setwd("/home/atanczos/atanczos/EIT/IntelligentSystems/Unit5_NLP/handsons")

printMap <- function(map) {
  for (i in 1:length(names(map))) {
  
    print(c(names(map)[i],map[[names(map)[i]]]))
  }
  }

isthereNext <- function(word, map) {
  hyphen <- sapply(strsplit(hyphenText(hyphen(word, hyph.pattern="en"))[["word"]],"-"), tail, 1)
  if (length(map[[hyphen]]) > 0){
    TRUE
  } else  FALSE
}

#read the source into the corpus
source.pos = DirSource("txt_sentoken/pos", encoding = "UTF-8")
corpus = Corpus(source.pos)
#preprocess the data: remove numbers, punctation, stemming
tdm = TermDocumentMatrix(corpus,
                         control=list(stopwords = F,
                                      removePunctuation = T, 
                                      removeNumbers = T,
                                      stemming = T))


#make a frequency matrix from the TermDocumentMatrix
freq=rowSums(as.matrix(tdm))


#hyphenize the data
hyph.txt.en <- hyphen(names(freq), hyph.pattern="en")
hy <- hyphenText(hyph.txt.en)

hy$firstSyllable <- sapply(strsplit(hy$word,"-"), `[`, 1)

#build a hash map
mapConstructed <- new.env(hash=T)
for(i in 1:length(hy$word)) {
  
  if (is.null(mapConstructed[[hy$firstSyllable[i]]])) {
    is <- c(hy$word[i])
    mapConstructed[[hy$firstSyllable[i]]] <- c(hy$word[i])
  }
  else {
    mapConstructed[[hy$firstSyllable[i]]] <- c(mapConstructed[[hy$firstSyllable[i]]], hy$word[i])
  }
}

minElements = args[2]
if (is.na(args[2])) {
  minElements = 3  
}
maxRuns = args[3]
if (is.na(args[3])) {
  maxRuns = 2
}

chain <- c()
runs = 0

while (length(chain) < minElements && runs != maxRuns) {
  chain <- c()
  
  
  # this way, we don't need to reconstruct the map in every loop (we would need the reconstruction because of the removings)
  map <- as.environment(as.list(mapConstructed, all.names=TRUE))

  #Choose first word: input argument
  nextWord <- args[1]
  if (is.na(args[1])) {
    nextWord <- sample(names(freq), 1)  
  }
  hyph.txt.en <- hyphen(nextWord, hyph.pattern="en")
  nextHyphen <- sapply(strsplit(hyphenText(hyph.txt.en)[["word"]],"-"), tail, 1)
  
  found <- TRUE
  chain <- c(nextWord)

  #if the input given by the user is in the corpus, we have to remove it otherwise it might appear twice in the chain
  if (nextWord %in% map[[nextHyphen]]) {
    map[[nextHyphen]] <- map[[nextHyphen]][!map[[nextHyphen]] %in%  nextWord]
    
  }

  while (found) {
    numberOfPossibilities = length(map[[nextHyphen]])
    
    if (numberOfPossibilities > 0) {
      goodOnes <- c()
      i = 1
      for (possibles in map[[nextHyphen]]) {
        if (isthereNext(possibles, map)) {
          goodOnes <- c(goodOnes, possibles)
        }
      }
      if (is.null(goodOnes)) {
        break
      }
      
      nextWord <- sample(goodOnes, 1)
      #remove it
      map[[nextHyphen]] <- map[[nextHyphen]][!map[[nextHyphen]] %in%  nextWord]
    
      nextHyphen <- sapply(strsplit(hyphenText(hyphen(nextWord, hyph.pattern="en"))[["word"]],"-"), tail, 1)   
      chain <- c(chain, nextWord)
    
    } else {
      found <- FALSE
    }
  }
  runs = runs + 1
}
if (length(chain) >= minElements) {
  print(paste("Here is the chain with startingword ", args[1], " and with at least ", minElements, "elements (", length(chain), ")"))
  print(chain)
} else {
  print(paste("cannot find chain with ", minElements, " long"))
}

