#aws machine code - to make tokens/bigrams/trigrams/quadgrams and store 
#them in appropriate structures
library(quanteda)
library(readtext)
library(dplyr)
prefix<-'~/programming/datasc/DSC10/data/'
source('computeQB11.R')
source('computeQTrigram.R')
source('computeQQuadgram.R')
source('computeMC.R')
dat<-readtext(paste0(prefix,'fraction.txt'))
corp<-corpus(dat)
tokenz<-tokens(corp, remove_numbers=T, remove_punct=T, remove_symbols=T, 
               remove_url=T,remove_separators=T, remove_twitter=T)
#make tokens lower case
tokenz<-tokens_tolower(tokenz)
#remove abuse words
#make tokenz dfm
frqTblTokenz<-dfm(tokenz)
#make bigrams
biGms<-tokens_ngrams(tokenz)
#make trigrams
Trigram<-tokens_ngrams(tokenz,n=3)
#make quadgrams
Quadgram<-tokens_ngrams(tokenz,n=4)
#total no of words
totalTokens<-rowSums(frqTblTokenz)
#list of words
listTokens<-featnames(frqTblTokenz)
#save required structures in data files
save(tokenz,file=paste0(prefix,'tokenz.rda'))
save(biGms,file=paste0(prefix,'biGms.rda'))
save(Trigram,file=paste0(prefix,'Trigram.rda'))
save(Quadgram,file=paste0(prefix,'Quadgram.rda'))

word1<-"the"
word2<-"international"
word3<-"recognition"
returnList<-computeQQuadgram(word1,word2,word3,frqTblTokenz,totalTokens,listTokens)
returnStatus<-returnList[[1]]
returnVector<-returnList[[2]]
finalFrame<-data.frame(cbind(listTokens,returnVector),stringsAsFactors = F)
names(finalFrame)<-c('word','prob')
finalFrame$prob<-as.numeric(finalFrame$prob)
finalFrame1<-finalFrame %>% arrange(desc(prob))
show(returnStatus)
show(finalFrame1[1:5,])
show(finalFrame1[which(finalFrame1$prob==(max(finalFrame1$prob))),1])
