#aws machine code - to make tokens/bigrams/trigrams/quadgrams and store 
#them in appropriate structures
library(quanteda)
library(readtext)
prefix<-'./data/'
# in server preprocess you will need to combine a lot of files 
# to arrive at the final corpus or design a proedure to knock out data
dat<-readtext(paste0(prefix,'fraction.txt'))
badWords<-read.delim2(file="badWords.txt",header=F,sep='\n')
corp<-corpus(dat)
tokenz<-tokens(corp, remove_numbers=T, remove_punct=T, remove_symbols=T, 
               remove_url=T,remove_separators=T, remove_twitter=T)
#make tokens lower case
tokenz<-tokens_tolower(tokenz)
#remove abuse words
tokenz<-tokens_remove(tokenz,badWords$V1,padding=F)
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
save(frqTblTokenz,file=paste0(prefix,'frqTblTokenz.rda'))
save(biGms,file=paste0(prefix,'biGms.rda'))
save(Trigram,file=paste0(prefix,'Trigram.rda'))
save(Quadgram,file=paste0(prefix,'Quadgram.rda'))

