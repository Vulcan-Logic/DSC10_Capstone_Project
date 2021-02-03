library(tictoc)
# function start
startProcess2Unlimited<-function(){
  #read the data file
  dat<-readtext("./data/Udata.txt")
  badWords<-read.delim2(file="badWords.txt",header=F,sep='\n')
  corp<-corpus(dat)
  tic()
  show("constructing tokens")
  tokenz<-tokens(corp, remove_numbers=T, remove_punct=T, remove_symbols=T, 
                 remove_url=T,remove_separators=T, remove_twitter=T)
  toc()
  show(tokenz)
  #make tokens lower case
  tokenz<-tokens_tolower(tokenz)
  #remove abuse words
  tokenz<-tokens_remove(tokenz,badWords$V1,padding=F)
  #make tokenz dfm
  tic()
  show("making tokens dfm")
  frqTblTokenz<-dfm(tokenz)
  toc()
  object.size(frqTblTokenz)
  save(frqTblTokenz,file=paste0(prefix,'frqTblTokenz.rda'))
  rm(frqTblTokenz)
  #make bigrams
  tic()
  show("making bigrams")
  biGms<-tokens_ngrams(tokenz)
  toc()
  object.size(biGms)
  save(biGms,file=paste0(prefix,'biGms.rda'))
  rm(biGms)
  #make trigrams
  tic()
  show("making trigrams")
  Trigram<-tokens_ngrams(tokenz,n=3)
  toc()
  object.size(Trigram)
  save(Trigram,file=paste0(prefix,'Trigram.rda'))
  rm(Trigram)
  #make quadgrams
  show("making quadgrams")
  tic()
  Quadgram<-tokens_ngrams(tokenz,n=4)
  toc()
  object.size(Quadgram)
  save(Quadgram,file=paste0(prefix,'Quadgram.rda'))
  rm(Quadgram)
}


