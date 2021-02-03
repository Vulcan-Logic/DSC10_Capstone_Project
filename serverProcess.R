# function start
library(readtext)
library(quanteda)
library(stopwords)
startProcess<-function(){
  row1<-c("blog","./data/en_US/en_US.blogs.txt")
  row2<-c("news","./data/en_US/en_US.news.txt")
  row3<-c("twitter","./data/en_US/en_US.twitter.txt")
  prefix<-"./data/"
  filesData<-data.frame(rbind(row1,row2,row3))
  writeCon<-file("./data/data.txt",open="wt")
  for(fileCtr in 1:dim(filesData)[1]){
    show("processing file")
    show(fileCtr)
    readCon<-file(as.character(filesData[fileCtr,2]),"rt")
    # repeat the following loop for each line in the file, i.e.
    # each document in the corpus stored in this file
    repeat{
      rawLnInp<-readLines(readCon,1,encoding="UTF-8",skipNul = TRUE)
      if (length(rawLnInp)==0) {break}
      else{
        if (runif(1,0,1)<0.50){
          writeLines(rawLnInp,writeCon)
        }
      }
    }
    close(readCon)
  }
  close(writeCon)
  dat<-readtext("./data/data.txt")
  badWords<-read.delim2(file="badWords.txt",header=F,sep='\n')
  corp<-corpus(dat)
  show("processing tokens")
  tokenz<-tokens(corp, remove_numbers=T, remove_punct=T, remove_symbols=T, 
                 remove_url=T,remove_separators=T, remove_twitter=T)
  #make tokens lower case
  tokenz<-tokens_tolower(tokenz)
  #remove abuse & stop words
  tokenz<-tokens_remove(tokenz,badWords$V1,padding=F)
  tokenz<-tokens_remove(tokenz,stopwords("english"),padding=F)
  #make tokenz dfm
  frqTblTokenz<-dfm(tokenz)
  #make bigrams
  show("processing biGrams")
  biGms<-dfm(tokens_ngrams(tokenz))
  #make trigrams
  show("processing Trigrams")
  Trigram<-dfm(tokens_ngrams(tokenz,n=3))
  #make quadgrams
  show("processing quadgrams")
  Quadgram<-dfm(tokens_ngrams(tokenz,n=4))
  save(frqTblTokenz,file=paste0(prefix,'frqTblTokenz.rda'))
  save(biGms,file=paste0(prefix,'biGms.rda'))
  save(Trigram,file=paste0(prefix,'Trigram.rda'))
  save(Quadgram,file=paste0(prefix,'Quadgram.rda'))
}


