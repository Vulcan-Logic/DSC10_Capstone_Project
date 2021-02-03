# function start
startProcessUnlimited<-function(){
  row1<-c("blog","./data/en_US/en_US.blogs.txt")
  row2<-c("news","./data/en_US/en_US.news.txt")
  row3<-c("twitter","./data/en_US/en_US.twitter.txt")
  filesData<-data.frame(rbind(row1,row2,row3))
  writeCon<-file("./data/Udata.txt",open="wt")
  for(fileCtr in 1:dim(filesData)[1]){
    readCon<-file(as.character(filesData[fileCtr,2]),"rt")
    # repeat the following loop for each line in the file, i.e.
    # each document in the corpus stored in this file
    repeat{
      rawLnInp<-readLines(readCon,1,encoding="UTF-8",skipNul = TRUE)
      if (length(rawLnInp)==0) {break}
      else{
       writeLines(rawLnInp,writeCon)
      }
    }
    close(readCon)
  }
  close(writeCon)
  #read the data file
  dat<-readtext("./data/Udata.txt")
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
  save(frqTblTokenz,file=paste0(prefix,'frqTblTokenz.rda'))
  rm(frqTblTokenz)
  #make bigrams
  biGms<-tokens_ngrams(tokenz)
  save(biGms,file=paste0(prefix,'biGms.rda'))
  rm(biGms)
  #make trigrams
  Trigram<-tokens_ngrams(tokenz,n=3)
  save(Trigram,file=paste0(prefix,'Trigram.rda'))
  rm(Trigram)
  #make quadgrams
  Quadgram<-tokens_ngrams(tokenz,n=4)
  save(Quadgram,file=paste0(prefix,'Quadgram.rda'))
  rm(Trigram)
}


