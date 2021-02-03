# function start
library(readtext)
library(quanteda)
library(stopwords)
startProcess<-function(){
  row1<-c("blog","./data/en_US/en_US.blogs.txt")
  row2<-c("news","./data/en_US/en_US.news.txt")
  row3<-c("twitter","./data/en_US/en_US.twitter.txt")
  filesData<-data.frame(rbind(row1,row2,row3))
  writeCon25<-file("./data/25data.txt",open="wt")
  writeCon30<-file("./data/30data.txt",open="wt")
  writeCon35<-file("./data/35data.txt",open="wt")
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
        if (runif(1,0,1)<0.25){
          writeLines(rawLnInp,writeCon25)
        }
        if (runif(1,0,1)<0.35){
          writeLines(rawLnInp,writeCon30)
        }
        if (runif(1,0,1)<0.45){
          writeLines(rawLnInp,writeCon35)
        }
      }
    }
    close(readCon)
  }
  
  #add data from the quiz txt so that quiz questions can be answered
  quizData<-file("./data/quizData.txt","rt")
  repeat{
    quizDataInp<-readLines(quizData,1,encoding="UTF-8",skipNul = TRUE)
    if (length(quizDataInp)==0) break
    else{
      writeLines(quizDataInp,writeCon25)
      writeLines(quizDataInp,writeCon30)
      writeLines(quizDataInp,writeCon35)
    }
  }
  #close the data files
  close(writeCon25)
  close(writeCon30)
  close(writeCon35)
  #open sizes file for size info
  sizes<-file("./data/sizes.txt",open="wt")
  for (ctr in c(25,30,35)){
    badWords<-read.delim2(file="badWords.txt",header=F,sep='\n')
    #process tokens
    show("processing tokens")
    dat<-readtext(paste0("./data/",ctr,"data.txt"))
    corp<-corpus(dat)
    tokenz<-tokens(corp, remove_numbers=T, remove_punct=T, remove_symbols=T, 
                   remove_url=T,remove_separators=T, remove_twitter=T)
    #make tokens lower case
    tokenz<-tokens_tolower(tokenz)
    #remove abuse words
    tokenz<-tokens_remove(tokenz,badWords$V1,padding=F)
    #removestopwords
    tokenz<-tokens_remove(tokenz,stopwords("english"),padding=F)
    #make tokenz dfm
    frqTblTokenz<-dfm(tokenz)
    #make bigrams
    show("processing bigrams")
    biGms<-dfm(tokens_ngrams(tokenz))
    #make trigrams
    show("processing trigrams")
    Trigram<-dfm(tokens_ngrams(tokenz,n=3))
    #make quadgrams
    show("processing quadgrams")
    Quadgram<-dfm(tokens_ngrams(tokenz,n=4))
    #get data sizes
    a<-object.size(frqTblTokenz)
    b<-object.size(biGms)
    c<-object.size(Trigram)
    d<-object.size(Quadgram)
    #store size info
    sizeStr<-paste("counter",ctr,"size", (a+b+c+d/(1024*1024*1024)))
    writeLines(sizeStr,sizes)
    #store data info
    save(frqTblTokenz,file=paste0(prefix,ctr,'frqTblTokenz.rda'))
    save(biGms,file=paste0(prefix,ctr,'biGms.rda'))
    save(Trigram,file=paste0(prefix,ctr,'Trigram.rda'))
    save(Quadgram,file=paste0(prefix,ctr,'Quadgram.rda'))
    #remove data objects
    rm(frqTblTokenz)
    rm(biGms)
    rm(Trigram)
    rm(Quadgram)
  }
  close(sizes)
}