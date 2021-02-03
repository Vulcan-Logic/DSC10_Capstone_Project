# function start
startProcess1Unlimited<-function(){
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
        if (runif(1,0,1)<1){
          writeLines(rawLnInp,writeCon)
        }
      }
    }
    close(readCon)
  }
  close(writeCon)
  #read the data file
}


