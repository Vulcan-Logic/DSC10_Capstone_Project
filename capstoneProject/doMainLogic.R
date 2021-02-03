doMainLogic<-function(inputText,frqTblTokenz,biGms,Trigram,Quadgram){
  #total no of words
  totalTokens<-rowSums(frqTblTokenz)
  #list of words
  listTokens<-featnames(frqTblTokenz)
  #read in list of bad words
  badWords<-read.delim2(file="badWords.txt",header=F,sep='\n')
  #tokenize input phrase
  inputWordsTokens<-tokens(inputText,remove_numbers=T, remove_punct=T, 
                           remove_symbols=T, 
                           remove_url=T,remove_separators=T, remove_twitter=T)
  #convert to lower case
  inputWordsTokens<-tokens_tolower(inputWordsTokens)
  #remove bad words
  inputWordsTokens<-tokens_remove(inputWordsTokens,badWords$V1,padding=F)
  inputWordsTokens<-tokens_remove(inputWordsTokens,
                                  pattern="[~`!@#$%^&*_{}()+0-9\\[\\]|]",
                                  valuetype="regex",padding=F)
  #check length of input phrase 
  inputWordsLength<-length(inputWordsTokens[[1]])
  #check length of input phrase is not 0
  if (inputWordsLength==0){
    return(list("No valid input phrase",NULL,NULL,NULL,NULL,NULL,0,0,0,0,0))
  }
  #check length >= 3 words  
  if (inputWordsLength>=3){
    word3<-inputWordsTokens[[1]][inputWordsLength]
    word2<-inputWordsTokens[[1]][inputWordsLength-1]
    word1<-inputWordsTokens[[1]][inputWordsLength-2]
    # branch off to other function here
    returnList<-computeQQuadgram(word1,word2,word3,frqTblTokenz,Quadgram, 
                                 Trigram, biGms,
                                 totalTokens,
                                 listTokens)
    returnStatus<-returnList[[1]]
    returnVector<-returnList[[2]]
    finalFrame<-data.frame(cbind(listTokens,returnVector),
                           stringsAsFactors = F)
    names(finalFrame)<-c('word','prob')
    finalFrame$prob<-as.numeric(finalFrame$prob)
    finalFrame1<-finalFrame %>% arrange(desc(prob))
    rWord1<-finalFrame1[1,1]
    rWord2<-finalFrame1[2,1]
    rWord3<-finalFrame1[3,1]
    rWord4<-finalFrame1[4,1]
    rWord5<-finalFrame1[5,1]
    prob1<-finalFrame1[1,2]
    prob2<-finalFrame1[2,2]
    prob3<-finalFrame1[3,2]
    prob4<-finalFrame1[4,2]
    prob5<-finalFrame1[5,2]
    if(!is.null(returnStatus)){
      if (returnStatus=="No bigrams with word found"){
        return(list("NIL",
                    rWord1,rWord2,rWord3,rWord4,rWord5,prob1,prob2,prob3,prob4,
                    prob5))
      }
      else{
        #return status is quadgram not found
        return(list(NULL,rWord1,rWord2,rWord3,rWord4,rWord5,prob1,prob2,prob3,
                    prob4,prob5))
      }
    }
    else{
      #returnStatus is NULL
      return(list(NULL,rWord1,rWord2,rWord3,rWord4,rWord5,prob1,prob2,prob3,
                  prob4,prob5))
    }
  }
  #check length >= 2 words
  if (inputWordsLength==2){
    word3<-inputWordsTokens[[1]][inputWordsLength]
    word2<-inputWordsTokens[[1]][inputWordsLength-1]
    # branch off to other function here
    returnList<-computeQTrigram(word2,word3,frqTblTokenz, Trigram, biGms,
                                totalTokens,
                                listTokens)
    returnStatus<-returnList[[1]]
    returnVector<-returnList[[2]]
    finalFrame<-data.frame(cbind(listTokens,returnVector),
                           stringsAsFactors = F)
    names(finalFrame)<-c('word','prob')
    finalFrame$prob<-as.numeric(finalFrame$prob)
    finalFrame1<-finalFrame %>% arrange(desc(prob))
    names(finalFrame)<-c('word','prob')
    finalFrame$prob<-as.numeric(finalFrame$prob)
    finalFrame1<-finalFrame %>% arrange(desc(prob))
    rWord1<-finalFrame1[1,1]
    rWord2<-finalFrame1[2,1]
    rWord3<-finalFrame1[3,1]
    rWord4<-finalFrame1[4,1]
    rWord5<-finalFrame1[5,1]
    prob1<-finalFrame1[1,2]
    prob2<-finalFrame1[2,2]
    prob3<-finalFrame1[3,2]
    prob4<-finalFrame1[4,2]
    prob5<-finalFrame1[5,2]
    if(!is.null(returnStatus)){
      if (returnStatus=="No bigrams with word found"){
        return(list("NIL",
                    rWord1,rWord2,rWord3,rWord4,rWord5,prob1,prob2,prob3,prob4,
                    prob5))
      }
      else{
        return(list(NULL,rWord1,rWord2,rWord3,rWord4,rWord5,prob1,prob2,prob3,
                    prob4,prob5))
      }
    }
    else{
      return(list(NULL,rWord1,rWord2,rWord3,rWord4,rWord5,prob1,prob2,prob3,
                  prob4,prob5))
    }
  }
  #check length >= 1 word
  if (inputWordsLength==1){
    word3<-inputWordsTokens[[1]][inputWordsLength]
    # branch off to other function here
    returnList<-computeQBigram(word3,frqTblTokenz,biGms,totalTokens,listTokens)
    returnStatus<-returnList[[1]]
    returnVector<-returnList[[2]]
    finalFrame<-data.frame(cbind(listTokens,returnVector),
                           stringsAsFactors = F)
    names(finalFrame)<-c('word','prob')
    finalFrame$prob<-as.numeric(finalFrame$prob)
    finalFrame1<-finalFrame %>% arrange(desc(prob))
    names(finalFrame)<-c('word','prob')
    finalFrame$prob<-as.numeric(finalFrame$prob)
    finalFrame1<-finalFrame %>% arrange(desc(prob))
    rWord1<-finalFrame1[1,1]
    rWord2<-finalFrame1[2,1]
    rWord3<-finalFrame1[3,1]
    rWord4<-finalFrame1[4,1]
    rWord5<-finalFrame1[5,1]
    prob1<-finalFrame1[1,2]
    prob2<-finalFrame1[2,2]
    prob3<-finalFrame1[3,2]
    prob4<-finalFrame1[4,2]
    prob5<-finalFrame1[5,2]
    if(!is.null(returnStatus)){
      if (returnStatus=="No bigrams with word found"){
        return(list("NIL",
                    rWord1,rWord2,rWord3,rWord4,rWord5,prob1,prob2,prob3,prob4,
                    prob5))
      }
      else{
        return(list(NULL,rWord1,rWord2,rWord3,rWord4,rWord5,prob1,prob2,prob3,
                    prob4,prob5))
      }
    }
    else{
      return(list(NULL,rWord1,rWord2,rWord3,rWord4,rWord5,prob1,prob2,prob3,
                  prob4,prob5))
    }
  }
}
