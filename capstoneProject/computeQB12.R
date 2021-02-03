computeQBigram<-function(word3,frqTblTokenz,biGms,totalTokens,listTokens){
  searchPtn<-paste0("(^|[^a-z])(",word3,"_)+")
  #subset known bigrams from the bigram tokens
  frqTblKnownBiGms<-dfm_select(biGms,pattern=searchPtn,selection="keep", 
                               valuetype="regex")
  #set return Status Flag
  returnStatusFlag<-NULL
  #get no of total known bigrams
  totalKnownBiGms<-rowSums(frqTblKnownBiGms)
  #setup returnVector1
  returnVector1<-rep(0,length(listTokens))
  # use frqTblKnownBiGms
  biGmsFrqVec<-as.vector(frqTblKnownBiGms[1,])
  if (length(biGmsFrqVec)<2){
    #length 0 means that no combination of given word and another word has been 
    #found in the bigrams list, we need to use returnVector2 itself for all 
    #probabilities. we also need to set a return flag value to let calling
    #program know 
    if (length(biGmsFrqVec)==0) {
      PA1<-0
      returnStatusFlag<-"No bigrams with word found"
      returnVector2<-as.vector(frqTblTokenz[1,])
      returnVector2<-returnVector2/totalTokens
      }
    #for length 1 we have no option other than manual discounting
    #here we use a factor of 0.5 to simulate a coin toss
    if (length(biGmsFrqVec==1)){
      #split bigrams up 
      splitBiGmsList<-strsplit(featnames(frqTblKnownBiGms),split="_")
      # a vector to store the known trailing words indexes
      #process the entire split words list using a for loop to store words
      #get trailing known word
      tWord<-splitBiGmsList[[1]][2]
      #get index of trailing known word
      indx<-which(listTokens==tWord)
      #get count of known trailing word 
      countBC2<-as.vector(frqTblTokenz[1,indx])
      # compute the count for B2
      countB2<-totalTokens-countBC2
      #compute Qml for B2 
      #in returnVector2 you will store all the probabilities of the unknown words 
      # for which we are using katz backoff.
      returnVector2<-as.vector(frqTblTokenz[1,])
      #compute probabilities and store in returnVector2 after using counts
      returnVector2<-returnVector2/countB2
      #set probabilities of known trailing word to 0 in returnVector2
      returnVector2[indx]<-0
      #manual discount this probability to a high value say 0.5
      PA1<-0.5
      returnVector1[indx]<-as.vector(frqTblKnownBiGms[1,])
      returnVector1<-returnVector1/totalKnownBiGms
      returnVector1<-returnVector1*PA1
    }
  }
  else {
    # length of the vector is more than 2 and 
    # possible to compute simple good turing discounting 
    # split bigrams up 
     words2 <- as.vector(sapply(
        featnames(frqTblKnownBiGms), function(x) strsplit(x, "_")[[1]][2]) )
    #get trailing word indexes
    wordIndxs<-vector()
    for (ctr in 1:length(words2)){
       wordIndxs[ctr]<-which(listTokens==words2[ctr])
    }
     #get the dfm for trailing words from the main word list
    frqTblKnownWords<-dfm_select(frqTblTokenz,pattern=words2,selection="keep",
                                 valuetype="fixed")
    #get the total counts from which to reduce the total word count by
    countBC2<-as.vector(rowSums(frqTblKnownWords))
    # compute the count for B2
    countB2<-as.vector(totalTokens-countBC2)
    #compute Qml for B2 
    #in returnVector2 you will store all the probabilities of the unknown words 
    # for which we are using katz backoff.
    returnVector2<-as.vector(frqTblTokenz[1,])
    #compute probabilities and store in returnVector2 after using counts
    returnVector2<-as.vector(returnVector2/countB2)
    #set probabilities of known trailing words to 0 in returnVector2
    returnVector2[wordIndxs]<-0
    
    #setup a flag for simple good turing
    simpleGoodTuring<-FALSE
    # we can find nA,nB to compute log(nC) = nA + nB*log(C)
    freqKnownBiGms <- biGmsFrqVec %>% unique() %>% sort()
    if (length(freqKnownBiGms)>1){
      cA<-freqKnownBiGms[1]
      nVA<-length(which(as.vector(frqTblKnownBiGms[1,])==cA))
      cB<-freqKnownBiGms[2]
      nVB<-length(which(as.vector(frqTblKnownBiGms[1,])==cB))
      matA<-matrix(c(1,log(cA),1,log(cB)), nrow=2,ncol=2,byrow=T)
      matY<-matrix(c(log(nVA),log(nVB)), nrow=2, ncol=1, byrow=T)
      invMatA<-solve(matA)
      matX<-invMatA %*% matY
      nA<-matX[1,1]
      nB<-matX[2,1]
      simpleGoodTuring<-TRUE
    }
    # n1 ~ n6 are frequencies of frequencies i.e. n1 stores the number
    # of times a frequency of 1 occurs in the frqTblKnownBiGms
    # n2 stores the number of times a frequency of 2
    # occurs in the frqTblKnownBiGms and so on
    n1<-0
    n2<-0
    n3<-0
    n4<-0
    n5<-0
    n6<-0
    #compute indexes of which entries in the table have frquencies 1~6 
    indx1<-which(as.vector(frqTblKnownBiGms[1,])==1)
    indx2<-which(as.vector(frqTblKnownBiGms[1,])==2)
    indx3<-which(as.vector(frqTblKnownBiGms[1,])==3)
    indx4<-which(as.vector(frqTblKnownBiGms[1,])==4)
    indx5<-which(as.vector(frqTblKnownBiGms[1,])==5)
    #setup frequenies of frequencies
    n1<-length(indx1)
    n2<-length(indx2)
    n3<-length(indx3)
    n4<-length(indx4)
    n5<-length(indx5)
    #check if we have any counts less than 5 so that we can use simple good turing
    #discounting 
    if ((n1==0)&(n2==0)&(n3==0)&(n4==0)&(n5==0)){
      #only large counts are available and these can't be discounted
      #switch to manual discounting
      simpleGoodTuring<-FALSE
      #setup returnVector1
      returnVector1[wordIndxs]<-as.vector(frqTblKnownBiGms[1,])
      returnVector1<-returnVector1/totalKnownBiGms
      PA1<-0.5
      returnVector1<-returnVector1*PA1
    }
  # check if conditions for simple good turing are met via flag    
    if (simpleGoodTuring) {
      #nA and nB are valid so we can use them to compute smoothed n1~n6 
      #whichever are at 0
      if (n1==0) n1=1*exp(nA)
      if (n2==0) n2=(2^nB)*exp(nA)
      if (n3==0) n3=(3^nB)*exp(nA)
      if (n4==0) n4=(4^nB)*exp(nA)
      if (n5==0) n5=(5^nB)*exp(nA)
      if (n6==0) n6=(5^nB)*exp(nA)
      #modify counts using simple good turning calculations
      modifiedCount<-as.vector(frqTblKnownBiGms[1,])
      modifiedCount<-computeMC(n1,n2,n3,n4,n5,n6,indx1,indx2,indx3,indx4,
                                indx5,modifiedCount)
      probabilityCount<-modifiedCount/totalKnownBiGms
      returnVector1[wordIndxs]<-probabilityCount
      PA1<-sum(probabilityCount)
    }
    else{
      #the algorithm has been unable to compute nA and nB so we can't use 
      #simple good turing to compute modified counts and hence the discounted
      #probabilities. in this case we are forced to use manual discounting to 
      #the extent of .50
      PA1<-0.5
      returnVector1[wordIndxs]<-as.vector(frqTblKnownBiGms[1,])
      returnVector1<-returnVector1/totalKnownBiGms
      returnVector1<-returnVector1*PA1
    }
  }
  returnVector2<-returnVector2*(1-PA1)
  return(list(returnStatusFlag,(returnVector1+returnVector2)))
}
