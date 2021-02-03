computeMC<-function(n1,n2,n3,n4,n5,n6,indx1,indx2,indx3,indx4,
                                indx5,modifiedCount){
  constantValue<-(6*n6)/n1
  if (length(indx1)>0){
      cS1<-((2*(n2/n1))-(1*constantValue))/(1-constantValue)
      modifiedCount[indx1]<-cS1
    }
  if (length(indx2)>0){
    cS2<-((3*(n3/n2))-(2*constantValue))/(1-constantValue)
    modifiedCount[indx2]<-cS2
  }
  if (length(indx3)>0){
    cS3<-((4*(n4/n3))-(3*constantValue))/(1-constantValue)
    modifiedCount[indx3]<-cS3
  }
  if (length(indx4)>0){
    cS4<-((4*(n5/n4))-(4*constantValue))/(1-constantValue)
    modifiedCount[indx4]<-cS4
  }
  if (length(indx5)>0){
    cS5<-((5*(n6/n5))-(5*constantValue))/(1-constantValue)
    modifiedCount[indx5]<-cS5
  }
  
  return(modifiedCount)
}