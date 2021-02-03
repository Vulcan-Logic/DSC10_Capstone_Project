#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(plotly)
library(shiny)
library(quanteda)
library(readtext)
library(dplyr)
source('doMainLogic.R')
source('computeQB12.R')
source('computeQTrigram.R')
source('computeQQuadgram.R')
source('computeMC.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output){
  prefix<-'./data/'
  load(file=paste0(prefix,"frqTblTokenz.rda"))
  load(file=paste0(prefix,"biGms.rda"))
  load(file=paste0(prefix,"Trigram.rda"))
  load(file=paste0(prefix,"Quadgram.rda"))
  observeEvent(input$button, {
      inputPh<-input$inputText
      returnList<-doMainLogic(inputPh,frqTblTokenz,biGms,Trigram,Quadgram)
      message<-returnList[[1]]
      if (is.null(message)){
        word1<-returnList[[2]]
        word2<-returnList[[3]]
        word3<-returnList[[4]]
        word4<-returnList[[5]]
        word5<-returnList[[6]]
        prob1<-returnList[[7]]
        prob2<-returnList[[8]]
        prob3<-returnList[[9]]
        prob4<-returnList[[10]]
        prob5<-returnList[[11]]
        head1<-"Predicted Word (highest probability):"
        head2<-"Other words with lower probability:"
        output$message0<-renderText(NULL)
        output$word1<-renderText(word1)
        output$word2<-renderText(word2)
        output$word3<-renderText(word3)
        output$word4<-renderText(word4)
        output$word5<-renderText(word5)
        output$head1<-renderText(head1)
        output$head2<-renderText(head2)
        output$message<-renderText(NULL)
        vectorWords<-c(word1,word2,word3,word4,word5)
        vectorProbs<-c(prob1,prob2,prob3,prob4,prob5)
        vectorProbs<-round(vectorProbs,3)
        data_mat<-data.frame(cbind(vectorWords,vectorProbs))
        col1<-runif(1,0,255)
        col2<-runif(1,0,255)
        col3<-runif(1,0,255)
        colArg<-paste0('rgb(',col1,',',col2,',',col3,')')
        output$plot <- renderPlotly({
          plot1 <- plot_ly(data=data_mat, x = data_mat[,1], y=data_mat[,2], 
            marker=list(
                      color=colArg,
                      line=list(
                              color='rgb(8,48,107)',
                              width=1.5
                      )),
            opacity=0.6,
            type = 'bar')
          plot1<-plot1 %>% layout(
            title = "PREDICTED WORD PROBABILITIES",
            xaxis = list(title = "Words"),
            yaxis = list(title = "Probability"))
          return(plot1)
        })  
      }
      else if (message=="NIL"){
        output$message<-renderText("No suggestions! Most frequent words
                                   by probability are as below:")
        word1<-returnList[[2]]
        word2<-returnList[[3]]
        word3<-returnList[[4]]
        word4<-returnList[[5]]
        word5<-returnList[[6]]
        prob1<-returnList[[7]]
        prob2<-returnList[[8]]
        prob3<-returnList[[9]]
        prob4<-returnList[[10]]
        prob5<-returnList[[11]]
        head1<-"Word with highest probability:"
        head2<-"Other words:"
        output$word1<-renderText("NO SUGGESTION")
        output$word2<-renderText(word1)
        output$word3<-renderText(word2)
        output$word4<-renderText(word3)
        output$word5<-renderText(word4)
        output$head1<-renderText(head1)
        output$head2<-renderText(head2)
        vectorWords<-c(word1,word2,word3,word4)
        vectorProbs<-c(prob1,prob2,prob3,prob4)
        vectorProbs<-round(vectorProbs,3)
        data_mat<-data.frame(cbind(vectorWords,vectorProbs))
        col1<-runif(1,0,255)
        col2<-runif(1,0,255)
        col3<-runif(1,0,255)
        colArg<-paste0('rgb(',col1,',',col2,',',col3,')')
        output$plot <- renderPlotly({
          plot1 <- plot_ly(data=data_mat, x = data_mat[,1], y=data_mat[,2],
                           marker=list(
                             color=colArg,
                             line=list(
                               color='rgb(8,48,107)',
                               width=1.5
                             )),
                           opacity=0.6,
                           type = 'bar')
          plot1<-plot1 %>% layout(
            title = "PREDICTED WORD PROBABILITIES",
            xaxis = list(title = "Words"),
            yaxis = list(title = "Probability",
                         range=c(0,(max(vectorProbs)+0.2))
                         )
            )
          return(plot1)
        })
      }
      else{
        output$message<-renderText(message)
        word1<-returnList[[2]]
        word2<-returnList[[3]]
        word3<-returnList[[4]]
        word4<-returnList[[5]]
        word5<-returnList[[6]]
        head1<-NULL
        head2<-NULL
        output$word1<-renderText(word1)
        output$word2<-renderText(word2)
        output$word3<-renderText(word3)
        output$word4<-renderText(word4)
        output$word5<-renderText(word5)
        output$head1<-renderText(head1)
        output$head2<-renderText(head2)
        output$plot <- renderPlotly({return(NULL)})
      }
    }
  )
})