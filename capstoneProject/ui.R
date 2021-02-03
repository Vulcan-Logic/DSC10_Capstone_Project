#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Capstone Project For Data Science Specialization"),
  fluidRow(
    column(3),
    column(6,h3("A project by Vineet Wazir Singh")),
    column(3)
  ),
  fluidRow(column(1),
            column(10,h5("To fulfill the requirements for the data science capstone 
                  project of the Data Science Specialization programme of")),
            column(1)
           ),
  fluidRow(column(3),
           column(6,h4("John Hopkins University and Coursera")),
           column(3)
  ),
  hr(),
  fluidRow(
    column(2, h4("Enter a phrase:")),
    column(6, textInput("inputText",label=NULL,width=600)),
    column(4, actionButton("button",label = "Predict next word"))
    ),
  fluidRow(column(12,h4("Please wait after clicking the button. It can take 
                       upto 2 minutes for the programme to make a prediction"))),
  hr(),
  
  fluidRow(
    column(5,textOutput("message"))
  ),
  fluidRow(
    column(5,textOutput("head1",h4)),
    column(7,textOutput("word1",h4))
  ),
  fluidRow(
    column(5,textOutput("head2",h4)),
    column(7,textOutput("word2",h4))
  ),
  fluidRow(
    column(5),
    column(7,textOutput("word3",h4))
  ),
  fluidRow(
    column(5),
    column(7,textOutput("word4",h4))
  ),
  fluidRow(
    column(5),
    column(7,textOutput("word5",h4))
  ),
  hr(),
  fluidRow(
    column(12,plotlyOutput("plot"))
  )
))
