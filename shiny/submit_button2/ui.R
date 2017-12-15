#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Demonstration of SubmitButton()"),
  
 
    sidebarPanel(
      
     selectInput("dataset","Choose a dataset:",
                 choices=c("iris","pressure","mtcars")),
     numericInput("obs","Number of observations:",6),
     submitButton("Update"),
     p("submitButton is used to control the reactiveness of the change in the user input")
     
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # just a header for the heading
      h4(textOutput("dataname")),
      # display the structure of the selected dataset
      verbatimTextOutput("structure"),
      
      # just a header for the heading
      h4(textOutput("observation")),
      
      # display the observations
      tableOutput("view")
    )
))
