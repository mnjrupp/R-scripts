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
  titlePanel("Demonstration of submitButton() in shiny"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("text1","Enter your first name"),
       textInput("text2","Enter your last name"),
       submitButton("Update!"),
       p("Click on the Update button to display name")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       textOutput("txt1"),
       textOutput("txt2")
    )
  )
))
