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
  titlePanel("Demonstration of renderUI in shiny - Dynamically creating the tabs based on user inputs"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       numericInput("n","Enter number of tabs needed",1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       # using the output variable from renderUI in server.r and display tabs
       # uiOutput is used in ui.r to interact with the output variable of the renderUI() function in server.r
      uiOutput("tabs")
    )
  )
))
