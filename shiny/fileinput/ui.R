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
  titlePanel("File Input"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # fileinput() function is used to get the file upload control option
      fileInput("file","Upload the file"),
      helpText("Default max. file size is 5MB"),
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
      checkboxInput(inputId="header",label = "Header",value = FALSE),
      checkboxInput(inputId = "stringAsFactors","stringAsFactors",FALSE),
      br(),
      radioButtons(inputId = "sep",label = "Seperator",choices = c(Comma=",",Semicolon=";",Tab="\t",Space=" ",selected=","))
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       uiOutput("tb")
    )
  )
))
