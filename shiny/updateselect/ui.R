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
  titlePanel("Demo updateselectInput()"),
  # data coming from global.r
  # using the variables from the data frame to populate the selectinput
  selectInput("Year","Year",choices = unique(data$Year)),
  
  # No choices in the next two because they will populate based on the first selectInput
  selectInput("Month","Month",choices = "",selected = ""),
  selectInput("Name","Name",choices = "",selected = ""),
  
  tableOutput("dataset")
  
))
