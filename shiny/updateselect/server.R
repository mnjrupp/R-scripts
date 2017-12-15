#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(session,input, output) {
   
  # Update based on the year change event
  # basically populate the months corresponding to the year
  observeEvent(
    input$Year,
    updateSelectInput(session,"Month","Month",choices = data$Month[data$Year==input$Year]))
  # Update as soon as Month gets populated according to the year and month selected
  observeEvent(
    input$Month,
    updateSelectInput(session,"Name","Name",choices = data$Name[data$Month==input$Month & data$Year==input$Year]))
  
  # just to display the dataset we created in globar.r
  output$dataset <-renderTable({data})
  
})
