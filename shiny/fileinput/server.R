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
shinyServer(function(input, output) {
   
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file=file1$datapath,sep=input$sep,header = input$header,stringsAsFactors = input$stringAsFactors)
  })
  # this reactive output contains the summary of the dataset and display summary in table format
  output$filedf <-renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
  # Output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return()}
    summary(data())
  })
  
  #contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  # renderUI is used to dynamically generate the tabsets when the file is loaded, else show image
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by R") # TODO Add an image tag 
    else
      tabsetPanel(tabPanel("About file",tableOutput("filedf")),tabPanel("Data",tableOutput("table")),tabPanel("Summary",tableOutput("sum")))
  })
  
  
})
