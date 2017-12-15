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
   
  output$tabs = renderUI({
    
    # usual syntax of tabset -
    ## tabsetPanel(tabPanel(title="")) ## for a single tabe in the tabsetPanel
    ## tabsetPanel(tabPanel(title=""), tabPanel(title="")) ## for two tabs in the tabsetPanel
    #   using lapply(), we will apply the tabPanel function on each of the tab title to get a list of tabPanels(s)
    
    Tabs <- lapply(paste("tab no.",1:input$n,sep=" "),tabPanel)
    
    #do.call function allows you to call any R function, but instead of writing out the arguments one by one
    
    do.call(tabsetPanel,Tabs)
    
  })
  
})
