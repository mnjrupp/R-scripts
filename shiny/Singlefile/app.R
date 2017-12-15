#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   h1("Hello World"),
   sidebarLayout(
     sidebarPanel(
       selectInput("dataset","Choose a dataset:",
                   choices = ls("package:datasets"),
                   selected = "pressure")
     ),
     mainPanel(
       verbatimTextOutput("dump"),
       plotOutput("plot")
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$dump <-renderPrint({
     dataset <- get(input$dataset,"package:datasets",inherits = FALSE)
     str(dataset)
   })
   
   output$plot <- renderPlot({
     dataset <- get(input$dataset,"package:datasets",inherits = FALSE)
     plot(dataset)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

