library(shiny)
library(shinydashboard)
library(rCharts)

setwd("~/RStudio/shiny/shinydashboards")

# Data preparation code
# function to convert to POSIXct date format, specifically for line chart
to_jsdate2 <- function(x){
  as.numeric(as.POSIXct(as.Date(x), origin="1970-01-01")) * 1000
}

# read in an altered sample of JIRA data 
opsdata <- read.csv("data/opsdata.csv")

# change the date columns format to Date
opsdata$created <- as.Date(opsdata$created, format = "%m/%d/%Y %H:%M")

# sort the data by date
opsdata <- opsdata[order(opsdata$created),]

# create a month variable for aggregation
opsdata$created_month <- as.Date(cut(opsdata$created, "month"))

# create a vector with cumulative sum of unique PIs
unique_PIs <- cummax(as.numeric(factor(opsdata$PI_name, levels = unique(opsdata$PI_name))))

# matching cumulative sum of unique PIs to unique months
PI_cumul_growth <- aggregate(unique_PIs, list(Month=opsdata$created_month), max)

# add variable with only new PIs added in each month
PI_cumul_growth$new_pi_num <- c(PI_cumul_growth$x[1], (tail(PI_cumul_growth$x, -1) - head(PI_cumul_growth$x, -1)))

#change the date format to suit rCharts
PI_cumul_growth$date <- to_jsdate2(as.Date(PI_cumul_growth$Month))





ui <- dashboardPage(
  dashboardHeader(
    title = "Demo Dashboard",
    dropdownMenuOutput("msgOutput")
  ),
  dashboardSidebar(
    dateRangeInput(inputId = "dateRange",label="Date Range",
                   start="2012-01-01",end=Sys.Date(),
                   format = "mm/dd/yyyy"),
    sidebarMenu(
      menuItem("Operations",tabName = "operations"),
      menuSubItem("Projects Analysis",tabName = "projects",icon = icon("bar-chart-o")),
      menuSubItem("Raw data",tabName="raw",icon = icon("table"))
    )),
  dashboardBody(
    tabItems(
      tabItem("operations",
              h1("This is the main page of the demo"),
              h3("place main chart of links here")),
      tabItem("projects",
              fluidRow(
                box(
                
                showOutput("PI_growth_chart","highcharts"),
                width = 4
              ),
              box(
                selectInput(inputId = "group",label = "Group by:",
                            choices = c("Application","FundingType"),
                            selected = "Application"),
                plotOutput("FrequencyChart",height = 300),width = 4
              ),
              box(
                selectInput(inputId = "distVar",label = "Distribution of: ",
                            choices = c("HoursLogged","DaysOpen"),
                            selected = "HoursLogged"),
                plotOutput("DistributionPlot",height = 300),width = 4
              )
          ),
          fluidRow(
            box(
              width = 4,
              downloadButton("pdfdown","download PDF")
            )
          )
      )
      ,
      tabItem("raw",
              fluidRow(
                box(width = 12,
                    dataTableOutput("table"),
                    downloadButton("downloadData","Download"))
              )
      )
    )
  )
  
)



server <- function(input,output){
  filteredData <-reactive({
    filtered_PI_growth <- subset(PI_cumul_growth,Month >= input$dateRange[1] &
                                   Month <= input$dateRange[2])
    filtered_PI_growth
  })
  
  output$PI_growth_chart <- renderChart({
    PI_growth_plot <- hPlot(x ~ date, type = "line", data = filteredData(), title = "Number of PIs")
    PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
    PI_growth_plot$yAxis(title = list(text = "PIs"),
                         min = 0, gridLineColor = "#ffffff")
    PI_growth_plot$plotOptions(line = list(color = "#5C7A00", marker = list(enabled = F)))
    PI_growth_plot$tooltip(dateTimeLabelFormats = list(month = "%A, %b %e, %Y"))
    PI_growth_plot$chart(zoomType="x", height = 375)
    PI_growth_plot$addParams(dom="PI_growth_chart")
    return(PI_growth_plot)
  })
  
  output$FrequencyChart <- renderPlot({
    GROUP <- input$group
    freqPlot <- plot(subset(opsdata[,GROUP],opsdata$created>=input$dateRange[1] &
                              opsdata$created<= input$dateRange[2]) ,main = "Number of Projects")
    freqPlot
  })
  
  output$d1 <- renderPlot({
    GROUP <- input$group
   d1 <- dPlot(
    x ="Application" ,
    data = opsdata[,"Application"],
    type = "bar"
  )
   d1
  })
  
  output$DistributionPlot <- renderPlot({
    VAR <- input$distVar
    histPlot <- hist(subset(opsdata[,VAR],opsdata$created>=input$dateRange[1] &
                              opsdata$created<= input$dateRange[2]),main = paste0("Distribution of ", VAR),
                     xlab = VAR)
    histPlot
  })
  
  output$table <-renderDataTable({opsdata})
  # downloadHandler function used to download the content
  output$downloadData <- downloadHandler(
    
    filename = "opsdata.csv",
    content = function(file){
      write.table(opsdata,file,sep = ",",row.names = F)
    }
    
  )
  
  output$pdfdown <- downloadHandler(
    filename = "dashboard.pdf",
    
    content = function(file){
      pdf(file)
      # TODO: Need to fix the first plot hPlot not rendering
      PI_growth_plot <- hPlot(x ~ date, type = "line", data = filteredData(), title = "Number of PIs")
      PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
      PI_growth_plot$yAxis(title = list(text = "PIs"),
                           min = 0, gridLineColor = "#ffffff")
      PI_growth_plot$plotOptions(line = list(color = "#5C7A00", marker = list(enabled = F)))
      PI_growth_plot$tooltip(dateTimeLabelFormats = list(month = "%A, %b %e, %Y"))
      PI_growth_plot$chart(zoomType="x", height = 300)
      PI_growth_plot$addParams(dom="PI_growth_chart")
      PI_growth_plot
      
     
      
      GROUP <- input$group
      plot(opsdata[,GROUP],main = "Number of Projects")
      
      VAR <- input$distVar
      hist(opsdata[,VAR],main = paste0("Distribution of ", VAR),
           xlab = VAR)
      
      dev.off()
    }
  )
  output$msgOutput <- renderMenu({
    msgs <- apply(read.csv("data/messages.csv"),1,function(row){
      messageItem(from=row[["from"]],message=row[["message"]])
    })
    dropdownMenu(type = "messages",.list = msgs)
    
  })
}

runApp(shinyApp(ui,server),launch.browser = TRUE)