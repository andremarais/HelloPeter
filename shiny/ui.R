# require(RCurl)
# require(XML)
# require(wordcloud)
require(plyr)
require(ggplot2)

shinyUI(fluidPage(
  titlePanel("Hello Peter stats"),
  sidebarLayout(
    sidebarPanel(selectInput("Insurer",
                             label = "Insurer",
                             choices = c("Momentum" = "Mom",
                                         "Momentum Health" = "MomHealth",
                                         "MSTI" = "MSTI",
                                         "Discovery Health" = "DiscHealth",
                                         "Discovery Life" = "DiscLife",
                                         "Discovery Insure" = "DiscInsure",
                                         "Liberty" = "Liberty",
                                         "Metropolitan" = "Metropolitan",
                                         "OutSurance " = "Outsurance",
                                         "MiWay" = "MiWay"),
                             selected = "Mom"),
                 dateInput("daterange",
                           label = "From:",
                           value = "2014-05-02",
                           min = "2014-05-02",
                           format = "yyyy-mm-dd",
                           startview = "month"
                           ),
                 wellPanel(plotOutput("Nature"),
                           dateInput("daterangeN",
                                     label = "Select Month",
                                     value = "2014-05-02",
                                     min = "2014-05-02",
                                     format = "yyyy-mm-dd",
                                     startview = "month"

                             ))
                 
      
      ),
    mainPanel(
      #Response time plot & buttons 
      plotOutput("PlotTime"),
      fluidRow(downloadButton("TurnaroundtimePlot", label = "Download Graph"),
               downloadButton("TurnaroundtimeData", label = "Download Data")),
      
      # Posts plot & buttons
      plotOutput("PlotPost"),
      fluidRow(downloadButton("PostsPlot", label = "Download Graph"),
               downloadButton("PostsData", label = "Download Data"))
      
                       
      
      
      
      )
  
)))
