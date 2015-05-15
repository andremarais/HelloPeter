# require(RCurl)
# require(XML)
# require(wordcloud)
require(plyr)
require(ggplot2)
require(scales)

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
                           fluidRow(
                           dateInput("daterangeN",
                                     label = "Select Month",
                                     value = seq(Sys.Date(), length = 2, by = "-1 month")[2],
                                     min = "2014-05-02",
                                     format = "yyyy-mm-dd",
                                     startview = "month"),
                           selectInput("Nature",
                                       "Nature: ",
                                       choices = c("Complaint",
                                                   "Compliment"),
                                       selected  = "Complaint"
                                       )

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
