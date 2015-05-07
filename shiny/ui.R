# require(RCurl)
# require(XML)
# require(wordcloud)
require(plyr)
require(ggplot2)

shinyUI(fluidPage(
  titlePanel("Hello Peter stats"),
  sidebarLayout(
    sidebarPanel(selectInput("Insurer",
                             label = "Insurar",
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
                           value = "2014-05-01",
                           min = "2014-05-01",
                           format = "yyyy-mm-dd",
                           startview = "month"
                           )
                 
      
      ),
    mainPanel(plotOutput("Plot"),
              fluidRow(downloadButton("TurnaroundtimePLot", label = "Download Graph"),
                       downloadButton("TurnaroundtimeData", label = "Download Data")
                       ))
  
)))
