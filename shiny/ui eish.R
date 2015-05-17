# require(RCurl)
# require(XML)
# require(wordcloud)
require(plyr)
require(ggplot2)
require(scales)

shinyUI(fluidPage(
  titlePanel("Hello Peter stats"),

    mainPanel(tabsetPanel(
      # 12 Month History ====
      tabPanel("12 Month History",

               selectInput("Insurer",
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

               plotOutput("PlotTime"),
               fluidRow(downloadButton("TurnaroundtimePlot", label = "Download Graph"),
                        downloadButton("TurnaroundtimeData", label = "Download Data")),
               
               # Posts plot & buttons
               plotOutput("PlotPost"),
               fluidRow(downloadButton("PostsPlot", label = "Download Graph"),
                        downloadButton("PostsData", label = "Download Data"))),
      
      # Nature & Topics of posts
      tabPanel("Nature of Posts",
                 fluidRow(
                   column(4,
                          dateInput("daterangeN",
                             label = "Select Month",
                             value = seq(Sys.Date(), length = 2, by = "-1 month")[2],
                             min = "2014-05-02",
                             format = "yyyy-mm-dd",
                             startview = "month")),
                   column(4,
                          selectInput("Insurer2",
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
                                      selected = "Mom")),
                   column(4,
                          selectInput("Nature",
                               "Nature: ",
                               choices = c("Complaint",
                                           "Compliment"),
                               selected  = "Complaint"))
                   
                 ),
                 plotOutput("Nature")
               ),
      tabPanel("Summary tables",
               fluidRow(
               sidebarPanel(
                 selectizeInput("summaryselect",
                                       label = "Select insurers",
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
                                       selected = c("Mom", 
                                                    "MomHealth", 
                                                    "MSTI",
                                                    "DiscHealth",
                                                    "DiscLife",
                                                    "DiscInsure",
                                                    "Liberty",
                                                    "Metropolitan",
                                                    "Outsurance",
                                                    "MiWay"),
                                       multiple = T),
                 dateInput("daterangeS",
                         label = "Select Month",
                         value = seq(Sys.Date(), length = 2, by = "-1 month")[2],
                         min = "2014-05-02",
                         format = "yyyy-mm-dd",
                         startview = "month")),
               mainPanel(plotOutput("Nature")
                         )))
      )
      )
  )
  )


