# require(RCurl)
# require(XML)
# require(wordcloud)
require(plyr)
require(ggplot2)

setwd("C:/Users/anmarais/Desktop/GitHub/HelloPeter/shiny")

hp.stats <- readRDS(file.path(getwd(),"data/hp.RDS"))



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
                                         "Discovery Insure", "DiscInsure",
                                         "Liberty" = "Liberty",
                                         "Metropolitan" = "Metropolitan"),
                                         #still need to add Outsurance and MiWay's data
                             selected = "Mom"),
                 dateInput("daterange",
                           label = "From:",
                           value = min(as.Date(hp.stats[[1]]$post.date)),
                           min = min(as.Date(hp.stats[[1]]$post.date)),
                           format = "yyyy-mm-dd",
                           startview = "month"
                           )
                 
      
      ),
    mainPanel(plotOutput("Plot")))
  
))
