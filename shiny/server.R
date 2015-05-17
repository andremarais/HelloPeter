Mom <- readRDS(file.path(getwd(),"data/Momentum.RDS"))
MomHealth <- readRDS(file.path(getwd(),"data/Momentum Health.RDS")) 
MSTI <- readRDS(file.path(getwd(),"data/Momentum Short Term.RDS")) 
DiscHealth <- readRDS(file.path(getwd(),"data/Discovery Health.RDS")) 
DiscLife <- readRDS(file.path(getwd(),"data/Discovery Life.RDS")) 
DiscInsure <- readRDS(file.path(getwd(),"data/Discovery Insure.RDS")) 
Liberty <- readRDS(file.path(getwd(),"data/Liberty.RDS")) 
Metropolitan <- readRDS(file.path(getwd(),"data/Metropolitan.RDS")) 
OutSurance <- readRDS(file.path(getwd(),"data/OutSurance.RDS")) 
#MiWay <- readRDS(file.path(getwd(),"data/MiWay.RDS")) 


#setwd("C:/Users/Veldrin/Documents/GitHub/HelloPeter/shiny")

HP.data <- rbind(Mom,
                 MomHealth,
                 MSTI,
                 DiscHealth,
                 DiscLife,
                 DiscInsure,
                 Liberty,
                 Metropolitan,
                 OutSurance)

HP.data$response.date <- as.POSIXct.numeric(HP.data$response.date, origin = "1970-01-01", tz = "GMT")
HP.data$post.date <- as.POSIXct.numeric(HP.data$post.date, origin = "1970-01-01", tz = "GMT")
HP.data$response.delay <- round(difftime(HP.data$response.date,
                                         HP.data$post.date, 
                                         units = "hours", 
                                         tz = "GMT"),4)
HP.data$post.date.month <- as.Date(paste(substring(as.character(HP.data$post.date), 1, 7), "01", sep = "-" ))




source("www/plot1.R", local = T)
source("www/plot2.R", local = T)
source("www/plot3.R", local = T)
source("www/summarytable.R", local = T)


shinyServer(function(input, output) {
  

  
  
# Turnaround Times====
#plot
output$PlotTime <- renderPlot(complaints(HP.data, input$Insurer))
# Download plot PNG
output$TurnaroundtimePlot <- downloadHandler(
  filename = function() paste(input$Insurer, " Turnaround time.png", sep = ''),
  #' A problem is caused by ggsave trying to match the file extension to the correct graphics device. 
  #' The temporary file, however, doesn't have an extension so the matching fails. 
  #' This can be remedied by specifically setting the device in the ggsave function call
  #' Im not a smart man... here's the link http://stackoverflow.com/a/22901025
  content = function(file) {
    device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file, plot = download.time.plot, device = device)
  }
  )
# Download supporting data
output$TurnaroundtimeData <- downloadHandler(
  filename = function() paste(input$Insurer,Sys.Date(), " Turnaround time.csv", sep = ''),
  content = function(file) {
    write.csv(download.time.data, file, row.names = F)
  }
    )
# 
#     
# 
# # Post Count====
# #plot
output$PlotPost <- renderPlot(posts(HP.data, input$Insurer))
# # Download plot PNG
output$PostsPlot <- downloadHandler(
  filename = function() paste(input$Insurer, " Posts.png", sep = ''),
  #' A problem is caused by ggsave trying to match the file extension to the correct graphics device. 
  #' The temporary file, however, doesn't have an extension so the matching fails. 
  #' This can be remedied by specifically setting the device in the ggsave function call
  #' Im not a smart man... here's the link http://stackoverflow.com/a/22901025
  content = function(file) {
    device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file, plot = download.post.plot, device = device)
  }
)
# # Download supporting data
output$PostsData <- downloadHandler(
  filename = function() paste(input$Insurer,Sys.Date(), " Posts.csv", sep = ''),
  content = function(file) {
    write.csv(download.post.data, file, row.names = F)
  }
)
#   
# #Nature Plot
 output$Nature <- renderPlot(nature(HP.data, input$Insurer2, input$daterangeN, input$Nature))

output$summarytable <- renderTable(summarytable(HP.data, input$summaryselect, input$daterangeS))






})


