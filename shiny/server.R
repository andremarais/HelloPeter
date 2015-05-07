Mom <- readRDS(file.path(getwd(),"data/Momentum.RDS"))
MomHealth <- readRDS(file.path(getwd(),"data/Momentum Health.RDS")) 
MSTI <- readRDS(file.path(getwd(),"data/Momentum Short Term.RDS")) 
DiscHealth <- readRDS(file.path(getwd(),"data/Discovery Health.RDS")) 
DiscLife <- readRDS(file.path(getwd(),"data/Discovery Life.RDS")) 
DiscInsure <- readRDS(file.path(getwd(),"data/Discovery Insure.RDS")) 
Liberty <- readRDS(file.path(getwd(),"data/Liberty.RDS")) 
Metropolitan <- readRDS(file.path(getwd(),"data/Metropolitan.RDS")) 
OutSurance <- readRDS(file.path(getwd(),"data/OutSurance.RDS")) 
MiWay <- readRDS(file.path(getwd(),"data/MiWay.RDS")) 



source("www/plot1.R", local = T)

shinyServer(function(input, output) {

  
  output$Plot <- renderPlot(complaints(input$Insurer, input$daterange)[[1]])
  output$TurnaroundtimePLot <- downloadHandler(filename = function() paste(input$Insurer, ".png", sep = ''),
                                               content = function(file) ggsave(file, plot = plotInput(), device = device))
  

  
  output$TurnaroundtimeData <- downloadHandler(
    filename = function() {
      paste(input$Insurer, ".cs3v", sep = '')
      },
    content = function(file) write.csv(download.data, file)
  )

  
}
)


# output$downloadData <- downloadHandler(
#   filename = function() {
#     paste('data-', Sys.Date(), '.csv', sep='')
#   },
#   content = function(file) {
#     write.csv(downdata , file)
#   }
# )