# reads in data sets
hp.stats <- readRDS(file.path(getwd(),"data/hp.RDS"))[[2]]

source("www/plot1.R", local = T)

shinyServer(function(input, output)

  output$Plot <- complaints(hp.stats, "2015-01-01")
  
  )