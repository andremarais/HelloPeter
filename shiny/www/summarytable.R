setwd("C:/Users/anmarais/Desktop/GitHub/HelloPeter/shiny")

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

Mom$name <- "Mom"
MomHealth$name <- "MomHealth"
MSTI$name <- "MSTI"
DiscHealth$name <- "DiscHealth"
DiscLife$name <- "DiscLife" 
DiscInsure$name <- "DiscInsure" 
Liberty$name <- "Liberty" 
Metropolitan$name <- "Metropolitan"  
OutSurance$name <- "OutSurance"
MiWay$name <- "MiWay" 

Mom$Pname <- "Momentum"
MomHealth$Pname <- "Momentum Health"
MSTI$Pname <- "MSTI"
DiscHealth$Pname <- "Discovery Health"
DiscLife$Pname <- "Discovery Life" 
DiscInsure$Pname <- "Discovery Insure" 
Liberty$Pname <- "Liberty" 
Metropolitan$Pname <- "Metropolitan"  
OutSurance$Pname <- "OutSurance"
MiWay$Pname <- "MiWay" 



hp <- rbind(Mom, MomHealth, MSTI, DiscHealth, DiscLife, DiscInsure, Liberty, Metropolitan, OutSurance, MiWay)
hp$response.time <- as.Date(hp$response.date) - as.Date(hp$post.date)

hp1 <- hp[hp$post.month == "2015-04",]
hp1$response.time


aggregate(data = hp1, response.time ~ name , FUN = mean)


a <- as.data.frame.matrix(table(hp1[,c('name', 'type')]))
a$Compliment.Ratio <- percent(a$Compliment/ (a$Complaint + a$Compliment))

