complaints <- function(insurar, from) {
  
  if (insurar == "Mom") {
    hp.df <- Mom; p.name <- "Momentum"
  } else if (insurar == "MomHealth") {
    hp.df <- MomHealth; p.name <- "Momentum Health"
  } else if (insurar == "MSTI") {
    hp.df <- MSTI; p.name <- "Momentum Short Term"
  } else if (insurar == "DiscHealth") {
    hp.df <- DiscHealth; p.name <- "Discovery Health"
  } else if (insurar == "DiscLife") {
    hp.df <- DiscLife; p.name <- "Discovery Life"
  } else if (insurar == "DiscInsure") {
    hp.df <- DiscInsure; p.name <- "Discovery Insure"
  } else if (insurar == "Liberty") {
    hp.df <- Liberty; p.name <- "Liberty"
  } else if (insurar == "Metropolitan") {
    hp.df <- Metropolitan; p.name <- "Metropolitan"
  } else if (insurar == "Outsurance") {
    hp.df <- OutSurance; p.name <- "Outsurance"
  } else if (insurar == "MiWay") {
    hp.df <- MiWay; p.name <- "MiWay"
  }
  
  


  hp.df$response.date <- as.Date(hp.df$response.date, format = "%Y-%m-%d")
  hp.df$post.date <- as.Date(hp.df$post.date, format = "%Y-%m-%d")
  hp.df <- hp.df[which(hp.df$post.date < paste(substring(as.character(Sys.Date()), 1, 7), "01", sep = "-" ) &
                         hp.df$post.date > from),]
  hp.df$response.time <- hp.df$response.date - hp.df$post.date
  hp.df$post.date.month <- as.Date(paste(substring(as.character(hp.df$post.date), 1, 7), "01", sep = "-" ))
  
  
  
  ave.time.pm <- data.frame(aggregate(data = hp.df, response.time ~ post.date.month, FUN = mean))
  ave.time.pm$response.time <- as.numeric(ave.time.pm$response.time)
  ave.time.pm$post.date.month <- as.Date(ave.time.pm$post.date.month )
  
  p <- ggplot(data = ave.time.pm, aes(x = post.date.month, y = response.time, fill = response.time)) + 
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    ggtitle(paste(p.name, "turaround time")) +
    xlab("Date") +
    ylab("Average number of days")
    
  download.data <<- hp.df
  download.plot <<- p
  
  return(list(p, hp.df))
  
}