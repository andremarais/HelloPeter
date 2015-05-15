complaints <- function(insurer, from) {
  
  if (insurer == "Mom") {
    hp.df <- Mom; p.name <- "Momentum"
  } else if (insurer == "MomHealth") {
    hp.df <- MomHealth; p.name <- "Momentum Health"
  } else if (insurer == "MSTI") {
    hp.df <- MSTI; p.name <- "Momentum Short Term"
  } else if (insurer == "DiscHealth") {
    hp.df <- DiscHealth; p.name <- "Discovery Health"
  } else if (insurer == "DiscLife") {
    hp.df <- DiscLife; p.name <- "Discovery Life"
  } else if (insurer == "DiscInsure") {
    hp.df <- DiscInsure; p.name <- "Discovery Insure"
  } else if (insurer == "Liberty") {
    hp.df <- Liberty; p.name <- "Liberty"
  } else if (insurer == "Metropolitan") {
    hp.df <- Metropolitan; p.name <- "Metropolitan"
  } else if (insurer == "Outsurance") {
    hp.df <- OutSurance; p.name <- "Outsurance"
  } else if (insurer == "MiWay") {
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
  
  time.plot <- ggplot(data = ave.time.pm, aes(x = post.date.month, y = response.time, fill = response.time)) + 
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    ggtitle(paste(p.name, "turaround time")) +
    xlab("Date") +
    ylab("Average number of days")
    
  download.time.data <<- ave.time.pm
  download.time.plot <<- time.plot
  
  return(time.plot)
  
}