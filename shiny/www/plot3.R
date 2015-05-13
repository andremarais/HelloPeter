nature <- function(insurar, plot.month, nature) {
  
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
  hp.df$response.time <- hp.df$response.date - hp.df$post.date
  hp.df$post.date.month <- as.Date(paste(substring(as.character(hp.df$post.date), 1, 7), "01", sep = "-" ))
  
  hp.df <- hp.df[which(hp.df$type == nature),]

  
  
  nature.pm <- count(hp.df, c('post.date.month',' nature'))
  
  to.plot <- nature.pm[which(nature.pm$post.date.month == as.Date(paste(substring(as.character(plot.month), 1, 7), "01", sep = "-" ))),] ###
  to.plot$nature <- factor(to.plot$nature)
  plot.order <- order(to.plot$freq, decreasing = T)
  to.plot <- to.plot[plot.order,]
  to.plot <- to.plot[1:5,]
  

  

  nature.plot <- ggplot()+ 
    geom_bar(data = to.plot, aes(x = reorder(nature, freq), y = freq, fill = freq), stat = "identity", position = "dodge")+ 
    coord_flip() +
    xlab("") +
    ylab("Frequency")+
    ggtitle(paste("Top 5 natures of", nature, sep = " "))+
    theme(legend.position="none")+
    scale_fill_gradient(low = "black", high = if(nature == "Complaint") "red" else "blue")
    

  
  return(nature.plot)
  
}