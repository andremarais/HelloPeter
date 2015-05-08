posts <- function(insurar, from) {
  
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
  
  
  hp.monthly <- count(hp.df, c('post.date.month','type'))

  
  post.plot <- ggplot()+ 
    geom_bar(data = hp.monthly, aes(x = post.date.month, y = freq, fill = type), stat = "identity", position = "dodge")+ 
    scale_fill_brewer(palette = "Set1")+
    theme(axis.text.x = element_text(angle = 45))+
    ggtitle(paste(p.name, " compliments, complaints and conversions"))+
    xlab("Date") +
    ylab("Frequency")
  
  download.post.data <<- hp.monthly
  download.post.plot <<- post.plot
  
  return(post.plot)
  
}