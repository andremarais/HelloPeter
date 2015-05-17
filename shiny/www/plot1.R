complaints <- function(hp.df, insurer) {
  
  

  
  hp.df <- hp.df[which(hp.df$Insurer == insurer),]
  
  
  hp.df <- hp.df[which(hp.df$post.date < paste(substring(as.character(Sys.Date()), 1, 7), "01", sep = "-" )),]

  
  not.zero <- which(hp.df$response.date != 0)
  
  
  hp.df$post.date.month <- as.Date(paste(substring(as.character(hp.df$post.date), 1, 7), "01", sep = "-" ))
  
  
  
  ave.time.pm <- data.frame(aggregate(data = hp.df, response.delay ~ post.date.month, FUN = mean))
  ave.time.pm$response.delay <- as.numeric(ave.time.pm$response.delay)
  ave.time.pm$post.date.month <- as.Date(ave.time.pm$post.date.month )
  
  time.plot <- ggplot(data = ave.time.pm, aes(x = post.date.month, y = response.delay, fill = response.delay)) + 
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    ggtitle(paste(as.character(unique(hp.df$Insurer.proper.name)), "turaround time")) +
    xlab("Date") +
    ylab("Hours")
    
#   download.time.data <<- ave.time.pm
#   download.time.plot <<- time.plot
  
  return(time.plot)
  
}