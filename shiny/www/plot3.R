nature <- function(hp.df, insurer, plot.month, nature) {
  
  
  hp.df <- hp.df[which(hp.df$Insurer == insurer),]
  
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