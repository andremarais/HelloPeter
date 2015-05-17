posts <- function(hp.df, insurer) {
  

  hp.df <- hp.df[which(hp.df$Insurer == insurer),]
  
  
  
  hp.monthly <- count(hp.df, c('post.date.month','type'))

  
  post.plot <- ggplot()+ 
    geom_bar(data = hp.monthly, aes(x = post.date.month, y = freq, fill = type), stat = "identity", position = "dodge")+ 
    scale_fill_brewer(palette = "Set1")+
    theme(axis.text.x = element_text(angle = 45))+
    ggtitle(paste(as.character(unique(hp.df$Insurer.proper.name)), " compliments, complaints and conversions"))+
    xlab("Date") +
    ylab("Frequency")
  
  download.post.data <<- hp.monthly
  download.post.plot <<- post.plot
  
  return(post.plot)
  
}