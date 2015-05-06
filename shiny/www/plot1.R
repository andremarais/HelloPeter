complaints <- function(df1, from) {
  
  # from <- 
  df1 <- hp.stats
  
  hp.df <- data.frame(df1)
  hp.df$response.date <- as.Date(hp.df$response.date)
  hp.df$post.date <- as.Date(hp.df$post.date)
  hp.df <- hp.df[which(hp.df$post.date < paste(substring(as.character(Sys.Date()), 1, 7), "01", sep = "-" ) &
                         hp.df$post.date > from),]
  hp.df$response.time <- hp.df$response.date - hp.df$post.date
  hp.df$post.date.month <- as.Date(paste(substring(as.character(hp.df$post.date), 1, 7), "01", sep = "-" ))
  
  #hp.monthly <- data.frame(count(hp.df, c('post.date.month','type')))
#   
#   
#   
#   hp.count <- count(hp.df, c('post.date', 'type'))
#   ave.time.pm <- aggregate(data = hp.df, response.time ~ post.date.month, FUN = mean)
#   nature.count <- count(hp.df, c('post.date.month', 'nature'))
#   
#   
#   
  hp.monthly <- aggregate(data = hp.df,  rownames(hp.df) ~ type + post.date.month, FUN = length)
  colnames(hp.monthly) <- c("type", "post.date.month", "freq")
  
  
  p <- ggplot(data = hp.monthly, aes(x = post.date.month, y = freq, fill = type))+ 
  geom_bar(stat = "identity", position = "dodge")
   # geom_bar(data = hp.monthly, aes(x = post.date.month, y = freq, fill = type), stat = "identity", position = "dodge")
#     scale_fill_brewer(palette = "Set1")+
#     theme(axis.text.x = element_text(angle = 45))+
#     #ggtitle(paste(insurar.names[1], "compliments, complaints and conversions"))+
#     xlab("Date") +
#     ylab("Frequency")
  
  return(p)
  
}