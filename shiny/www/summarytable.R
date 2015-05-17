summarytable <- function(hp.df, insurers, month) {






hp1 <- hp.df[which(substring(hp.df$post.date.month, 1, 7) == substring(as.character(month), 1,7) &
                       HP.data$response.date > 0),]

hp1 <- hp1[which(hp1$Insurer %in% insurers),]

response.time <- aggregate(data = hp1, response.delay ~ Insurer.proper.name , FUN = mean)


summary <- as.data.frame.matrix(table(hp1[,c('Insurer.proper.name', 'type')]))
summary$Compliment.Ratio <- percent(summary$Compliment/ (summary$Complaint + summary$Compliment))
summary$Conversion.Ratio <- percent(summary$Conversion/ summary$Complaint)
summary$Pname <- rownames(summary)

summary.table <- merge(summary, response.time, by.x = 'Pname', by.y = 'Insurer.proper.name')

colnames(summary.table) <- c("Insurer", "Complaints", "Compliments", "Conversions", "Compliment ratio", "Conversion ratio", "Response time (hours)")
rownames(summary.table) <- NULL


return(summary.table[order((summary.table$Complaints + summary.table$Compliments), decreasing = T),])

}



