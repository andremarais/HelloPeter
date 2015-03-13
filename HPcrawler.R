require(RCurl)
require(XML)

hp <- c()

link <- data.frame()
title <- data.frame()
for (i in 1:8){
  hp[i] <- httpGET(paste("http://hellopeter.com/momentum/compliments-and-complaints?country=South%20Africa&pg=", i, sep = ""))
  
}

for (i in 1:8) {
  linklocations <- gregexpr("<div class=\"td-item2\"><a class=\"fb-link\"", hp[i])
for (j in 1:length(linklocations[[1]])) {

#gets URL
snippet <- substring(hp[i],
                     linklocations[[1]][j],
                     if (j == length(linklocations[[1]])) nchar(hp[i]) else linklocations[[1]][j+1])

#
link[j,i] <- substring(snippet, gregexpr("href=\"",snippet)[[1]][1] + 6,gregexpr(" title=\"",snippet)[[1]][1]-2)
a <- gregexpr("title=\"",snippet)[[1]][1] + 7
b <- min(gregexpr("\">",snippet)[[1]][which(gregexpr("\">",snippet)[[1]] > gregexpr("title=\"",snippet)[[1]][1] + 7)])-1
title[j,i] <- substring(snippet, a, b)

}

}



