require(RCurl)
require(XML)

trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)


hp <- c()
link <- data.frame()
title <- data.frame()
time <- data.frame()
type <- data.frame()

hp[1] <- httpGET("http://hellopeter.com/momentum/compliments-and-complaints?country=South%20Africa&pg=1")



#gets last page
lpsnippet <- substring(hp[1], gregexpr(">>", hp[1])[[1]][1], gregexpr(">>", hp[1])[[1]][2])
a <- as.numeric(regexpr("pg=",lpsnippet)[[1]] + attr(gregexpr("pg=",lpsnippet)[[1]], "match.length"))
b <- min(gregexpr("\"",lpsnippet)[[1]][which(gregexpr("\"",lpsnippet)[[1]] > gregexpr("pg=",lpsnippet)[[1]][1])]) -1
pages <- as.numeric(substring(lpsnippet, a, b))


for (i in 2:pages){
  hp[i] <- httpGET(paste("http://hellopeter.com/momentum/compliments-and-complaints?country=South%20Africa&pg=", i, sep = ""))

  
}

for (i in 1:pages) {
  linklocations <- gregexpr("<div class=\"td-item2\"><a class=\"fb-link\"", hp[i])
for (j in 1:length(linklocations[[1]])) {

#gets URL
snippet <- substring(hp[i],
                     linklocations[[1]][j],
                     if (j == length(linklocations[[1]])) nchar(hp[i]) else linklocations[[1]][j+1])

# Link
link[j,i] <- substring(snippet, gregexpr("href=\"",snippet)[[1]][1] + 6,gregexpr(" title=\"",snippet)[[1]][1]-2)

# Title
a <- gregexpr("title=\"",snippet)[[1]][1] + 7
b <- min(gregexpr("\">",snippet)[[1]][which(gregexpr("\">",snippet)[[1]] > gregexpr("title=\"",snippet)[[1]][1] + 7)])-1
title[j,i] <- substring(snippet, a, b)

# Time of post
a <- gregexpr("\">\t",snippet)[[1]][1] + 7
b <- min(gregexpr("\t",snippet)[[1]][which(gregexpr("\t",snippet)[[1]] > gregexpr("\">\t",snippet)[[1]][1] + 7)])-1
time[j,i] <- as.Date(trim.leading(substring(snippet, a, b)), format = "%H:%M:%S %A %d %b %y")

}

}



