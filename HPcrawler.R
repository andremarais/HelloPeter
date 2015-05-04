require(RCurl)
require(XML)
require(wordcloud)
require(plyr)

trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


hp <- c()
link <- data.frame()
title <- data.frame()
time <- data.frame()
type <- data.frame()
postbody <- data.frame()
response.date <- data.frame()

txt <- "\\(Supplier name changed from.*\\)"

hp[1] <- httpGET("http://hellopeter.com/momentum-health/compliments-and-complaints?country=South%20Africa&pg=1")

momentum[1] <- httpGET("http://hellopeter.com/momentum/compliments-and-complaints?country=South%20Africa&pg=1")

MSTI[1] <- httpGET("http://hellopeter.com/momentum-short-term-insurance/compliments-and-complaints?country=South%20Africa&pg=")


#gets last page
lpsnippet <- substring(hp[1], gregexpr(">>", hp[1])[[1]][1], gregexpr(">>", hp[1])[[1]][2])
a <- as.numeric(regexpr("pg=",lpsnippet)[[1]] + attr(gregexpr("pg=",lpsnippet)[[1]], "match.length"))
b <- min(gregexpr("\"",lpsnippet)[[1]][which(gregexpr("\"",lpsnippet)[[1]] > gregexpr("pg=",lpsnippet)[[1]][1])]) -1
pages <- as.numeric(substring(lpsnippet, a, b))


for (i in 1:pages){
  hp[i] <- httpGET(paste("http://hellopeter.com/momentum-health/compliments-and-complaints?country=South%20Africa&pg=", i, sep = ""))
print(i)
  
}


# Run this

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
  time[j,i] <- as.character(as.Date(trim.leading(substring(snippet, a, b)), format = "%H:%M:%S %A %d %b %y"))
  
  # Type of post
  if (regexpr("complaints/", link[j,i], ignore.case = T) != -1) type[j,i] <- "Complaint" 
  if (regexpr("complaints-to-compliments/", link[j,i], ignore.case = T) != -1) type[j,i] <- "Convertion"
  if (regexpr("compliments/", link[j,i], ignore.case = T) != -1) type[j,i] <- "Compliment" 

  # Response time
  if (!is.na(link[j,i])) {
    post <- httpGET(link[j,i])
    a <- min(gregexpr("SUPPLIER'S RESPONSE", post)[[1]])
    if (a == -1)  response.date[j,i] <- 0 else {
       
    b <- min(gregexpr("[0-9]{2}:[0-9]{2}:[0-9]{2}", substring(post, a, a + 500))[[1]])
    c <- min(gregexpr("</td>", substring(post, a + b , a + b + 60))[[1]])
    d <- substring(post, a + b + 10, a +  b + c + 8)
    e <- as.Date(as.character(d), format = "%a %d %b %y")
    response.date[j,i] <- as.character(e)}
                

  }
    

      
  print(c(i,j))
  }
}

link <- data.frame()
title <- data.frame()
time <- data.frame()
type <- data.frame()
postbody <- data.frame()
response.date <- data.frame()


time.vector <- as.vector(as.matrix(time))
type.vector <- as.vector(as.matrix(type))
response.date.vector <- as.vector(as.matrix(response.date))

hp.df <- data.frame(cbind(time.vector, type.vector, response.date.vector))
colnames(hp.df) <- c("post.date", "type", "response.date")
hp.df <- hp.df[!is.na(hp.df$post.date),]

hp.df$response.date <- as.Date(hp.df$response.date)
hp.df$post.date <- as.Date(hp.df$post.date)
hp.df$response.time <- hp.df$response.date - hp.df$post.date
hp.df$post.date.month <- format(hp.df$post.date, format = "%Y-%m")
ave.time.pm <- aggregate(data = hp.df, response.time ~ post.date.month, FUN = mean)

hp.monthly <- count(hp.df, c('post.date.month','type'))
hp.count <- count(hp.df, c('post.date', 'type'))

ggplot()+ 
  geom_bar(data = hp.monthly, aes(x = post.date.month, y = freq, fill = type), stat = "identity", position = "dodge")+ 
  scale_fill_brewer(palette = "Set1")
#
ggplot() + 
  geom_bar(data = ave.time.pm, aes(x = post.date.month, y = as.numeric(response.time), fill = as.numeric(response.time)), stat = "identity")

