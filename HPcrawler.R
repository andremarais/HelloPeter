require(RCurl)
require(XML)
require(wordcloud)
require(plyr)
require(ggplot2)

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
post <- data.frame()
postbody <- data.frame()
response.date <- data.frame()
all.data <- list()
nature <- data.frame()

# Momentum
MomHealth <- "http://hellopeter.com/momentum-health/compliments-and-complaints?country=South%20Africa&pg="
Momentum <- "http://hellopeter.com/momentum/compliments-and-complaints?country=South%20Africa&pg="
MSTI <- "http://hellopeter.com/momentum-short-term-insurance/compliments-and-complaints?country=South%20Africa&pg="

# Discovery
DiscHealth <- "http://hellopeter.com/discovery-health/compliments-and-complaints?country=South%20Africa&pg="
DiscLife <- "http://hellopeter.com/discovery-life/compliments-and-complaints?country=South%20Africa&pg="
DiscInsure <- "http://hellopeter.com/discovery-insure/compliments-and-complaints?country=South%20Africa&pg="

# Old Mutual
OM <- "http://hellopeter.com/old-mutual/compliments-and-complaints?country=South%20Africa&pg="

#Liberty
LibertyLife <- "http://hellopeter.com/liberty-life/compliments-and-complaints?country=South%20Africa&pg="

# Metropolitan
Metro <- "http://hellopeter.com/metropolitan-life/compliments-and-complaints?country=South%20Africa&pg="

#Outsurance
Outsurance <- "http://hellopeter.com/outsurance/compliments-and-complaints?country=South%20Africa&pg="

#MiWay
Miway <- "http://hellopeter.com/miway/compliments-and-complaints?country=South%20Africa&pg="




insurars <- c(MomHealth, Momentum, MSTI,
              DiscHealth, DiscLife, DiscInsure,
              OM,
              LibertyLife,
              Metro,
              Outsurance,
              Miway)

insurar.names <- c("Momentum Health", "Momentum", "Momentum Short Term",
                   "Discovery Health", "Discovery Life", "Discovery Insure",
                   "Old Mutual",
                   "Liberty",
                   "Metropolitan",
                   "OutSurance",
                   "MiWay")


## BEGIN HIERSO POES!!!
system.time(
  #for (h in 1:length(insurars)) {
  for (h in 1:1) {
    
    
    hp[1] <- httpGET(paste(insurars[h], 1, sep = ""))
    
    
    
    #gets last page
    lpsnippet <- substring(hp[1], gregexpr(">>", hp[1])[[1]][1], gregexpr(">>", hp[1])[[1]][2])
    if(is.na(lpsnippet)) pages <- 1 else {
      a <- as.numeric(regexpr("pg=",lpsnippet)[[1]] + attr(gregexpr("pg=",lpsnippet)[[1]], "match.length"))
      b <- min(gregexpr("\"",lpsnippet)[[1]][which(gregexpr("\"",lpsnippet)[[1]] > gregexpr("pg=",lpsnippet)[[1]][1])]) -1
      pages <- as.numeric(substring(lpsnippet, a, b))
    }
    
    
    for (i in 1:pages){
      hp[i] <- httpGET(paste(insurars[h], i, sep = ""))
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
        c <- as.character(as.Date(trim.leading(substring(snippet, a, b)), format = "%H:%M:%S %A %d %b %y"))
        d <- str_match(snippet, "[0-9]{2}:[0-9]{2}:[0-9]{2}")[1,1]
        time[j,i] <- as.POSIXct(paste(c, d), tz = "GMT")
        
        
        # Type of post
        if (regexpr("complaints/", link[j,i], ignore.case = T) != -1) type[j,i] <- "Complaint" 
        if (regexpr("compliments/", link[j,i], ignore.case = T) != -1) type[j,i] <- "Compliment" 
        if (regexpr("complaints-to-compliments/", link[j,i], ignore.case = T) != -1) type[j,i] <- "Conversion"
        
        # Response time
        if (!is.na(link[j,i])) {
          post[j,i] <- httpGET(link[j,i])
          a <- min(gregexpr("SUPPLIER'S RESPONSE", post[j,i])[[1]])
          if (a == -1)  response.date[j,i] <- 0 else {
            
            b <- min(gregexpr("[0-9]{2}:[0-9]{2}:[0-9]{2}", substring(post[j,i], a, a + 500))[[1]])
            c <- min(gregexpr("</td>", substring(post[j,i], a + b , a + b + 60))[[1]])
            d <- substring(post[j,i], a + b -1, a +  b + c-2)
            e <- as.Date(as.character(d), format = "%H:%M:%S | %a %d %b %y")
            f <- str_match(substring(post[j,i], a, a + 500), "[0-9]{2}:[0-9]{2}:[0-9]{2}")[1,1]
            response.date[j,i] <- as.POSIXct(paste(e, f), tz = "GMT")
            
            #Nature of post
            if (type[j,i] == "Compliment" ) look.for <- "NATURE"  else look.for <- "PROBLEM"
            a <- min(gregexpr(look.for, post[j,i], ignore.case = F)[[1]]) + attr(gregexpr(look.for, post[j,i], ignore.case = F)[[1]], "match.length")[1]
            b <- min(gregexpr("tbl-txt-hd-nb\"", substring(post[j,i], a, a + 200))[[1]]) + attr(gregexpr("tbl-txt-hd-nb\"", substring(post[j,i], a, a + 200))[[1]], "match.length")[1]
            c <- min(gregexpr("</h3>", substring(post[j,i], a + b, a + b + 200))[[1]]) 
            nature[j,i] <- substring(post[j,i], a + b, a+b+c -2)
            
            # acutal post
            a <- gregexpr("shade border justify\">\n\t\t\t\t", post[j,i])
            b <- gregexpr("<div class=\"report-action\">", post[j,i])
            post[j,i] <- substring(post[j,i], as.numeric(a) + attr(a[[1]], "match.length"), as.numeric(b) -1)
            post[j,i] <- gsub("\n", "", post[j,i])
            post[j,i] <- gsub("\t", "", post[j,i])
            post[j,i] <- stripwhitespace(post[j,i])
            
            
            
            
            
            
          }             
        }
        
        
        
        
        
        print(c(h,i,j))
      }
    }
    
    time.vector <- as.vector(as.matrix(time))
    type.vector <- as.vector(as.matrix(type))
    response.date.vector <- as.vector(as.matrix(response.date))
    nature.vector <- as.vector(as.matrix(nature))
    post.vector <- as.vector(as.matrix(post))
    
    hp.df <- data.frame(cbind(time.vector, type.vector, response.date.vector, nature.vector, post.vector))
    colnames(hp.df) <- c("post.date", "type", "response.date", "nature", "content")
    hp.df <- hp.df[!is.na(hp.df$post.date),]
    
    all.data[[h]] <- hp.df
    saveRDS(hp.df, file = paste(insurar.names[h],".RDS", sep = ""))
    
  }
)





