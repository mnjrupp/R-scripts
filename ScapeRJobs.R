# install.packages("data.table")
# install.packages("XML")

library(data.table)
library(XML)

pages<-c(2:15)

urls <- rbindlist(lapply(pages,function(x){
  url <-paste("https://www.r-users.com/jobs/page/",x,"/",sep = "")
  data.frame(url)
}),fill=TRUE)

jobLocations <-rbindlist(apply(urls,1,function(url){
  doc1 <- htmlParse(url)
  locations <- getNodeSet(doc1,'//*[@id="mainContent"]/div[2]/ol/li/dl/dd[3]/span/text()')
  data.frame(sapply(locations,function(x){xmlValue(x)}))
}),fill=TRUE)

#---------------------------------------------------------------

# using RCurl to scrap website
# Install the RCurl package if necessary
#install.packages("RCurl", dependencies = TRUE)
library("RCurl")

# Install the XML package if necessary
#install.packages("XML", dependencies = TRUE)
library("XML")

# Get first quarter archives
jan09 <- getURL("https://stat.ethz.ch/pipermail/r-help/2009-January/date.html", ssl.verifypeer = FALSE)

jan09_parsed <- htmlTreeParse(jan09,useInternalNodes = TRUE)

author_lines <-xpathApply(jan09_parsed,'//i',xmlValue)

class(author_lines)

# clean out carriage returns
authors <- gsub("\n","",author_lines,fixed=TRUE)

# Top ten most frequent authors

author_counts <- sort(table(authors),decreasing = TRUE)
author_counts[1:10]
table(author_counts)

atag <-xpathApply(jan09_parsed,'//a',xmlValue)
atag.clean <-gsub("\n","",atag,fixed=TRUE)
atag.issue_count <- sort(table(atag.clean),decreasing = TRUE)

atag.issue_count[2:10]


