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

#-----------------------------------------------------------------------------
# Web scraping example using the "XML" library
#
#library(XML)

url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

# Tell the readHTMLTable function what table we want to read in
# ONly 1 table on the page

poptable <-readHTMLTable(url,which = 1)
head(poptable)

#------------------------------------------------------------------
#,Web scraping using rvest package

library(rvest)
library(dplyr)

google <-read_html("https://news.google.com/news/headlines?hl=en")

google_news<-google%>%html_nodes(".kzAuJ")%>%html_text()
str(google_news)
head(google_news)
google_news

newl <-list()
s <- html_session("https://news.google.com/news/headlines?hl=en")

for(i in google_news[1:15]){
  page <- s%>%follow_link(i)%>%read_html()
  newl[[i]] <- page%>%html_nodes(".ME7ew")%>%html_text()
  
}
newl[i]































