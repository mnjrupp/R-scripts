# install.packages("data.table")
# install.packages("XML")

library(data.table)
library(XML)

pages<-c(1:15)

urls <- rbindlist(lapply(pages,function(x){
  url <-paste("http://www.r-users.com/jobs/page/",x,"/",sep = "")
  data.frame(url)
}),fill=TRUE)

jobLocations <-rbindlist(apply(urls,1,function(url){
  doc1 <- htmlParse(url)
  locations <- getNodeSet(doc1,'//*[@id="mainContent"]/div[2]/ol/li/dl/dd[3]/span/text()')
  data.frame(sapply(locations,function(x){xmlValue(x)}))
}),fill=TRUE)