library(rvest)
library(dplyr)
library(data.table)

setwd("F:/documents/RStudio/ScrapWWW")

url1 <-"https://www.uwhealth.org/findadoctor/"

pages <- c("A","B","C","D","E","D","E","F","G","H","I","J",
           "K","L","M","N","O","P","Q","R","S","T","U","V",
           "W","X","Y","Z")

urls <- rbindlist(lapply(pages,function(x){
  url <-paste(url1,"Search.action?doBrowseName=&browsedLetter=",x,sep = "")
  data.frame(url)
}),fill=TRUE)

doctorl <- list()

for (i in 1:nrow(urls)){
  pageurl <-urls[i,"url"]
  search_page <- read_html(as.character(pageurl$url))
  doctorl[[i]] <- search_page%>%html_nodes(xpath="//span[@class='resultCol doctorColName']")%>%html_text()
  doctorl[[i]] <- gsub("\\t","",doctorl[[i]])
  doctorl[[i]] <- gsub("\\r","",doctorl[[i]])
  doctorl[[i]] <- gsub("\\n","",doctorl[[i]])
  doctorl[[i]] <- gsub("  ","",doctorl[[i]])
  }


doctorl.df <- data.frame(
  doctor_name = unlist(doctorl),
  doctor_title = unlist(doctorl.title),
  doctor_specialty = unlist(doctorl.specialty),
  doctorl_location = unlist(doctorl.location),
  doctorl_city = unlist(doctorl.city)
)

write.csv(doctorl.df,file="UWHealthDocs.csv")
