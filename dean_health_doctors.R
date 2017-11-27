library(rvest)
library(dplyr)

url1 <-"http://www.deancare.com/find-a-doc/"

s <-html_session(url1)

pgform.1 <- html_node(s,"[id=aspnetForm]")%>%html_form()

filled_form <- set_values(pgform.1,
                          "VsMasterPage$ctl00$MainContentPlaceHolder$MainContent$AdvancedSearchFields$searchForm$cities$InternalDropDownList"="Madison")
page.2 <- read_html(submit_form(s,filled_form))


doc_count <- page.2%>%html_node("span.DoctorCount")%>%html_text()
next_url <- page.2%>%html_node("a.Next")%>%xml_attr("href")
doc_count <- gsub("[()]","",doc_count)%>%as.integer()
doc_count <- round(doc_count/10)
# retrieve url for manipulation
 s.url2 <- xml_attrs(xml_child(xml_child(xml_child(page.2, 2), 4), 2))[["action"]]

url2 <- paste("http://www.deancare.com",next_url,sep = "")

i <- 1
# List to hold doctors as we loop through pages
doctorl <- list()
doctorl[[i]] <- page.2%>%html_nodes("span.DrName")%>%html_text()

while(i<doc_count){
    pageN <- read_html(url2)
    i=i+1
    doctorl[[i]] <- pageN%>%html_nodes("span.DrName")%>%html_text()
    next_url <- pageN%>%html_node("a.Next")%>%xml_attr("href")
    url2 <- paste("http://www.deancare.com",next_url,sep = "")
}



