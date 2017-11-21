
#---------------------------------------------------------------------------------
# Authentication and webscraping comments in a Wordpress website
# * Note: You will need to add your domain name. This was used on a Wordpress
# * site. This will log you in and take you to your comments page to load 
# * into a table for exporting.
#---------------------------------------------------------------------------------


library(rvest)
library(dplyr)

mypass    <-"password"
mylogin   <-"admin"

url1       <-"http://domain_name/wp-login.php"

# Start a session
pgsession <-html_session(url1)

# grab The form and fill it with username:password
pgform    <-html_form(pgsession)[[1]]

filled_form <- set_values(pgform,
                          "log" = mylogin, 
                          "pwd" = mypass)

# Submit form and jump to comment page
submit_form(pgsession,filled_form)
comment_list <- jump_to(pgsession, "http://domain_name/wp-admin/edit-comments.php")

# Read the comments using pipes
page <- read_html(comment_list)

comments <-page%>%html_nodes("div.comment-author")%>%html_text()

my_tables <- html_nodes(page,"table")[[1]]
comment_table <- html_table(my_tables)

# Don't need the first column
comment_table.1 <- comment_table[,-1]

# save to csv file
write.csv(comment_table.1,row.names = FALSE,"comments_saved.csv")















