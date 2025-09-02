library(rvest)
library(tidyverse)
library(stringr)
library(rlist)
#We have made 24 HTML files on our own, which lets us access the Tables which give us the data about the players (since in the actual HTML Source page, the table code was commented out).
#These HTML files are of the form "index20xx-20yy.htm".
#We have attached an example HTML file that we used (index2016-2017.htm).
dt <- vector("list",24)
start <- "index"
end <- ".htm"
a <- 2000:2023
b <- 2001:2024
c <- character(24)
for(i in 1:24){
  c[i] <- paste0(start,a[i],'-',b[i],end)
}
for(i in 1:24){
  x <- read_html(c[i])
  x <- x %>% html_table()
  x <- as.data.frame(x)
  dt[[i]] <- x[-(seq(1,length(x$Var.1),26)),]
}
saveRDS(dt, file="Player_Data.Rdata")
p = readRDS("Player_Data.Rdata")

#we are interested in only some columns, so we remove the remaining columns and we name the columns after that based on the website.

dt1 = vector("list",24)

for (i in 1:24)
{
  dt1[[i]] = p[[i]] %>% select(Var.1,Var.2,Var.3,Var.4,Var.5,Playing.Time, Playing.Time.2,Performance,Performance.1,Performance.4,Performance.6,Performance.7)
  colnames(dt1[[i]]) = c("S.No","Player","Nation","Position","Squad","MP","Mins","Goals","Assists","Penalty Kicks","Yellow Cards","Red Cards")
  dt1[[i]]$Nation = gsub("[a-z]", "", dt1[[i]]$Nation)
  dt1[[i]]$Mins = gsub(",","",dt1[[i]]$Mins)
}

saveRDS(dt1, file="Player_Data_Modified.Rdata")

