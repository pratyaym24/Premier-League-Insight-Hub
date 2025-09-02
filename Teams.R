library(rvest)
library(tidyverse)
library(stringr)
library(rlist)

#We are using character variables to store the "common" strings of the url we would like to use
common <- "https://fbref.com"
html <- read_html("https://fbref.com/en/comps/9/history/Premier-League-Seasons")
common2 <- "https://www.transfermarkt.co.in/premier-league/einnahmenausgaben/wettbewerb/GB1/plus/0?ids=a&sa=&saison_id="
common3 <- "&saison_id_bis="
common4 <- "&nat=&pos=&altersklasse=&w_s=&leihe=&intern=0"
a <- 2000:2023
b <- 2001:2024
u <- list(0)

#We are taking the money spent by each team in the Premier league over the years and cleaning the data
for(j in 1:24){
  link1 <- paste0(common2,a[j],common3,a[j],common4)
  h <- read_html(link1)
  x <- h %>% html_elements(".rechts.hauptlink.greentext") %>% html_text()
  y <- h %>% html_elements(".rechts.hauptlink.redtext") %>% html_text()
  x <- gsub("-","₹0 Cr",x)
  x <- gsub("₹","",x)
  x <- gsub(",","",x)
  k <- substr(x,str_length(x),str_length(x))
  for(i in 1:length(k)){
    if(k[i] == "L"){
      x[i] = '0'
    }
  }
  x <- as.integer(gsub(" Cr","",x))
  y <- gsub("-","₹0 Cr",y)
  y <- gsub("₹","",y)
  y <- gsub(",","",y)
  k <- substr(y,str_length(y),str_length(y))
  for(i in 1:length(k)){
    if(k[i] == "L"){
      y[i] = '0'
    }
  }
  y <- as.integer(gsub(" Cr","",y))
  na <- h %>% html_elements(".hauptlink.no-border-links") %>% html_text()
  d <- data.frame("Names" = na, "Spent" = y-x)
  d <- as_tibble(d)
  u <- append(u,d)
}
u <- u[-1]

#Here we are taking the team stats over the years in the Premier League
link <- html %>% html_elements(".left a") %>% html_attr("href")
link <- link[seq(3,96,3)]
for(i in 1:length(link)){
  link[i] <- paste0(common,link[i])
}
x <- read_html(link[1])
y <- x %>% html_table()
y <- y[1]
v <- list(y[1])
Sys.sleep(1)

for(i in 2:24){
  x <- read_html(link[i])
  y <- x %>% html_table()
  y <- y[1]
  v <- append(v,y[1])
}

#Saving the data collected as 2 different .RData files
saveRDS(v, file="Team_Data.Rdata")
saveRDS(u, file ="Team_Spends.RData")
