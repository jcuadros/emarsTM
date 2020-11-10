install.packages("RSelenium")
library(RSelenium)
library(xml2)
library(jsonlite)
library(tidyverse)
library(rvest)

eMarsdf <- data.frame("ID" = vector(length=1082), "URL" = vector(length=1082), "Title" = vector(length=1082))
webp <- vector(length=217)

rD <- rsDriver(browser="firefox", port=4546L, verbose=F)
remDr <- rD[["client"]]
remDr$open()
remDr$navigate("https://emars.jrc.ec.europa.eu/en/emars/accident/search")

for (i in 1:216) {
  webp[i] <- remDr$getPageSource()
  num_char <- unlist(str_locate_all(webp[i], "PrintDetail"))
  num <- as.numeric(num_char)
  
  for (j in 1:5) {
    eMarsdf[5*(i-1)+j,"ID"] <- str_sub(webp[i],num[j+5]+2,num[j+5]+37)
  }
  
  nextpg <- remDr$findElement("class name", "nextpg")
  nextpg$clickElement()
  Sys.sleep(2)
}

eMarsdf[1081,"ID"] <- "45163465-6cc7-db99-a456-542697aba916"
eMarsdf[1082,"ID"] <- "aa4023ef-1ccf-3a28-ec6d-fb2fed44ed02"


for (i in 1:1082) {
  eMarsdf[i,"URL"] <- paste("https://emars.jrc.ec.europa.eu/api/JsonAccident/",eMarsdf[i,"ID"], sep="")
}

remDr$open()
remDr$navigate(eMarsdf[1,"URL"])
jsonprova <- fromJSON("https://emars.jrc.ec.europa.eu/api/JsonAccident/6923d7d1-c794-9738-05f2-05e41223dd72")

codejson
jsonprova
