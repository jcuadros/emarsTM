install.packages("RSelenium")
library(RSelenium)
library(xml2)
library(jsonlite)
library(tidyverse)
library(rvest)


rD <- rsDriver(browser="firefox", port=4546L, verbose=F)
remDr <- rD[["client"]]

# PubChem
remDr$open(silent=T)
remDr$navigate("view-source:https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/smiles/CCCCBr/JSON")
html <- remDr$getPageSource()

writeLines(html[[1]], "1.txt")
xmlscript <- as.character(xml_find_first(read_html(html[[1]]),"//../pre/text()"))
jsonprova <- fromJSON(xmlscript)



