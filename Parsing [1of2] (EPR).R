

rm(list = ls())
setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

library(rvest)
library(data.table)
library(stringr)

#define archive folder
archive.folder <- "./ElPaisRajoy/"

#list all the downloaded files
file.list <- list.files(archive.folder)

#empty list of dataframes
dfs <- list()


#loop over the files present on our archive folder
for (file in file.list) {
  
  #get date and time of the day for each file
  file.date <- str_extract(file, "[[:digit:]]*-[[:digit:]]*-[[:digit:]]*[[:alpha:]]")
  
  #print status
  print(paste("Working on:", file.date))
  
  #load html source code from archive
  load(paste0(archive.folder, file))
  
  #save 'xml_document' 
  p <- read_html(to.save)
  
  #get titles
  fulltexttitle <- p %>% html_nodes(".articulo-titulo a") %>% html_text(trim = TRUE)
  
  find.hits <- grepl("Rajoy|Mariano|Moncloa|gobierno|ejecutivo", fulltexttitle, ignore.case = T)
  
  fulltexttitle <- fulltexttitle[find.hits]
  
  #get link for the full article
  fulltextlink <- p %>% html_nodes(".articulo-titulo a") %>% html_attr("href")
  fulltextlink <- fulltextlink[find.hits]
  
  if (length(fulltexttitle) == 0) next
  
  #create number for each page
  iteration <- (1 + length(dfs))
  
  #creating a dataframe
  dfs[[iteration]] <- data.frame(
        fulltexttitle=fulltexttitle, 
        fulltextlink=fulltextlink,
        iteration=iteration, 
        file.date=file.date, 
        stringsAsFactors = F)
}

dataframe <- as.data.frame(rbindlist(dfs))

View(dataframe)

# save dataframe
save(dataframe, file = "ElPaisRajoy.RData")


#install quanteda
#install.packages("quanteda")
#install.packages("tidyverse")
library(quanteda)
library(tidyverse)

#break, no need to run it every time, just load the dataframe

load("D:/Master Denmark/3rd semester/Political Data Science/Final Exam/True Final Exam/1. Downloading/ElPaisRajoy.RData")
data.frame(dataframe$fulltexttitle)

library(dplyr)
dataclean <- dataframe %>% distinct(fulltexttitle, .keep_all = TRUE)

View(dataclean)

#keyword analysis (1st roudn)
kwic(dataclean$fulltexttitle, "Rajoy", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Mariano", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Moncloa", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "moncloa", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Gobierno", window = 2, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "gobierno", window = 2, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "ejecutivo", window = 3, valuetype = "fixed")

#Unconclusive validity of articles using only kwic
#(246, 216, 177)

#Deleting uninteresting obvservations
finaldat <- dataclean[ -c(91, 109, 159, 342, 304, 253, 207, 227, 186, 17, 18, 79, 84, 1, 101, 172, 311, 3, 11, 12, 13, 14, 15, 16, 19, 21, 29, 46, 51, 55, 56, 59, 61, 62, 65, 67, 72, 78, 82, 87, 93, 95, 106, 107, 121, 126, 128, 129, 144, 149, 153, 167, 168, 204, 209, 215, 216, 218, 225, 243, 248, 252, 261, 267, 68, 122, 280), ]

#keyword analysis second round (and double check)

kwic(finaldat$fulltexttitle, "Rajoy", window = 3, valuetype = "fixed")
kwic(finaldat$fulltexttitle, "Gobierno", window = 2, valuetype = "fixed")
kwic(finaldat$fulltexttitle, "ejecutivo", window = 3, valuetype = "fixed")


finaldata <- finaldat[ -c(131, 211, 219, 222, 228, 236, 242, 247, 251, 253, 254, 272), ]


#save dataframe
save(finaldata, file = "ElPaisRajoyFinal.RData")


