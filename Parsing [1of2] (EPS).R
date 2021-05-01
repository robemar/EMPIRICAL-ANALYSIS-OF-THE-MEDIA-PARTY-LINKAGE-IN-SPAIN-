

rm(list = ls())
setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

library(rvest)
library(data.table)
library(stringr)

#define archive folder
archive.folder <- "./ElPaisSanchez/"

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
  
  find.hits <- grepl("Sanchez|Sánchez|Pedro|Moncloa|gobierno|ejecutivo", fulltexttitle, ignore.case = T)
  
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
save(dataframe, file = "ElPaisSanchez.RData")


#install quanteda
#install.packages("quanteda")
#install.packages("tidyverse")
library(quanteda)
library(tidyverse)

#break, no need to run it every time, just load the dataframe

load("D:/Master Denmark/3rd semester/Political Data Science/Final Exam/True Final Exam/1. Downloading/ElPaisSanchez.RData")
data.frame(dataframe$fulltexttitle)

library(dplyr)
dataclean <- dataframe %>% distinct(fulltexttitle, .keep_all = TRUE)

View(dataclean)

#keyword analysis first round (since there is a limit of visualization for kwic three rounds will be done)
kwic(dataclean$fulltexttitle, "Sánchez", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Pedro", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Moncloa", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "moncloa", window = 3, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "Gobierno", window = 2, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "gobierno", window = 2, valuetype = "fixed")
kwic(dataclean$fulltexttitle, "ejecutivo", window = 3, valuetype = "fixed")

#Unconclusive validity of articles using only kwic
#(246, 216, 177)

#Deleting uninteresting obvservations
finaldat <- dataclean[ -c(35, 129, 185, 186, 192, 208, 256, 287, 291, 365, 366, 565, 573, 412, 381, 380, 347, 306, 245, 244, 210, 163, 54, 53, 52, 7, 11, 24, 77, 105, 109, 107, 112, 117, 118, 120, 179, 187, 193, 201, 195, 221, 226, 234, 232, 226, 242, 255, 258, 259, 261, 275, 277, 284, 308, 325, 337, 372, 396), ]

#keyword analysis second round (and double check)

kwic(finaldat$fulltexttitle, "Sánchez", window = 3, valuetype = "fixed")
kwic(finaldat$fulltexttitle, "Pedro", window = 3, valuetype = "fixed")
kwic(finaldat$fulltexttitle, "Moncloa", window = 3, valuetype = "fixed")
kwic(finaldat$fulltexttitle, "Gobierno", window = 2, valuetype = "fixed")
kwic(finaldat$fulltexttitle, "ejecutivo", window = 3, valuetype = "fixed")


finalda <- finaldat[ -c(344, 343, 338, 335, 333, 318, 201, 191, 181, 95, 72), ]

#keyword analysis third round

kwic(finalda$fulltexttitle, "Sánchez", window = 3, valuetype = "fixed")
kwic(finalda$fulltexttitle, "Gobierno", window = 2, valuetype = "fixed")


finald <- finalda[ -c(344, 343, 338, 335, 333, 318, 201, 191, 181, 95, 72), ]

#final data control done manually on the articles not visualized by kwic
finaldata <- finald[ -c(343, 321, 358, 373, 374, 403, 421, 425, 527, 448, 449, 453, 454, 462, 468, 469, 472, 473, 476, 479, 503, 526, 537, 576, 549, 500, 493, 382), ]


#save dataframe
save(finaldata, file = "ElPaisSanchezFinal.RData")


