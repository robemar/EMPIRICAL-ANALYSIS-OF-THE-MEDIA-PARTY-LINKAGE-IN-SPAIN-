
rm(list = ls())
setwd("D:/Master Denmark/3rd Semester/Political Data Science/Final Exam/True Final Exam/1. Downloading")

library(RCurl)
library(stringr)
library(rvest)
library(data.table)

# base url for scraping !!!IMPORTANT!!! this url is not the one displayed on the original website, if the link is downloaded as a http instead of https the required information won't be present on the html source code.
base.url <- "https://www.elmundo.es/elmundo/hemeroteca/"

# create main archive
if(!dir.exists("ElMundoarticles")) dir.create("ElMundoarticles")

# create sub-archive for search results
archive.folder <- "./ElMundoarticles/search_archive/"
if(!dir.exists(archive.folder)) dir.create(archive.folder)


# create a loop that goes from february until september
for (yr in 2018:2018) { #creatin of year variable not for loop need but for dates
  print(paste("Starting on year", yr)) # print year for status update
  
  for (mo in 2:9) { 
    print(paste("Starting on month", mo)) # print month for status update
    
    # months with 31 days
    if (mo %in% c(3, 5, 7, 8)) mo.days <- 31
    
    # months with 30 days
    if (mo %in% c(4, 6, 9)) mo.days <- 30
    
    # february
    if (mo == 2) {
      if (yr %in% c(2008, 2012, 2016)) {
        # nnecessary but undisturbing element from the original code
        mo.days <- 29
      } else {
        # normal year
        mo.days <- 28
      }
    }
    
    for (dy in 1:mo.days) { #loop over selected number of days
      
      # add zeros to day and month for date in url
      if (str_length(dy) == 1) dy <- paste0(0, dy)
      if (str_length(mo) == 1) mo <- paste0(0, mo)
      
      # construct data for url
      crawl.date <- paste0(yr, mo, dy)
      
      # check and create a scraping date
      scraping.date <- as.Date(crawl.date, "%Y%m%d") #convert crawl.date to date format

      for (tod in c("m", "t", "n")) {
        # build url to fetch
        final.url <- paste0(base.url, yr, "/", mo, "/", dy, "/", tod, "/index.html")
        
        print(final.url)
        
        #check archive for file
        file.name <- paste0(archive.folder, "search_page_", scraping.date, tod, ".RData")
        if(file.exists(file.name)) {
          next
        } else {
          # print status update
          print(paste("Downloading:", scraping.date, tod))
          
          to.save <- getURL(final.url) # download html source code
          save(to.save, file = file.name) # save downloaded source code
          Sys.sleep(6) # break for 6 seconds 
        }
      }
    }
  }
}

#the downloaded files will be divided manually into specific document folders depending on the executive and newspaper which they belong