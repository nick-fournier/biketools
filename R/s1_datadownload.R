#Reads files from source, downloading any not already downloaded.

#Required packages
library(XML)
library(RCurl)
# Extracting the file names to download ####
url <- 'https://s3.amazonaws.com/hubway-data/'
s3list <- xmlTreeParse(getURL(url), useInternalNodes = TRUE)
s3list <- xmlToList(s3list)
s3list <- unlist(s3list)
s3list <- as.character(s3list[grepl(".zip|.csv",s3list)])

# Downloading the files ####
sapply(s3list, function(x) {
  source <- paste(url, x, sep = "")
  dest <- paste('./data/zips/', x, sep="")
  if(!file.exists(dest)) {
    print(paste("downloading",x))
    try(download.file(source, dest))
  }
  })


rm(list=ls())
