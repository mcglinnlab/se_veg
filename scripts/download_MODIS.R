#!/usr/bin/env Rscript

## NOTES from modis userguide ##
################################

## A user-friendly geotiff version of the MCD45 product is derived from the
## standard MCD45A1 hdf version by University of Maryland.

## The geotiffs are reprojected in Plate-Carree projection and cover a set of
## sub-continental windows (figure 2).

## A table containing the regions covered and bounding coordinates of the 24
## windows is available in Appendix III.

## a) MCD45monthly = monthly Geotiff version of MCD45A1
## b) A2000306 = year and Julian date of the starting day of the month covered by
## the product: 306 is the Julian date of Nov 1, hence 2000306 means that the
## product covers November 2000.
## c) Win01 = spatial extent: the file covers window 1 (Alaska)
## d) 005 = version identifier. 005 indicates Collection5

## e) burndate/ba_qa = content of the file: unlike hdf, geotiff files contain a
## single layer. At the moment, two layers of the original product are available
## as geotiffs: "burndate" and "QA". See 3.3.3 for details.

## DATA

## Burn date

##   0 - unburned (2 bytes): Approximate Julian day of burning from eight days
##   before the beginning of the month to eight days after the end of the month,
##   or a code indicating unburned areas, snow, water, or lack of data.

##   1-366 - approximate Julian day of burning
##   900 - snow or high aerosol
##   9998 - water bodies (internal)
##   9999 - water bodies (seas and oceans)
##   10000 - not enough data to perform inversion throughout the period
##   BA pixel QA

## 1 - most confidently detected pixels, regardless of direction in time (forward,
##     backward or both), passing test (4) described in appendix 1. (1 byte):
##     Confidence of the detection (1)
## 2 - pixels where backward and forward direction in time predict the same
##     change, passing test (5) described in appendix I.
## 3 - pixels selected in the first stage of the contextual analysis.
## 4 - pixels selected in the second stage of the contextual analysis. Unlike the
##     HDF version of the product, the GEOTIFFS do not include any overlap between
##     consecutive months.

library(RCurl)
library(R.utils)

DownloadFireDat.s <- function(ftpsite, Downloaddir, whatwins = "all",
                              whatyears = "getyears"){
  # This aim of this script is to download the burned area data from the .Ftp
  # site. is is a simplified version of the code to be found at
  # "MODIS_downloadBurnedAreafromFTP.r" it requires the user to input the
  # ftpsite, the download directory, the windows required (default is all), and
  # the years for which data are requierd (default is all years)
  if(whatwins == "all") {
    windows <- sprintf("%02d", 1:24) # 24 windows
  } else {
    windows <- whatwins
  }

  which <- NULL
  for(i in 1:length(windows)){
    win <- paste("Win", windows[i], sep = "")
    ### create the folder for the window if necessary
    if (! file.exists(paste(Downloaddir, win, sep = "/"))){
      dir.create(file.path(Downloaddir, win))    
    }  
    if(whatyears[1] == "getyears"){    # IF you want to get ALL the years
      years <- getURL(paste(ftpsite, win, "/", sep = ""),ftp.use.epsv = FALSE, dirlistonly = TRUE)
      years <- strsplit(years, "\r*\n")[[1]]
    }
    if(whatyears[1] != "getyears") years <- whatyears
    #use getURL to download the requisite data
    for(j in 1:length(years)){
      ### test this for just one folder
      inputURL <- paste(ftpsite, win, "/", years[j], "/", sep = "")
      filenames <- try(getURL(inputURL, ftp.use.epsv = FALSE, dirlistonly = TRUE))
      #filenames <- getURL(inputURL, ftp.use.epsv = FALSE, dirlistonly = TRUE)
      Sys.sleep(2)
      files <- strsplit(filenames, "\r*\n")[[1]]
      # use this code if you only want to download one type of burned area data
      files <- files[grep("burndate", files)]   
      # to make sure you dont download all over again
      currentfiles <- list.files(file.path(Downloaddir, win))
      files <- setdiff(files, currentfiles)
      filenames = paste(inputURL, files, sep = "") 
      if(length(files)>0){
        which <- c(which, files)
        for(k in 1:length(filenames)){          
            try(
                download.file(filenames[k], method = "auto",
                              destfile = paste(Downloaddir, "/", win, "/",
                                               files[k], sep = ""))
            )
            Sys.sleep(2)
          
          #x <-getURL("ftp://ftp.ba1.geog.umd.edu/TIFF/Win04/2010/MCD45monthly.A2010244.Win04.005.burndate.tif.gz", opts = opts)
          #gunzip(paste(tempdir, files[k], sep = ""), remove = FALSE)
        }    
      }
    }
  }  
  ## to check whether all the data are there....
  for(i in 1:length(windows)){  
    files <- dir(paste(Downloaddir, "/", "Win", windows[i], sep = ""))
    # use this code if you only want to download one type of burned area data
    files <- files[grep("burndate", files)]   
    files <- gsub(".gz", "", files)
    dates <- gsub("MCD45monthly.A", "", files); dates <- substr(dates, 1, 4)
    cat("window", windows[i], "\n")
    cat(paste(names(table(dates)), ":", table(dates), sep = ""), "\n", sep = " ")
  }
}

# get local directories:
source("./scripts/read_config.R") # need MODIS_downloaddir

# download the fire data from the MODIS ftp site
DownloadFireDat.s(ftpsite, MODIS_downloaddir, whatwins = "03", whatyears = "getyears")

# extract compressed files using bash:
system(paste("find", MODIS_downloaddir , "-name '*.gz' -exec gunzip '{}' \\;"))

# check that all files were downloaded:
#files = list()
#window = '03'
#win <- paste("Win", window, sep = "")
#years = 2000:2015
#for(j in 10:12){
#      ### test this for just one folder
#      inputURL <- paste(ftpsite, win, "/", years[j], "/", sep = "")
#        filenames <- try(getURL(inputURL, ftp.use.epsv = FALSE, dirlistonly = TRUE))
#        #filenames <- getURL(inputURL, ftp.use.epsv = FALSE, dirlistonly = TRUE)
#        Sys.sleep(4)
#      files[[j]] <- strsplit(filenames, "\r*\n")[[1]]
#      files[[j]] <- files[[j]][grep("burndate", files[[j]])]
#}