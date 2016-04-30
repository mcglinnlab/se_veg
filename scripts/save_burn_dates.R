#!/usr/bin/env Rscript

# Sally Archibald
# sally.archibald1@gmail.com
# May 2015

## this code is to:

# Associated MODIS burn dates with gbif location coordinates (cleaned and
# extracted using gbif_to_sp_data.R). The raw burn dates are stored in separate
# files per MODIS window in the intervalDir directory.

# INSTRUCTIONS:

# 1. Run gbif_to_sp_data.R if necessary. This saves two objects to binary cahced
# .rds files. Thes are then re-read at the beginning of this script.

library(sp)
library(maptools)
library(raster)
library(rgdal)
library(R.utils)

###############################################################################
# Functions
###############################################################################

# this function works one window at a time - it loops through all the raster
# layers (12 for each year) and pulls out any situation where a location point
# has burned and records the date it requires an input dataset of points (with
# lats, longs and a unique identifier), and an indiction of which window to use
# (i) and a list of windows for which we have point data (window), and
# information on where the modis data are that were extracted
getBurnDates <- function(pnts, i, window = window, data_dir = MODIS_downloaddir){
  #dat <- pnts[pnts$WinName == window[i],]
  dat = pnts
  inputfiles <- dir(file.path(data_dir, paste("Win", window[i], sep="")))
  # these are the files that have already been extracted using the
  # "download_MODIS.R. Now comes the tricky part - to add the burn date for
  # each month to a dataset of points. read in the MODIS layers one at a time
  # and extract the values for all the points to a file. however in order to
  # make your open-ended (censored) dataset you need to: create a new dataset
  # and populate it with the beginning and end dates of the period of interest

  # first get the year and month for each input data
  years <- unlist(strsplit(inputfiles, "MCD45monthly.A"))
  years <- years[years != ""]
  years <- unlist(strsplit(years, paste(".Win", window[i], ".051.burndate.tif", sep = "")))
  dates <- strptime(years, "%Y%j")
  # check that all the months are there
  a <- dates[2:length(dates)] - dates[1:(length(dates)-1)]
  if(length(a[a > 31]) > 1) paste("problem with input data - missing files")
  # make a begining and end date
  start <- as.character(min(dates))
  end <- as.character(max(dates))
  # create the dataset - basically we need the unique lat-long, a date, and an
  # indication of whether it is the first/laste date or not
  Bdates <- matrix(NA, ncol = 3, nrow = nrow(dat)*length(inputfiles))

  # record that this is the first/last date (i.e. an open interval):
  Bdates[1:nrow(dat),] <- cbind(dat$latlongs, rep(start, nrow(dat)), rep(0, nrow(dat)))
  nn <- nrow(dat)+1   # make a counter that records the next empty row
  Bdates[nn:(nn+nrow(dat)-1),] <- cbind(dat$latlongs, rep(end, nrow(dat)), rep(0, nrow(dat)))
  nn <- nrow(dat)*2+1

  getstart <- Sys.time()
  for(k in 1:length(inputfiles)){ # for every input burned area layer
      a <- raster(file.path(data_dir, paste("Win", window[i], sep=""), inputfiles[k]))
      year <-  substring(years, 1, 4)[k]
      b <- raster::extract(a, dat)   # get
      # the values for these points. now you need to do some cleaning.
      # Basically, only the points that have a date value attached need to be
      # kept. at the moment, ignore all other values except dates... maybe
      # later add code to extract points that are classed as water etc
      id <- dat$latlongs[b > 0 & b < 367] 
      b <- b[b > 0 & b < 367]

      if(length(b) > 0) {    #i.e. if there are some dates that are valid, add
                            #them to the new dataset.
          bdate <- strptime(paste(rep(year, length(b)), b), "%Y %j")
          Bdates[nn:(nn+length(b)-1),] <- cbind(id, as.character(bdate), rep(1, length(b)))
          nn <- nn + length(b)
      }
      cat("Win", window[i], ": at file", k,"of", length(inputfiles),
          "files. total time taken: ", round(Sys.time()-getstart,0), "\n")
  }

  t <-  max(c(1:nrow(Bdates))[!is.na(Bdates[,1])])
  Bdates <- Bdates[1:(t-1),]
  Bdates <- data.frame(Bdates)
  colnames(Bdates) <- c("latlong", "date", "burned")
  return(Bdates)
}

###############################################################################
# Main script
###############################################################################

# get external data locations:
source("./scripts/read_config.R")

dat = read.csv('./data/CharlestonPlots.csv')
dat$latlongs = paste(dat$Real.Latitude, dat$Real.Longitude, sep = "_")
dat_sp = SpatialPointsDataFrame(coords = dat[ , c('Real.Longitude','Real.Latitude')],
                                dat)

# pull out a list of windows for which we need data:
window <- '03'

# Get all dates that a pixel burned for each window and save each file in
# burndateDir
Bdates <- getBurnDates(dat_sp, 1, window, MODIS_downloaddir)
Bdates = Bdates[Bdates$burned == 1, ]

table(table(Bdates$latlong))
tapply(Bdates$burned, Bdates$latlong, length)

save(Bdates, file = file.path(burndateDir,
                              paste("Bdates_", window, ".Rdata", sep = "")))