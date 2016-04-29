#!/usr/bin/env Rscript

## Read the local configuration file

CONFIG_FILE <- "./config.R"

# specify local data directories.

if(! file.exists(CONFIG_FILE)) {
    default <- "# Local configuration
                # MODIS
                ftpsite <- 'ftp://user:burnt_data@ba1.geog.umd.edu/Collection51/TIFF/'
                # local storage for downloaded MODIS data. These are big, so may need to be
                # placed on separate filesystem, etc. There must be room for the final
                # EXTRACTED tiff.gz files.
                MODIS_downloaddir <- './gis/ModisData/'
                # Location of burn date data as extracted from MODIS tiffs:
                burndateDir <- './results/burn_dates/'
                # set location of fire interval data as extracted from MODIS
                intervalDir <- './results/fire-intervals'"
    write(default, file=CONFIG_FILE)
} else {
  source(CONFIG_FILE)

  # make sure locations exist
  if(! file.exists(MODIS_downloaddir) ) dir.create(MODIS_downloaddir)
  if(! file.exists(burndateDir) ) dir.create(burndateDir)
  if(! file.exists(intervalDir) ) dir.create(intervalDir)
}

