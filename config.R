# Local configuration
                # MODIS
                ftpsite <- 'ftp://user:burnt_data@ba1.geog.umd.edu/Collection51/TIFF/'
                # local storage for downloaded MODIS data. These are big, so may need to be
                # placed on separate filesystem, etc. There must be room for the final
                # EXTRACTED tiff.gz files.
                MODIS_downloaddir <- './gis/ModisData/'
                # Location of burn date data as extracted from MODIS tiffs:
                burndateDir <- './results/burn_dates/'
                # set location of fire interval data as extracted from MODIS
                intervalDir <- './results/fire-intervals'
