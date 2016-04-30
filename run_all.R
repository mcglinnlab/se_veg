
system('Rscript ./scripts/read_config.R')

system('Rscript ./scripts/download_MODIS.R > download_MODIS.log 2>&1', 
       wait=F)

system('Rscript ./scripts/modis_burn_grid.R > modis_burn_grid.log 2>&1', 
       wait=F)
