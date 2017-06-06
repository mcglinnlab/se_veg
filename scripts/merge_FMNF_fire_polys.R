library(sp)
library(rgdal)
library(raster)
library(rgeos)

fire_poly = readOGR('./gis/poly/Monitoring_Trends_in_Burn_Severity__Burned_Area_Boundaries.shp', 
                    layer='Monitoring_Trends_in_Burn_Severity__Burned_Area_Boundaries')
fire_Rx = readOGR('./gis/poly/FM_RxBurnHistory.shp', layer='FM_RxBurnHistory')
#clean up subunit id field called FACTS_S
fire_Rx$FACTS_S = as.character(fire_Rx$FACTS_S)
true = nchar(fire_Rx$FACTS_S) > 19
fire_Rx$FACTS_S[true] = substring(fire_Rx$FACTS_S[true], 1, 19)
fire_haz = readOGR('./gis/poly/SC.Activity_HazFuelTrt_PL.shp', layer='SC.Activity_HazFuelTrt_PL')
# crop fire_haz down to francis marion
fire_haz = crop(fire_haz, extent(-80, -79, 32, 33.5))

# S_USA.Activity_HazFuelTrt_PL is identical to SC.Activity_HazFuelTrt_PL once cropped
#fire_haz2 = readOGR('./gis/poly/S_USA.Activity_HazFuelTrt_PL.shp', layer='S_USA.Activity_HazFuelTrt_PL')
#fire_haz2 = crop(fire_haz, extent(-80, -79, 32, 33.5))

# drop burns without a date
fire_Rx = fire_Rx[!is.na(fire_Rx$BurnDat), ]
fire_Rx$BurnDat = as.Date(fire_Rx$BurnDat, "%Y/%m/%d %H:%M:%S")

fire_haz = fire_haz[!is.na(fire_haz$DATE_ACCOM), ]
fire_haz$DATE_ACCOM = as.Date(fire_haz$DATE_ACCOM, "%Y/%m/%d")

# make sure both in same projection
proj4string(fire_Rx)
proj4string(fire_haz)
fire_haz = spTransform(fire_haz, proj4string(fire_Rx))

# create lists of relevant fields to keep
Rx_nm = c('BurnUnt', 'BurnDat', 'GIS_Acr', 'FACTS_S')
haz_nm = c('SUID', 'ACTIVITY_C', 'ACTIVITY', 'LOCAL_QUAL',
           'PRODUCTIVI', 'DATE_ACCOM', 'COST_PER_U', 'FACTS_ID',
           'TREATMENT_', 'EQUIPMENT', 'METHOD', 'TREATMENT1',
           'STAGE_VALU', 'DATA_SOU_1', 'GIS_ACRES', 'SHAPE_AREA', 
           'SHAPE_LEN')

temp = data.frame(matrix(NA, ncol=length(haz_nm), nrow=nrow(fire_Rx)))
names(temp) = haz_nm
fire_Rx@data = data.frame(fire_Rx@data[ , Rx_nm], temp)
fire_Rx@data$DATE_ACCOM = as.Date(fire_Rx@data$DATE_ACCOM)

temp = data.frame(matrix(NA, ncol=length(Rx_nm), nrow=nrow(fire_haz)))
names(temp) = Rx_nm
fire_haz@data = data.frame(temp, fire_haz@data[ , haz_nm])
fire_haz@data$BurnDat = as.Date(fire_haz@data$BurnDat)

# set slot ids for both datasets so they are non-overlapping
fire_Rx = spChFIDs(fire_Rx, as.character(1:nrow(fire_Rx)))
new_ids = as.character((nrow(fire_Rx) + 1):(nrow(fire_haz) + nrow(fire_Rx)))
fire_haz = spChFIDs(fire_haz, new_ids)

# now carry out rbind
fire = rbind(fire_Rx, fire_haz)

# filter out rows in which date and subunit id match and are not just NAs
Rx_id = NULL
for(i in seq_along(fire@data$FACTS_S)) {
  true = !is.na(fire@data$FACTS_S[i]) & 
         fire@data$FACTS_S[i] != "not found in FACTS"
  if (true)
      Rx_id[i] = with(fire@data, paste(FACTS_S[i], BurnDat[i], sep="_"))
  else
      Rx_id[i] = NA
}
 
haz_id = with(fire@data, paste(SUID, DATE_ACCOM, sep="_"))

# search for matches and only keep the rows from haz 
# which attempt to capture "Actual" burn area
fire = fire[is.na(match(Rx_id, haz_id)), ]

# create new date field 
fire$date = as.Date(with(fire@data, 
                         ifelse(is.na(BurnDat), 
                                as.character(DATE_ACCOM),
                                as.character(BurnDat))))
# create year field
fire$year = as.numeric(format(fire$date, "%Y"))

save(fire, file='./gis/poly/FMNF_fire_poly_df.Rdata')
