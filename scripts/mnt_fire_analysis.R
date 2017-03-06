library(raster)
library(rgdal)
library(zoom)
library(maps)

fire_usa = readOGR('./gis/poly/S_USA.Activity_HazFuelTrt_PL.shp', layer='S_USA.Activity_HazFuelTrt_PL')
# crop fire_usa down to Southern Apps focus
xmin =-86.220703125
ymin = 31.5785354265
xmax = -75.0366210938
ymax = 37.0551771067
e = extent(matrix(c(xmin, ymin, xmax, ymax), 2, 2))
fire_usa = crop(fire_usa, e)

mnt_plot = read.csv('./data/CVS-PlotLocations-Mts.csv')
# drop plots with poor spatial resolution
mnt_plot = subset(mnt_plot, !is.na(Location.Accuracy))
mnt_plot = subset(mnt_plot, Location.Accuracy < 200)
# convert date to better format
mnt_plot$Observation.Start.Date = as.Date(mnt_plot$Observation.Start.Date,
                                          "%d-%b-%Y")

mnt_plot = SpatialPointsDataFrame(coords = mnt_plot[ , c('Real.Longitude', 'Real.Latitude')],
                                  data=mnt_plot, 
                                  proj4string = CRS(proj4string(fire_usa)))

mnt_fire = extract(fire_usa, mnt_plot)

mnt_fire = mnt_fire[!is.na(mnt_fire$poly.ID), ]

write.csv(mnt_fire, file='./data/mnt_fire_extracted.csv', row.names=F)
