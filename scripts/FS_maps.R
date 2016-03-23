library(rgdal)
library(zoom)
library(maps)

tst = readOGR('./S_USA.Activity_HazFuelTrt_LN.shp', 
              layer='S_USA.Activity_HazFuelTrt_LN')

ogrListLayers("./sef_lidar.gdb/gdb")

topo = readOGR('./Topography.shp', layer='Topography')
soil = readOGR('./SSURGO_Soils.shp', layer='SSURGO_Soils')    

Stand = readOGR('./Stand.shp', layer='Stand')
Owners = readOGR('./BasicSurfaceOwnership.shp', layer='BasicSurfaceOwnership')

invasive = readOGR('./Current_Invasive_Plants_Inventory.shp', 
                   layer='Current_Invasive_Plants_Inventory')

fire_occ = readOGR('./Monitoring_Trends_in_Burn_Severity__Fire_Occurrence_Locations.shp', 
                   layer='Monitoring_Trends_in_Burn_Severity__Fire_Occurrence_Locations')
fire_poly = readOGR('./Monitoring_Trends_in_Burn_Severity__Burned_Area_Boundaries.shp', 
                    layer='Monitoring_Trends_in_Burn_Severity__Burned_Area_Boundaries')
fire_Rx = readOGR('./FM_RxBurnHistory.shp', layer='FM_RxBurnHistory')

par(mfrow=c(1,2))
plot(fire_poly)
plot(fire_Rx)


llStand = Stand[grep('Longleaf pine', Stand@data$FORESTTYPE), ]

pdf('./Stand.pdf')
plot(Stand)
dev.off()

proj4string(Stand)
geo_prj =  CRS("+proj=longlat +datum=WGS84")
Stand_ll = spTransform(Stand, geo_prj)
llStand_ll = spTransform(llStand, geo_prj)
Owners_ll = spTransform(Owners, geo_prj)
topo_ll = spTransform(topo, geo_prj)
soil_ll = spTransform(soil, geo_prj)

vegplots = read.csv('./CharlestonPlots.csv')
head(vegplots)
vegplots = SpatialPointsDataFrame(coords = vegplots[ , c('Real.Longitude', 'Real.Latitude')],
                                  data=vegplots, coords.nrs = 5:6,
                                  proj4string =  CRS("+proj=longlat +datum=WGS84"))
vegplots$project_num = as.integer(sapply(strsplit(as.character(vegplots$Plot.Code), "-"), function(x)x[1]))

llvegplots = vegplots[grep('Pinus palustris', vegplots$commPrimaryScientific), ]


llyr = as.numeric(sapply(as.character(llvegplots@data$Date), function(x)
                  strsplit(x, '-')[[1]][3]))
table(llyr)

pdf('vegplot_map.pdf')
data(us.cities)
map('county', c('south carolina,charleston', 'south carolina,berkeley'))
map.cities(us.cities, country="SC")
points(vegplots, col='dodgerblue', pch=19)
points(llvegplots, col='green3', pch=19)
points(llvegplots[llyr == 1995, ], col='red')
legend('bottomright', c('longleaf plot', 'other plot'), 
       col=c('green3', 'dodgerblue'), pch=19, bty='n')
dev.off()

## export kmls
writeOGR(Stand_ll, "Stand.kml", "Stand", "KML")
writeOGR(llStand_ll, "llStand.kml", "Stand", "KML")
writeOGR(Owners_ll, "Owners.kml", "Owners", "KML")
writeOGR(topo_ll, "topo.kml", "topo", "KML")
writeOGR(soil_ll, "soil.kml", "soil", "KML")
writeOGR(vegplots, "vegplots.kml", "vegplots", "KML")
writeOGR(llvegplots, "llvegplots.kml", "llvegplots", "KML")

## summary




