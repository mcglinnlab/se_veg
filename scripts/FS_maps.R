library(rgdal)
library(zoom)
library(maps)

ogrListLayers("./S_USA.Activity_HazFuelTrt_PL.gdb")
fuel = readOGR("./S_USA.Activity_HazFuelTrt_PL.gdb",
               layer="Activity_HazFuelTrt_PL")

ex= -79.7 to -80.05
32.5 to 33.5

Stand = readOGR('./Stand.shp', layer='Stand')
Owners = readOGR('./BasicSurfaceOwnership.shp', layer='BasicSurfaceOwnership')


llStand = Stand[grep('Longleaf pine', Stand@data$FORESTTYPE), ]

pdf('./Stand.pdf')
plot(Stand)
dev.off()

proj4string(Stand)
geo_prj =  CRS("+proj=longlat +datum=WGS84")
Stand_ll = spTransform(Stand, geo_prj)
llStand_ll = spTransform(llStand, geo_prj)
Owners_ll = spTransform(Owners, geo_prj)


vegplots = read.csv('./CharlestonPlots.csv')
head(vegplots)
vegplots = SpatialPointsDataFrame(coords = vegplots[ , c('Real.Longitude', 'Real.Latitude')],
                                  data=vegplots, coords.nrs = 5:6,
                                  proj4string =  CRS("+proj=longlat +datum=WGS84"))

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
writeOGR(vegplots, "vegplots.kml", "vegplots", "KML")
writeOGR(llvegplots, "llvegplots.kml", "llvegplots", "KML")

## summary




