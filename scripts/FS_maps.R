library(rgdal)
library(zoom)
library(maps)

source('./scripts/functions.R')

topo = readOGR('./gis/Topography.shp', layer='Topography')
soil = readOGR('./gis/SSURGO_Soils.shp', layer='SSURGO_Soils')    

Stand = readOGR('./gis/Stand.shp', layer='Stand')
Owners = readOGR('./gis/BasicSurfaceOwnership.shp', layer='BasicSurfaceOwnership')

invasive = readOGR('./gis/Current_Invasive_Plants_Inventory.shp', 
                   layer='Current_Invasive_Plants_Inventory')

tst = readOGR('./gis/MCD45monthly.A2000092.Win03.051.burndate.shp', 
              'MCD45monthly.A2000092.Win03.051.burndate')
fire_occ = readOGR('./gis/Monitoring_Trends_in_Burn_Severity__Fire_Occurrence_Locations.shp', 
                   layer='Monitoring_Trends_in_Burn_Severity__Fire_Occurrence_Locations')
fire_poly = readOGR('./gis/Monitoring_Trends_in_Burn_Severity__Burned_Area_Boundaries.shp', 
                    layer='Monitoring_Trends_in_Burn_Severity__Burned_Area_Boundaries')
fire_Rx = readOGR('./gis/FM_RxBurnHistory.shp', layer='FM_RxBurnHistory')
# drop burns without a date
fire_Rx = fire_Rx[!is.na(fire_Rx$BurnDat), ]
fire_Rx$BurnDat = as.Date(fire_Rx$BurnDat, "%Y/%m/%d %H:%M:%S")
fire_Rx$year = as.numeric(format(fire_Rx$BurnDat, "%Y"))


proj4string(fire_Rx)

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

vegplots = read.csv('./data/CharlestonPlots.csv')
head(vegplots)
vegplots$project_num = as.integer(sapply(strsplit(as.character(vegplots$Plot.Code), "-"),
                                         function(x) x[1]))
vegplots$Date = as.Date(vegplots$Date, "%d-%b-%Y")
vegplots$year = as.numeric(format(vegplots$Date, "%Y"))

vegplots = SpatialPointsDataFrame(coords = vegplots[ , c('Real.Longitude', 'Real.Latitude')],
                                  data=vegplots, coords.nrs = 5:6,
                                  proj4string =  CRS("+proj=longlat +datum=WGS84"))
utm_prj =  CRS(proj4string(fire_Rx))
vegplots = spTransform(vegplots, utm_prj)

plot(fire_Rx)
points(vegplots, col='red')
points(vegplots[grep('Pinus palustris', vegplots$commPrimaryScientific), ],
       col='red', pch=19)
points(vegplots[vegplots$project_num == 16, ], col='blue')


tst = sapply(fire_Rx, function(x) over(x, vegplots))
plt_burns = sapply(1:nrow(fire_Rx), function(x) 
                   over(fire_Rx[x, ], vegplots)$Plot.Code)
table(plt_burns)
yrs_since_burn = vector('list', nrow(vegplots))
names(yrs_since_burn) = vegplots$Plot.Code
for(i in 1:nrow(vegplots)) {
    burndates = fire_Rx$BurnDat[which(plt_burns == vegplots$Plot.Code[i])]
    yrs_since_burn[[i]] = vegplots$Date[i] - burndates  
}

yrs_since_burn[grep('Pinus palustris', vegplots$commPrimaryScientific)]

ff_pre = sapply(yrs_since_burn, function(x) sum(x > 0))
ff_post = sapply(yrs_since_burn, function(x) sum(x < 0))


par(mfrow=c(1,3))
plot(ff_pre, ff_post)
abline(a=0, b=1)
lines(lowess(ff_pre, ff_post), col='red')
hist(ff_pre)
hist(ff_post)

vegplots$ff_pre = ff_pre
vegplots$ff_post = ff_post                                

llvegplots = vegplots[grep('Pinus palustris', vegplots$commPrimaryScientific), ]
llvegplots$nburns = sapply(yrs_since_burn[grep('Pinus palustris',
                                               vegplots$commPrimaryScientific)],
                           length)

inbounds = ifelse(is.na(over(llvegplots, fire_Rx)[,1]), F, T)

col_temp = rev(terrain.colors(5))[-1]
par(mfrow=c(1,2))
plot(fire_Rx, border='grey')
points(llvegplots, pch=1)
cols = get_samp_cols(llvegplots$ff_pre[inbounds], col_temp)
points(llvegplots[inbounds, ], pch=19, 
       col=get_samp_cols(llvegplots$ff_pre[inbounds], col_temp)$col)
legend('bottomright', 
       sort(unique(cut(llvegplots$ff_post[inbounds], length(cols))))
plot(fire_Rx, border='grey')
points(llvegplots, pch=1)
points(llvegplots[inbounds, ], pch=19, 
       col=get_samp_cols(llvegplots$ff_post[inbounds], cols))

spplot(llvegplots, c("ff_pre", "ff_post"), col.regions=rev(terrain.colors(6)[-6]),
       pch=19)


table(llvegplots$year)

pdf('vegplot_map.pdf')
data(us.cities)
map('county', c('south carolina,charleston', 'south carolina,berkeley'))
map.cities(us.cities, country="SC")
points(vegplots, col='dodgerblue', pch=19)
points(llvegplots, col='green3', pch=19)
points(llvegplots[ == 1995, ], col='red')
legend('bottomright', c('longleaf plot', 'other plot'), 
       col=c('green3', 'dodgerblue'), pch=19, bty='n')
dev.off()

## export kmls
writeOGR(Stand_ll, "./gis/Stand.kml", "Stand", "KML")
writeOGR(llStand_ll, "./gis/llStand.kml", "Stand", "KML")
writeOGR(Owners_ll, "./gis/Owners.kml", "Owners", "KML")
writeOGR(topo_ll, "./gis/topo.kml", "topo", "KML")
writeOGR(soil_ll, "./gis/soil.kml", "soil", "KML")
writeOGR(vegplots, "./gis/vegplots.kml", "vegplots", "KML")
writeOGR(llvegplots, "./gis/llvegplots.kml", "llvegplots", "KML")

## summary




