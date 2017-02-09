library(raster)
library(rgdal)
library(zoom)
library(maps)
library(readxl)

source('./scripts/functions.R')

topo = readOGR('./gis/poly/Topography.shp', layer='Topography')
soil = readOGR('./gis/poly/SSURGO_Soils.shp', layer='SSURGO_Soils')    

Stand = readOGR('./gis/poly/Stand.shp', layer='Stand')
Owners = readOGR('./gis/poly/BasicSurfaceOwnership.shp', layer='BasicSurfaceOwnership')
roads = readOGR('./gis/poly/Road.shp', layer='Road')
invasive = readOGR('./gis/poly/Current_Invasive_Plants_Inventory.shp', 
                   layer='Current_Invasive_Plants_Inventory')

topo = stack('./gis/raster/1944 Cordesville Topographic Map.img')

fire_modis = raster('./gis/ModisData/nburns.grd')
fire_occ = readOGR('./gis/poly/Monitoring_Trends_in_Burn_Severity__Fire_Occurrence_Locations.shp', 
                   layer='Monitoring_Trends_in_Burn_Severity__Fire_Occurrence_Locations')
fire_poly = readOGR('./gis/poly/Monitoring_Trends_in_Burn_Severity__Burned_Area_Boundaries.shp', 
                    layer='Monitoring_Trends_in_Burn_Severity__Burned_Area_Boundaries')
fire_Rx = readOGR('./gis/poly/FM_RxBurnHistory.shp', layer='FM_RxBurnHistory')
# drop burns without a date
fire_Rx = fire_Rx[!is.na(fire_Rx$BurnDat), ]
fire_Rx$BurnDat = as.Date(fire_Rx$BurnDat, "%Y/%m/%d %H:%M:%S")
fire_Rx$year = as.numeric(format(fire_Rx$BurnDat, "%Y"))


proj4string(fire_Rx)

par(mfrow=c(1,2))
plot(fire_poly)
plot(fire_Rx)

proj4string(Stand)
geo_prj =  CRS("+proj=longlat +datum=WGS84")
Stand_ll = spTransform(Stand, geo_prj)
llStand_ll = spTransform(llStand, geo_prj)
Owners_ll = spTransform(Owners, geo_prj)
topo_ll = spTransform(topo, geo_prj)
soil_ll = spTransform(soil, geo_prj)
fire_Rx_ll = spTransform(fire_Rx, geo_prj)


plots16sp = read_excel('./data/Project016.xlsx', sheet = 'plot species list')
sr = with(plots16sp, tapply(currentTaxonName, authorObsCode, function(x) length(unique(x))))
plot(density(sr))
hist(sr)

plots16 = read_excel('./data/Project016.xlsx', sheet = 'plot data')
plots16$sr = as.numeric(sr[match(plots16$`Author Observation Code`, names(sr))])
crds16 = as.matrix(plots16[ , c('Real Longitude', 'Real Latitude')])

plots16 = SpatialPointsDataFrame(coords = crds16, data=as.data.frame(plots16),
                                  coords.nrs = c(11,12),
                                  proj4string =  CRS("+proj=longlat +datum=WGS84"))


vegplots = read.csv('./data/CharlestonPlots.csv')
# fix erroneous coordinates
c_coords = read.csv('./data/CharlestonPlots_corrected_coords.csv')
c_coords = SpatialPointsDataFrame(coords=c_coords[ , 2:3], c_coords,
                         proj4string = CRS('+proj=utm +zone=17 +datum=NAD83 +ellps=GRS80 +units=m +no_defs'))
c_coords = spTransform(c_coords,  CRS("+proj=longlat +datum=WGS84"))
vegplots[match(c_coords@data$Plot.Code, vegplots$Plot.Code), 
         c("Real.Longitude", "Real.Latitude")] = coordinates(c_coords)


vegplots$project_num = as.integer(sapply(strsplit(as.character(vegplots$Plot.Code), "-"),
                                         function(x) x[1]))
vegplots$team_num = as.integer(sapply(strsplit(as.character(vegplots$Plot.Code), "-"),
                                         function(x) x[2]))
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
cols = get_samp_cols(llvegplots$ff_pre[inbounds], col_temp)
grps = as.character(sort(unique(cols$grps)))

pdf('./figs/pre_post_fire_freq_llvegplots.pdf', width=7*2, height=7)
plot(fire_Rx, border='grey')
points(llvegplots, pch=1)
points(llvegplots[inbounds, ], pch=19, col=cols$col)
legend('bottomright', grps, col = cols$col[match(grps, cols$grps)], 
       pch=19, bty='n')

cols = get_samp_cols(llvegplots$ff_post[inbounds], col_temp)
grps = as.character(sort(unique(cols$grps)))
plot(fire_Rx, border='grey')
points(llvegplots, pch=1)
points(llvegplots[inbounds, ], pch=19, col=cols$col)
legend('bottomright', grps, col = cols$col[match(grps, cols$grps)], 
       pch=19, bty='n')
dev.off()

## project 16 only
pdf('./figs/pre_post_fire_freq_llvegplots.pdf', width=7*2, height=7)
plot(fire_Rx, border='grey')
points(llvegplots, pch=1)
points(llvegplots[inbounds, ], pch=19, col=cols$col)
legend('bottomright', grps, col = cols$col[match(grps, cols$grps)], 
       pch=19, bty='n')

cols = get_samp_cols(llvegplots$ff_post[inbounds], col_temp)
grps = as.character(sort(unique(cols$grps)))
plot(fire_Rx, border='grey')
points(llvegplots, pch=1)
points(llvegplots[inbounds, ], pch=19, col=cols$col)
legend('bottomright', grps, col = cols$col[match(grps, cols$grps)], 
       pch=19, bty='n')
dev.off()

table(llvegplots$year)

## extract fire history for llvegplots
llburn = spTransform(burn, CRS(proj4string(llvegplots)))
vegburns = over(llvegplots, llburn, returnList=TRUE)

lastburn = unlist(lapply(vegburns, function(x) as.character(max(x$BurnDate))))
nburn = unlist(lapply(vegburns, function(x) length(x$BurnDate)))

vegchar = data.frame(ID = llvegplots@data$Plot.Code, 
                     Year = as.numeric(sapply(as.character(llvegplots@data$Date), function(x)
                         strsplit(x, '-')[[1]][3])),
                     Lat = llvegplots@data$Real.Latitude,
                     Long = llvegplots@data$Real.Longitude,
                     lastburn, nburn, 
                     llvegplots@data$Soil.Drainage)

llvegplots@data$lastburn = lastburn
llvegplots@data$nburn = nburn



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

## for resampling
llvegplots16 = llvegplots[llvegplots$project_num ==16,]
#llvegplots16 = spTransform(llvegplots16, geo_prj)
labs = sub('016-', '', llvegplots16@data$Plot.Code)

plot(llvegplots16, col='red', pch=19, cex=.5)
oldcrds = coordinates(llvegplots16)
newcrds = SpatialPoints(coords = cbind(oldcrds[,1], oldcrds[,2] + 700))
text(newcrds, labs, col='red')
plot(roads, add=T, col='grey')

## export kmls
writeOGR(Stand_ll, "./gis/kml/Stand.kml", "Stand", "KML")
writeOGR(llStand_ll, "./gis/kml/llStand.kml", "Stand", "KML")
writeOGR(Owners_ll, "./gis/kml/Owners.kml", "Owners", "KML")
writeOGR(topo_ll, "./gis/kml/topo.kml", "topo", "KML")
writeOGR(soil_ll, "./gis/kml/soil.kml", "soil", "KML")
vegplots = spTransform(vegplots, geo_prj)
llvegplots = spTransform(llvegplots, geo_prj)
writeOGR(vegplots, "./gis/kml/vegplots.kml", "vegplots", "KML")
writeOGR(llvegplots, "./gis/kml/llvegplots.kml", "llvegplots", "KML")
writeOGR(fire_Rx_ll, "./gis/kml/fire_Rx.kml", "fire_Rx", "KML")
plots16_sub = plots16[ , c("Author Plot Code", "countyName", "realUTME", "realUTMN",
                           "Author Location", "commPrimaryTranslated",
                           "sr")]
writeOGR(plots16_sub, "./gis/kml/plots16.kml", "plots16", "KML")
roads = spTransform(roads, geo_prj)
writeOGR(roads, "./gis/kml/roads.kml", "roads", "KML")
## summary




## export kmls
writeOGR(Stand_ll, "./gis/Stand.kml", "Stand", "KML")
writeOGR(llStand_ll, "./gis/llStand.kml", "Stand", "KML")
writeOGR(Owners_ll, "./gis/Owners.kml", "Owners", "KML")
writeOGR(vegplots, "./gis/vegplots.kml", "vegplots", "KML")
writeOGR(llvegplots, "./gis/llvegplots.kml", "llvegplots", "KML", overwrite_layer = T)
writeOGR(llRoad, "./gis/Road.kml", "Road", "KML")
    
out = llvegplots
out@data = data.frame(name=out@data$Plot.Code)
writeOGR(out, './gis/llvegplots.gpx', 'llvegplots', 'GPX')

tst = spTransform(llvegplots, CRS(proj4string(burn)))
tst[tst@data$Plot.Code == '044-02-0602',]
