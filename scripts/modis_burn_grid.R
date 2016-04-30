library(raster)

files = dir('./gis/ModisData/Win03/')
file_pathes = file.path('./gis/ModisData/Win03', files)
fire = stack(file_pathes)

#plot(fire, 1)
#map('state', add=T)

xmin = -82
xmax = -75
ymin = 30
ymax = 35
e = extent(matrix(c(xmin, ymin, xmax, ymax), 2, 2))

fire_cp = crop(fire, e)


get_nburns = function(r) calc(r, function(x) length(x[x > 0 & x < 367]))

beginCluster(n=30)
nburns = clusterR(fire_cp, get_nburns)
endCluster()

nburns = calc(nburns, function(x) ifelse(x == 0, NA, x))

cols = colorRampPalette(c('pink', 'red'))
plot(nburns, col=cols(11))

writeRaster(fire_cp, filename='./gis/ModisData/fire_crop.grd', 
            overwrite=TRUE)
writeRaster(nburns, filename='./gis/ModisData/nburns.grd',
            overwrite=TRUE)
KML(nburns, './gis/ModisData/nburns.kmz', col=cols(11), overwrite=TRUE)
