library(raster)

files = dir('./gis/ModisData/Win03/')
years <- unlist(strsplit(files, "MCD45monthly.A"))
years <- years[years != ""]
years <- unlist(strsplit(years, paste(".Win03.051.burndate.tif", sep = "")))
dates <- strptime(years, "%Y%j")
years <- as.numeric(format(dates, "%Y"))

file_pathes = file.path('./gis/ModisData/Win03', files)
fire = stack(file_pathes)
names(fire) = as.character(dates)

#plot(fire, 1)
#map('state', add=T)

xmin = -82
xmax = -75
ymin = 30
ymax = 35
e = extent(matrix(c(xmin, ymin, xmax, ymax), 2, 2))

fire_cp = crop(fire, e)


get_nburns = function(r) calc(r, function(x) length(x[x > 0 & x < 367]))

get_lburn_id = function(r) calc(r, function(x) max(which(x > 0 & x < 367)))

get_last_burn_date = function(r, years) {
    ## this function is too slow to be practical
    out = r[[1]]
    for(i in seq_along(out)) {
        pixel = r[i]
        indices = which(pixel > 0 & pixel < 367)
        if (length(indices) > 0) {
            bdates = as.Date(paste(years[indices], pixel[indices]), "%Y %j")
            out[i] = max(bdates)
        }
        else {
            out[i] = NA
        }
    }
}

beginCluster(n=30)
nburns = clusterR(fire_cp, get_nburns)
lburn_id = clusterR(fire_cp, get_lburn_id)
endCluster()

nburns = calc(nburns, function(x) ifelse(x == 0, NA, x))
lburn_yr = calc(lburn_id, function(x) years[x])

cols = colorRampPalette(c('pink', 'red'))
plot(nburns, col=cols(11))
plot(lburn_yr, col=cols(15))

writeRaster(fire_cp, filename='./gis/ModisData/fire_crop.grd', 
            overwrite=TRUE)
writeRaster(nburns, filename='./gis/ModisData/nburns.grd',
            overwrite=TRUE)
writeRaster(lburn_yr, filename='./gis/ModisData/lburn_yr.grd',
            overwrite=TRUE)
KML(nburns, './gis/kml/nburns.kmz', col=cols(11), overwrite=TRUE)
KML(lburn_yr, './gis/kml/lburn_yr.kmz', col=cols(15), overwrite=TRUE)
