library(raster)
library(maps)

nburns = raster('./gis/ModisData/fire_crop.grd')
lburn_yr = raster('./gis/ModisData/lburn_yr.grd')

pdf('./figs/nburns_apps.pdf')
plot(nburns, xlim=c(-85, -82), ylim=c(34.5,36), col=rev(heat.colors(10)))
map('county', add=T, col='grey')
map('state', add=T, col='black', lwd=3)
dev.off()

pdf('./figs/last_burn_year_apps.pdf')
plot(lburn_yr, xlim=c(-85, -82), ylim=c(34.5,36), col=rev(heat.colors(10)))
map('county', add=T, col='grey')
map('state', add=T, col='black', lwd=3)
dev.off()
