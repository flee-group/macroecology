## notes
## this is a very simple pseudo-absence generator
## it will take a random selection of background points over whatever geographic domain
## there is considerable debate over how to best do this
## see:
## Barbet-Massin, M., Jiguet, F., Albert, C.H. and Thuiller, W. (2012)
## Selecting pseudo-absences for species distribution models: how, where and how many?. 
## Methods in Ecology and Evolution, 3: 327-338. 
## https://doi.org/10.1111/j.2041-210X.2011.00172.x
##
## There are many newer articles as well dealing with this topic

## We will use random selection with a 4:1 ratio of absences to presences
## We need:
## 1. The presence only dataset
## 2. A raster layer giving the spatial extent where we want to work
## 3. or (instead of 2) a set of coordinates describing the extent

library(sf)
library(raster)
presences = readRDS("data/sorex_po.rds")

# if you have a raster, load it here:
clim_pres = stack("data/bioclim_pres.grd")
domain = extent(clim_pres)

# or define the extent manually
# domain = extent(x1, x2, y1, y2)
# the disadvantage here is that there is no easy way to avoid the oceans

# start by picking points totally randomly
n_total = 4 * nrow(presences)
absences = data.frame(
	presence = "absent",
	x = runif(n_total, domain[1], domain[2]),
	y = runif(n_total, domain[3], domain[4])
)
absences = st_as_sf(absences, coords = c('x', 'y'), crs = crs(clim_pres))

# here we check the raster for NAs (meaning ocean) and drop them
absences = cbind(absences, extract(clim_pres$bio1, absences))
colnames(absences)[2] = "bio1"
absences = absences[!is.na(absences$bio1),]
absences$bio1 = NULL

# choose more absences iteratively until we have enough that are non-ocean
while(nrow(absences) < n_total) {
	n = n_total - nrow(absences)
	new_absences = data.frame(
		presence = "absent",
		x = runif(n, domain[1], domain[2]),
		y = runif(n, domain[3], domain[4])
	)
	new_absences = st_as_sf(new_absences, coords = c('x', 'y'), crs = crs(clim_pres))
	new_absences = cbind(new_absences, extract(clim_pres$bio1, new_absences))
	colnames(new_absences)[2] = "bio1"
	new_absences = new_absences[!is.na(new_absences$bio1),]
	new_absences$bio1 = NULL
	absences = rbind(absences, new_absences)
}

plot(clim_pres$bio1)
plot(absences, add = TRUE, pch = 16, cex = 0.7, col = "black")

# combine the two
pa = rbind(presences, absences)
# save
saveRDS(pa, "data/pres_abs_data.rds")
