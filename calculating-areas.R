library(tidyverse)
library(sf)
library(tmap)
library(terra)
library(exactextractr)
library(microbenchmark)
library(crsuggest)
options(scipen=999)

sf_use_s2()

dat <- rnaturalearthdata::countries50 %>% 
  st_as_sf() %>% 
  filter(admin == 'Sudan')

st_is_valid(dat)

dat_v <- dat %>% st_make_valid()
st_is_valid(dat_v)

# quick map

tmap_mode('view')
qtm(dat_v)

# calculate area

area_s2 <- st_area(dat_v) %>% units::set_units('km2')
area_s2

# turn off s2
sf_use_s2(FALSE)
area_GeoLib <- st_area(dat_v) %>% units::set_units('km2')

area_s2 - area_GeoLib

# speed for accuracy trade-off

microbenchmark(
  {sf_use_s2(TRUE); area_s2 <- st_area(dat_v)},
  {sf_use_s2(FALSE); area_GeoLib <- st_area(dat_v)}
)

# area with PCS - equal-area 

dat_p <- dat_v %>% st_transform('ESRI:54009')

area_mollweide <- st_area(dat_p) %>% units::set_units('km2')
area_mollweide - area_GeoLib

# crsuggest

suggest_crs(dat_v)

dat_p2 <- dat_v %>% st_transform('EPSG:22992')
area_UTM <- st_area(dat_p2) %>% units::set_units('km2')
area_UTM - area_GeoLib

### raster area calculations

sf_use_s2(TRUE)
r <- rast(nrows = 10, ncol= 10, xmin = 0, ymin = 0, xmax = 10, ymax = 10)
r <- init(r, 8)
p <- vect('POLYGON ((2 2, 7 6, 4 9 , 2 2))', crs = crs(r))
r_mask <- mask(r, p)

qtm(r) + qtm(r_mask) + qtm(st_as_sf(p))

# area calcs with terra

area_t <- expanse(p, 'km')
sf_use_s2(TRUE)
area_sf <- st_area(st_as_sf(p)) %>% units::set_units('km2')
round(area_t - as.numeric(area_sf))

# calculate area rasters

r_area <- cellSize(r_mask, unit='km')
qtm(r_area) + qtm(st_as_sf(p))
e <- extract(r_area, p, exact = TRUE)

area_t - sum(e$area * e$fraction)

e2 <- exact_extract(r_area, st_as_sf(p))[[1]]
sum(e$area * e$fraction) - sum(e2$value * e2$coverage_fraction)

microbenchmark(extract(r_area, p, exact=TRUE), 
               exact_extract(r_area, st_as_sf(p)))

