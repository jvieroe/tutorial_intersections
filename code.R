rm(list=ls())

# ----- Call libraries
library(sf)
library(tidyverse)
library(magrittr)
library(rio)
library(haven)
library(nngeo)

# ----- Define 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

# ----- set working directory
setwd("/Users/jeppeviero/Dropbox/02 PhD/18 tutorials/intersections")

# ----- Load map data
map <- st_read(dsn = "ne_10m_admin_0_countries/ne_10m_admin_0_countries",
               layer = "ne_10m_admin_0_countries",
               crs = 4326)

# ----- Filter map data to Denmark and Sweden
map <- map %>% 
  filter(GEOUNIT == "Denmark" | GEOUNIT == "Sweden") %>% 
  dplyr::select(geometry,
                GEOUNIT)

# ----- Plot map data
ggplot(map) + 
  geom_sf(aes(fill = GEOUNIT), alpha = 0.4)


# ----- Load cities data
cities <- haven::read_dta("cities.dta")

# check city data
head(cities)
table(cities$country) # danish and swedish cities

# ----- Transform to spatial data structure
cities <- cities %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326)

# ----- Plot it!
ggplot() + 
  geom_sf(data = map, aes(fill = GEOUNIT), alpha = 0.4) +
  geom_sf(data = cities)

# ----- Match cities to polygons (1)
joins <- st_join(cities, map,
                 join = st_intersects) # match cities to polygons that they are part of/touches

# check whether it worked: compare raw variable (country) to geospatial matched data (GEOUNIT)
joins %$%
  table(country, GEOUNIT) # looks good!

any(is.na(joins$GEOUNIT)) # but, some cities have not been matched to a polygon. That's odd

joins %>% 
  filter(is.na(GEOUNIT)) %>% 
  head() # Actually, only Karlskrona is unmatched. It's probably a coastal city whose coordinates are a bit off

joins %>% 
  mutate(is_karlskrona = ifelse(city == "karlskrona" & country == "sweden",
                                "1",
                                "0")) %>% 
  ggplot(.) +
  geom_sf(aes(col = is_karlskrona, fill = is_karlskrona),
          shape = 21, size = 3) +
  geom_sf(data = map, alpha = 0.25) # yes, just by the coast. Guess that's why! 

# Let's try to be a little less restrictive: match to polygons that the cities either touch or are close to (the maxdist parameter)

st_crs(map)
st_crs(cities)
# both have length units = meters (NB: they need to have the same CRS so I shouldn't have to check twice...)

# ----- Match cities to polygons (2)
joins_vol2 <- st_join(cities, map,
                      join = st_nn, # nngeo::st_nn()
                      k = 1, # 1 nearest feature (polygon)
                      maxdist = 10*10^3, # 10,000 = 10km
                      progress = T)

joins_vol2 %$%
  table(country, GEOUNIT) # aha! another correctly identified swedish city!

any(is.na(joins_vol2$GEOUNIT)) # no NAs

# check Karlskrona
joins_vol2 %>% 
  filter(city == "karlskrona") %>% 
  head() # worked!

# ----- Match cities to polygons (3)
joins_vol3 <- st_intersection(cities, map)

cities %>% 
  filter(city %!in% joins_vol3$city) %>% 
  head()
# same as procedure (1)

