library(osmdata)
library(sf)
library(sp)
library(leaflet)
library(ggmap)

############################# OSM Data #############################

# search for your place of interest
# coordinates of patrick county 36.6886° N, 80.3213° W
bb <- getbb('patrick county, virginia')

# from that place get a key and the specific value
q <- opq(bbox = bb) %>%
  add_osm_feature(key = 'amenity', value = c('pharmacy', 'hospital', 'clinic', 'doctors', 'dentist',
                                             'nursing_home', 'social_facility', 'ambulance_station'))
  
r <- opq(bbox = bb) %>%
  add_osm_feature(key = 'highway', value = c('primary', 'secondary', 'tertiary'))

medical <- osmdata_sf(q)
roads <- osmdata_sf(r)

# ggmaps ---------------------------------------------

mad_map <- get_map(bb, maptype = "toner-background")

ggmap(mad_map)+
  geom_sf(data = medical$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = .5,
          shape = 21)+
  labs(x = "", y = "")

