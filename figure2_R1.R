library(easyclimate)
library(terra)
library(tidyverse)

#### 1- Data downloading ####

#Austria contour
#aus <- geodata::gadm("Austria", level = 1, path = paste(getwd(), "contour", sep = "/"))
aus <- readRDS("contour/gadm41_AUT_1_pk.rds")
aus <- unwrap(aus)

#Wien contour
wien <- terra::subset(x = aus, subset = aus$NAME_1=="Wien")
nowien <- terra::subset(x = aus, subset = aus$NAME_1!="Wien")

#Climatic data
wientemp <- get_daily_climate(coords = wien, climatic_var = "Tmin",
                              period = c("2020-01-01","2020-01-15","2020-02-01","2020-02-15","2020-03-01","2020-03-15",
                                         "2020-04-01","2020-04-15","2020-05-01","2020-05-15"),
                              output = "raster")

wientemp2 <- crop(wientemp, wien, mask = TRUE)
nowien <- crop(nowien, wientemp)

#Elevation data
#elev.data <- geodata::elevation_3s(lon = 16.39, lat = 48.21, path = paste(getwd(), "strm", sep = "/")) #It is not working
#elev.data <- geodata::elevation_30s("Austria", path = paste(getwd(), "strm", sep = "/"))
elev.data <- rast("strm/AUT_elv_msk.tif")

elev <- crop(elev.data, wientemp)

#hillshade
slopes <- terrain(elev, "slope", unit = "radians")
aspect <- terrain(elev, "aspect", unit = "radians")
hs <- shade(slopes, aspect)

#osm
# highways <- geodata::osm("Austria", var = "highways", path = paste(getwd(), "osm", sep = "/"))
# railways <- geodata::osm("Austria", var = "railway", path = paste(getwd(), "osm", sep = "/"))

highways <- vect("osm/AUT_highways.gpkg")
highways <- unwrap(highways)
railways <- vect("osm/AUT_railway.gpkg")
railways <- unwrap(railways)

highways <- crop(highways, wientemp)
railways <- crop(railways, wientemp)

#
wientemp_df <- terra::as.data.frame(wientemp2, xy=TRUE)
wientemp_tidydf <- wientemp_df %>%
  pivot_longer(cols = `2020-01-01`:`2020-05-15`, names_to = "dates", values_to = "tmin")


hs_df <- terra::as.data.frame(hs, xy=TRUE)

wien_df <- as.data.frame(geom(wien))
nowien_df <- as.data.frame(geom(nowien))
high_df <- as.data.frame(geom(highways))
rail_df <- as.data.frame(geom(railways))
places_df <- as.data.frame(geom(places))

#
fig2 <- ggplot() +
  # geom_raster(data = hs_df, aes(x = x, y = y, fill = lyr1), interpolate = TRUE, show.legend = FALSE) +
  # scale_fill_gradient(low = "grey80", high = "grey20") +
  # ggnewscale::new_scale("fill") +
  geom_line(data = high_df, aes(x=x, y=y, group=paste(geom,part)), col = "black",
            alpha = 0.3, linewidth = 0.2) +
  geom_line(data = rail_df, aes(x=x, y=y, group=paste(geom,part)), col = "black",
            alpha = 0.3, linewidth = 0.5) +
  geom_tile(data = wientemp_tidydf, aes(x = x, y = y, fill = tmin), alpha = 0.9, color="grey80") +
  tidyterra::scale_fill_whitebox_c(palette = "muted") +
  facet_wrap(.~dates, nrow = 4) +
  geom_polygon(data = nowien_df, aes(x=x, y=y, group=part), fill = "white", col = NA, alpha = 0.6, linewidth = 0) +
  geom_polygon(data = wien_df, aes(x=x, y=y, group=part), fill = NA, col = "grey30", linewidth = 1) +
  xlab("") + ylab("") +
  labs(fill = "Minimum\ntemperature (ºC)",) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0))

ggsave("images/Figure2_R122.jpg", plot = fig2, width = 160, height = 230, dpi = 400, units = "mm")

