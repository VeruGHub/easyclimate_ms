library(easyclimate)
library(terra)
library(tidyverse)

#### 1- Data downloading ####

#Austria contour
#aus <- geodata::gadm("Austria", level = 1, path = paste(getwd(), "contour", sep = "/"))
aus <- readRDS("contour/gadm41_AUT_1_pk.rds")
aus <- unwrap(aus)

#Wien contour
tirol <- terra::subset(x = aus, subset = aus$NAME_1=="Tirol")
notirol <- terra::subset(x = aus, subset = aus$NAME_1!="Tirol")

#Climatic data
tiroltemp <- get_daily_climate(coords = tirol, climatic_var = "Tmin",
                              period = "2020-05-01:2020-05-10",
                              output = "raster")

tiroltemp2 <- crop(tiroltemp, tirol, mask = TRUE)
notirol <- crop(notirol, tiroltemp)

#Elevation data
#elev.data <- geodata::elevation_3s(lon = 16.39, lat = 48.21, path = paste(getwd(), "strm", sep = "/")) #It is not working
#elev.data <- geodata::elevation_30s("Austria", path = paste(getwd(), "strm", sep = "/"))
elev.data <- rast("strm/AUT_elv_msk.tif")

elev <- crop(elev.data, tiroltemp)

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

highways <- crop(highways, tiroltemp)
railways <- crop(railways, tiroltemp)

#
tiroltemp_df <- terra::as.data.frame(tiroltemp2, xy=TRUE)
tiroltemp_tidydf <- tiroltemp_df %>%
  pivot_longer(cols = `2020-05-01`:`2020-05-10`, names_to = "dates", values_to = "tmin")


hs_df <- terra::as.data.frame(hs, xy=TRUE)

tirol_df <- as.data.frame(geom(tirol))
notirol_df <- as.data.frame(geom(notirol))
aus_df <- as.data.frame(geom(aus))
high_df <- as.data.frame(geom(highways))
rail_df <- as.data.frame(geom(railways))

#
fig2 <- ggplot() +
  # geom_raster(data = hs_df, aes(x = x, y = y, fill = lyr1), interpolate = TRUE, show.legend = FALSE) +
  # scale_fill_gradient(low = "grey80", high = "grey20") +
  # ggnewscale::new_scale("fill") +
  # geom_line(data = high_df, aes(x=x, y=y, group=paste(geom,part)), col = "black",
  #           alpha = 0.3, linewidth = 0.5) +
  # geom_line(data = rail_df, aes(x=x, y=y, group=paste(geom,part)), col = "black",
  #           alpha = 0.3, linewidth = 0.5) +
  geom_tile(data = tiroltemp_tidydf, aes(x = x, y = y, fill = tmin), alpha = 0.9) +
  tidyterra::scale_fill_whitebox_c(palette = "muted") +
  facet_wrap(.~dates, nrow = 5) +
  geom_polygon(data = notirol_df, aes(x=x, y=y, group=geom), fill = "grey80", col = NA, linewidth = 0, alpha = 0.9) +
  geom_polygon(data = tirol_df, aes(x=x, y=y, group=part), fill = NA, col = "grey30", linewidth = 0.3) +
  xlab("") + ylab("") +
  # coord_cartesian(xlim = c(11.2,11.5), ylim = c(47.1,47.4)) + #Focus on Innsbruck
  scale_y_continuous(breaks = 47) +
  labs(fill = "Minimum\ntemperature (ºC)",) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0))

ggsave("images/Figure2_R133.jpg", plot = fig2, width = 160, height = 230, dpi = 400, units = "mm")


loc <- ggplot() +
  geom_polygon(data = aus_df, aes(x=x, y=y, group=paste0(geom,part)), fill = "grey80", col = "grey80", linewidth = 0) +
  geom_polygon(data = tirol_df, aes(x=x, y=y, group=part), fill = NA, col = "grey30", linewidth = 0.3) +
  xlab("") + ylab("") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0))

ggsave("images/Figure2_R133_loc.jpg", plot = loc, width = 55, height = 38, dpi = 400, units = "mm")

#Composition

library(magick)

img1 <- image_read("images/Figure2_R133.jpg")
img2 <- image_read("images/Figure2_R133_loc.jpg")

img2 <- image_crop(img2, "+60-70")
img2 <- image_resize(img2, "525")

fig1_2 <- image_composite(img1, image_border(img2, "grey80", "3x3"), offset = "+1975+1075")

image_write(fig1_2, "images/Figure2_R1_final.jpg")






