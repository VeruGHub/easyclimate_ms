
library(easyclimate)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggmap)

full_poly <- vect("POLYGON ((-40.5 25.5, -40.5 75.5, 75.5 75.5, 75.5 25.5))")

eu <- get_daily_climate(coords = full_poly, climatic_var = "Tmax", period = "1950-01-01", output = "raster")
beepr::beep()
#border <- geodata::world(resolution=5, level=0, path = tempdir()) #Not working after updating packages
border <- map_data(map = "world")

#
eu2 <- terra::project(eu, "EPSG:3035")

border2 <- border %>%
  mutate(coords = paste(long, lat, sep = " ")) %>%
  group_by(group, region) %>%
  summarize(polygon = str_c(coords, collapse = ", "))

border_spat2 <- NULL
for (i in unique(border2$group)) {
  border_spat <- vect(paste0("POLYGON ((", border2[border2$group == i, "polygon"], "))"),
                    crs = "EPSG:4326")
  border_spat2 <- c(border_spat2, border_spat)
}

border_spat2_2 <- vect(border_spat2)
border_spat2_2 <- terra::project(border_spat2_2, "EPSG:3035")

#
fig1 <- ggplot() +
  geom_spatvector(data = border_spat2_2, fill = "grey90", color = "grey90") +
  geom_spatraster(data = eu2, aes(fill = `1950-01-01`)) +
  coord_sf(xlim = c(-100000,8000000), ylim = c(300000, 6000000), expand = FALSE) +
  scale_fill_whitebox_c(palette = "muted") +
  labs(fill = "Maximum\ntemperature (ºC)") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"))

# fig1 <- ggplot() +
#   geom_polygon(data = border, aes(x = long, y = lat, group = group),
#                fill = "grey90", color = "grey90") +
#   geom_raster(data = as.data.frame(eu2, xy=TRUE), aes(x = x, y = y, fill = `1950-01-01`)) +
#   coord_sf(xlim = c(-100000,8000000), ylim = c(300000, 6000000), expand = FALSE)

ggsave("images/Figure1_R1.jpg", plot = fig1, width = 210, height = 110, dpi = 300, units = "mm")

library(magick)

img1 <- image_read("images/Figure1_R1.jpg")
img2 <- image_read("images/Figure1_R1.jpg")

img1 <- image_crop(img1, "-500+0")

img2_legend <- image_crop(img2, "+1950-400")
img2_legend <- image_crop(img2_legend, "-100+350")

fig1_2 <- image_composite(img1, image_border(img2_legend, "grey80", "3x3"), offset = "+250+50")

image_write(fig1_2, "images/Figure1_R1_2.jpg")





