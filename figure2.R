
library(easyclimate)
library(terra)

coords.poly <- vect("POLYGON ((-5.37 40.30, -5.17 40.30, -5.17 40.15, -5.37 40.15))")

ras_tmax <- get_daily_climate(
  coords.poly,
  period = c("2012-01-01", "2012-08-07"),
  climatic_var = "Tmax",
  output = "raster"
)


#{r Fig-2, echo=FALSE, fig.width = 170, fig.height = 110, fig.cap="A multilayer raster of maximum temperature values for a given polygon in two different days of the year."}

library(terra)
elev.data <- geodata::elevation_3s(lon = -5.27, lat = 40.22, path = paste(getwd(), "stmr", sep = "/"))
elev <- crop(elev.data, ras_tmax)

#### Calculate hillshade ####
slopes <- terrain(elev, "slope", unit = "radians")
aspect <- terrain(elev, "aspect", unit = "radians")
hs <- shade(slopes, aspect)

#### Map ####

jpeg(filename = "images/maptemp.jpg", width = 170, height = 110, units = "mm", res = 300)
#
par(mfrow = c(1, 2))

## Plot hillshading as basemap
plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = FALSE,
     main = "January 1 2012", mar=c(4, 2, 4, 2), yaxp = c(40.15, 40.30, 3))
# overlay with Tmax
plot(ras_tmax[[1]], alpha = 0.8, axes = TRUE, add = TRUE,
     col = rev(heat.colors(20)), type = "continuous",
     smooth = TRUE, range = c(3, 35), legend = FALSE)


plot(hs, col = gray(0:100 / 100), legend = FALSE, axes = FALSE,
     main = "August 7 2012", mar=c(4, 1, 4, 3), yaxp = c(40.15, 40.30, 3))
# overlay with Tmax
plot(ras_tmax[[2]], alpha = 0.8, axes = TRUE, add = TRUE,
     col = rev(heat.colors(20)), type = "continuous",
     smooth = TRUE, range = c(3, 35), legend = TRUE)

dev.off()
