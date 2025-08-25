
spp1 <-
  spp %>%
  dplyr::filter(species == "sp1") %>%
  dplyr::filter(pr_ab == 1) %>%
  dplyr::select(-pr_ab)

crs(regions, proj = TRUE)

ca_1 <- calib_area(
  data = spp1,
  x = "x",
  y = "y",
  method = c("buffer", width = 40000),
  crs = crs(regions)
)
plot(regions, main = "Buffer method")
plot(ca_1, add = TRUE)
points(spp1[, 2:3], pch = 19, cex = 0.5)


ca_2 <- calib_area(
  data = spp1,
  x = "x",
  y = "y",
  method = c("mcp"),
  crs = crs(regions)
)

plot(regions, main = "Minimum convex polygon method")
plot(ca_2, add = TRUE)
points(spp1[, 2:3], pch = 19, cex = 0.5)

ca_3 <- calib_area(
  data = spp1,
  x = "x",
  y = "y",
  method = c("bmcp", width = 40000),
  crs = crs(regions)
)

plot(regions, main = "Buffered minimum convex polygon")
plot(ca_3, add = TRUE)
points(spp1[, 2:3], pch = 19, cex = 0.5)

clusters <- system.file("external/clusters.shp", package = "flexsdm")
clusters <- terra::vect(clusters)

ca_4 <- calib_area(
  data = spp1,
  x = "x",
  y = "y",
  method = c("mask", clusters, "clusters"),
  crs = crs(regions)
)

par(mfrow = c(1, 2))
plot(clusters, main = "Original polygons")
plot(ca_4, main = "Polygons with points (mask)")
points(spp1[, 2:3], pch = 19, cex = 0.5)
