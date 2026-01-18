py_config()

coords <- st_coordinates(site)  # Extract coordinates
coords <- rbind(coords, coords[1,])  # Make sure the polygon is closed by repeating the first coordinate

# Convert matrix to a list of points, where each point is a list of [lon, lat]
coords_list <- lapply(1:nrow(coords), function(i) {
  list(as.numeric(coords[i, 1]), as.numeric(coords[i, 2]))
})
polygon_coords<- list(
  list(list(20.222026045631814, 49.46894668625066), list(20.22202753216462, 49.46894867949474),
       list(20.222261361723007, 49.469259598621555), list(20.222259645698042, 49.469259910848805))
)
# Wrap the list of points in another list to create a single ring
  #polygon_coords <- list(coords_list)

# Create the polygon in Earth Engine
  ee_polygon <- ee$Geometry$Polygon(nested_list)

# Proceed with your Earth Engine image collection filtering
sentinel_img <- ee$ImageCollection('COPERNICUS/S2')$
  filterDate("2020-09-10", "2020-09-20")$
  filterBounds(ee_polygon)$
  select(c('B2', 'B3', 'B4', 'B8'))

median_composite <- sentinel_img$median()

# Retrieve information about the composite
info_result <- median_composite$getInfo()



raster_data <- rasterFromXYZ(as.data.frame(info_result$values))
