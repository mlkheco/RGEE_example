library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
#Required code: maskL456sr.R, addEVI2.R, and their LS8 counterparts
#INITIALIZE THESE FUNCTIONS EVERYTIME YOU RESTART R!
#If no layers show up, run visualise landsatnir2.R first. #Should not be needed in this example

#Requires Python and GEE
index<-9
id_number<-1:9
# Load specific Landsat image IDs and select the desired one
Collection <- select_landsat_collection(A$Sample_start_earliest[index])

# Split the image IDs
image_ids <- strsplit(A$Image_IDs[index], "; ")[[1]]

# Select the desired image ID
selected_image_id <- trimws(image_ids)  # Trim any leading or trailing whitespace
selected_image_id <- paste0(Collection$collection, "/", selected_image_id)

#Calculate EVI2 
if(grepl("^LANDSAT/LT", selected_image_id[1]) | grepl("^LANDSAT/LE", selected_image_id[1])) {
  message("LandSat457 for ", selected_image_id[1])
  image_objects <- lapply(selected_image_id, function(id) {
    image <- ee$Image(id) #Gets the images
    evi_image <- add_evi2LS457(image)  #The add EVI function
    
    return(evi_image)
  })
} else {
  if(grepl("^LANDSAT/LC", selected_image_id[1])) {
    message("LandSat8 for ", selected_image_id[1])
    image_objects <- lapply(selected_image_id, function(id){
      image <- ee$Image(id)
      evi_image <- add_evi2LS8(image)
      return(evi_image)
    })
  } else {errorCondition("no LandSat Collection compatible")}}


#Get a collection of all images extracted for that study
image <- ee$ImageCollection$fromImages(image_objects)
#Creates a composite median image here
median<-image$median()
# grab the evi2 band
single_band_image <- median$select('evi2')

# # Set visualization parameters for the single band based on statistics
evi2_vis_params <- list(
  min = -1,   # Adjust based on the minimum evi2 value found in your data
  max = 1.2,    # Adjust based on the maximum evi2 value found in your data
  palette = c(
    'blue', 'cyan', 'green', 'yellow', 'orange', 'red'
  )
)
# Get the map ID for the single band image
evi2_map_id <- single_band_image$getMapId(evi2_vis_params)



# for some reason "/{z}/{x}/{y}?" is needed in the URL to bypass deprecated token requirements
url_template <- gsub("/0/0/0?", "/{z}/{x}/{y}?", evi2_map_id$tile_fetcher$url_format, fixed = TRUE)



# Create leaflet map
map <- leaflet() %>%
  addProviderTiles(provider = providers$Esri.WorldImagery, group = "Base Map", options = tileOptions(opacity = 0.8)) %>%
  # addTiles(urlTemplate = nir_url_template, options = tileOptions(opacity = 1, attribution = "NIR Band: Google Earth Engine"), group = "NIR Band") %>%
  # addTiles(urlTemplate = evi_url_template, options = tileOptions(opacity = 1, attribution = "EVI: Google Earth Engine"), group = "EVI") %>%
  addTiles(urlTemplate = url_template, options = tileOptions(opacity = 1, attribution = "EVI2: Google Earth Engine"), group = "EVI2") %>%
  # addTiles(urlTemplate = ndvi_url_template, options = tileOptions(opacity = 1, attribution = "NDVI: Google Earth Engine"), group = "NDVI") %>%
  # addTiles(urlTemplate = ndbi_url_template, options = tileOptions(opacity = 1, attribution = "NDBI: Google Earth Engine"), group = "NDBI") %>%
  # # addTiles(urlTemplate = Distance_url_template, options = tileOptions(opacity = 1, attribution = "Distance: Google Earth Engine"), group = "Distance") %>%
  # addTiles(urlTemplate = ibi_url_template, options = tileOptions(opacity = 1, attribution = "ibi: Google Earth Engine"), group = "ibi") %>%
  setView(lng = A$Longitude[index], lat = A$Latitude[index], zoom = 10) %>%
  addCircles(lng = A$Longitude[index], lat = A$Latitude[index], radius = 300, weight = 2, color = 'red', fillColor = '#FF000088', fillOpacity = 0.5, group = "Buffer Zone 300m") %>%
  addCircles(lng = A$Longitude[index], lat = A$Latitude[index], radius = 1000, weight = 2, color = 'red', fillColor = '#FF000088', fillOpacity = 0.5, group = "Buffer Zone 1000m") %>%
  addCircles(lng = A$Longitude[index], lat = A$Latitude[index], radius = 2500, weight = 2, color = 'grey', fillColor = 'grey', fillOpacity = 0.5, group = "Buffer Zone 2500m") %>%
  addCircleMarkers(lng = A$Longitude[index], lat = A$Latitude[index], radius = 5, color = 'blue', fillColor = 'blue', fillOpacity = 1, group = "Center Point") %>%
  addLayersControl(overlayGroups = c(  "EVI2", "Buffer Zone 300m", "Buffer Zone 1000m","Buffer Zone 2500m", "Center Point"), baseGroups = c("Base Map"), options = layersControlOptions(collapsed = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
  print(map)


longitude <- A$Longitude[index]
latitude <- A$Latitude[index]
point_of_interest <- ee$Geometry$Point(c(longitude, latitude))
buffered_area <- point_of_interest$buffer(1000)  # Buffer by 1000 meters

# Extract EVI2 in pre-defined scale
buffered_evi2_stats <- single_band_image$reduceRegion(
  reducer = ee$Reducer$toList(),  # This reducer compiles all pixel values into a list
  geometry = buffered_area,       # The defined buffer area
  scale = 30,                     # Scale should be set to the resolution of the imagery
  maxPixels = 1e8                 # Adjust this value based on the expected size of the buffer
)

# Extract evi2 values from the returned dictionary
evi_list <- buffered_evi2_stats$get("evi2")$getInfo()

# Print evi2 values or analyze further
print(evi_list)

# Calculate mean, median, etc.
evi_mean <- mean(evi_list)
evi_median <- median(evi_list)
evi_min <- min(evi_list)
evi_max <- max(evi_list)

print(paste("Mean evi2:", evi_mean))
print(paste("Median evi2:", evi_median))
print(paste("Min evi2:", evi_min))
print(paste("Max evi2:", evi_max))

#Distribution of EVI in extracted data
evi_data <- data.frame(evi2 = evi_list)

evi_histogram <- ggplot(evi_data, aes(x = evi2)) +
  geom_histogram(bins = 50, fill = "forestgreen", color = "black") + 
  ggtitle("Histogram of evi2 Values") +
  xlab("evi2") +
  ylab("Frequency") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))  

# Display the histogram
print(evi_histogram)



