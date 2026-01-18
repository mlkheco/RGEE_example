library(rgee)
library(reticulate)
library(sf)
library(dplyr)
library(lubridate)
rgee::ee_install_upgrade() #install the previous version of ee python extension so rgee can communicate
ee <- import('ee')
ee_Authenticate()
ee$Initialize() #Required if a bug is encountered, initialize with python first before rgee.
ee_Initialize()  # Initialize the Earth Engine API and stop it from "UPGRADING"
#do not abort this code or R crashes.
nrow(PREDSStime)
rm(Error_List)
Error_List<-list()




process_sentinel_images <- function(data, long_col, lat_col, date_cols) { 
  #this is landsat, The function name is just not changed
  # Filter data based on the duration condition
    data_filtered<-data
    #data_filtered <- dplyr::filter(data, Sample_start_earliest_to_Sample_end_latest_Duration <= 3)
  
  # Filter for unique combinations
      data_filtered <- data_filtered %>%
        group_by(!!sym(long_col), !!sym(lat_col), !!sym(date_cols[1]), !!sym(date_cols[2])) %>%
        filter(row_number() == 1) %>%
        ungroup()
  # Create a subset for testing after filtering
    data_copy<-data_filtered
    #data_copy <- head(data_filtered, 5)  # Adjust the subset size as needed
  
  # Convert data to simple features and apply buffer
  coordinates <- st_as_sf(data_copy, coords = c(long_col, lat_col), crs = 4326)
  buffer_radius <- 300
  search_areas <- st_buffer(coordinates, dist = buffer_radius)
  
  # Prepare results storage
  results <- vector("list", length = nrow(search_areas))
    
    

    
  # Process each unique site
  for (i in seq_len(nrow(search_areas))) {
    message("Processing row: ", i, " of ", nrow(search_areas))
        # Assuming 'search_areas' is your sf dataframe
        # Extracting the coordinates of the first polygon in the geometry column
        coords <- st_coordinates(search_areas$geometry[i, ])
        
        # Convert the matrix of coordinates into the desired nested list format
        nested_list <- lapply(seq_len(nrow(coords)), function(i) {
          list(as.numeric(coords[i, 'X']), as.numeric(coords[i, 'Y']))
        })
        
        # The result is a list of lists, where each inner list is a pair of coordinates (x, y)
        #nested_list
        ee_polygon <- ee$Geometry$Polygon(nested_list)
    
    
    start_date <- format(data_copy[[date_cols[1]]][i]-62, "%Y-%m-%d")
    end_date <- format(data_copy[[date_cols[2]]][i]+62, "%Y-%m-%d")
    
    
    # Getting the collection and band info
    collection_date <- format(data_copy[[date_cols[1]]][i], "%Y-%m-%d")
    collection_info <- select_landsat_collection(collection_date)#"2099-02-16" <- for fail test
    
    # Accessing Earth Engine
      print(paste0("LandSat: ",collection_info$collection, " For ",start_date," and ", end_date, " on ", c(collection_info$bands)))
    landsat_img <- ee$ImageCollection(collection_info$collection)$
      filterDate(start_date, end_date)$
      filterBounds(ee_polygon)$
      select(collection_info$bands)$
      sort('system:time_start')
      #map(maskL457sr) #cloud mask, apply post.
    
    # Attempt to get the nearest image
    nearest_image <- landsat_img$aggregate_array('system:index')  #
   
    # Get info about the nearest image, whether it matches exactly or is simply the closest
    info_nearest_image <- tryCatch({
      info <- nearest_image$getInfo()
      if (is.null(info)) {
        list(error = "No images available even after expanding search range.")
      } else {
        info
      }
    }, error = function(e) {
      list(error = "Error retrieving data: " %||% e$message)
      print(paste0("Error Encountered " %||% e$message))
    })
##############
    first_image <- landsat_img$first()
    
    info_first_image <- first_image$getInfo()
    print(paste0(info_first_image$id))
    if(info_first_image$id %>% is.null()){

      Error_51 <- paste0("ERROR in ",data_copy$SSBS[i], ": no images"," From ", data_copy$Sample_start_earliest[i])
      Error_List <- c(Error_List, as.character(Error_51))
        message("ERROR: ","Image for ", data_copy$SSBS[i], ", From ", data_copy$Sample_start_earliest[i], " to ", data_copy$Sample_end_latest[i]," is NULL ", info_first_image$id, ".")
    }
    else {
    message("First Image ID for ", data_copy$SSBS[i], " From ", data_copy$Sample_start_earliest[i], " to ", data_copy$Sample_end_latest[i]," is ", info_first_image$id, ".")
    }
    info_string <- paste(info_nearest_image, collapse = "; ")
    results[[i]] <- info_string
  }

    
  #   # Store composite image info for now
  #   median_composite <- landsat_img$median()
  #   results[[i]] <- median_composite$getInfo()
  # }
  
  # Combine results with the original data subset
  combined_data <- cbind(data_copy, do.call(rbind, results))
  Error_List<- paste(Error_List, collapse = "\n")
  if(Error_List != "") {
    message("Error List Retained; Error within the following: \n", Error_List)
    formatted_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
    Error_Log <- paste0("error_list_", formatted_time, ".txt")
    writeLines(Error_List, con = Error_Log)
    message("Error Log Created at: ", Error_Log , ".")
  } else {
    
    message("No Errors Encounted, Error Log Removed")
    rm(Error_List)
  }


  names(combined_data)[names(combined_data) == 'do.call(rbind, results)'] <- "Image_IDs"
  return(combined_data)

}


# Example usage assuming 'df' is your data frame with appropriate columns
# Example usage:
# Assuming results is your data frame and 'start_date', 'end_date' are the date columns
result03062024 <- process_sentinel_images(PREDSStime, "Longitude", "Latitude", c('Sample_start_earliest', 'Sample_end_latest'))
a <- PREDSStime[1:20, ]

A <- process_sentinel_images(a, "Longitude", "Latitude", c('Sample_start_earliest', 'Sample_end_latest'))

View(A)



result %>% View()
count_na(result, "Image_IDs")
count_missing(result03062024, "Image_IDs")
blank04062024<-extract_blank_rows(result03062024, "Image_IDs")
View(blank %>% filter( Sample_start_earliest_Year ==2008))
Error_List %>% View()
#result26052024<- result
palette <- colorRampPalette(brewer.pal(12, "Set3"))(20)

  ggplot(blank, aes(x = Sample_start_earliest_Year, fill = Source_ID)) + 
  geom_histogram(position = "stack", binwidth = 1, color = "black") +
  # Adds color distinction for different SSBS types
  theme_minimal() +
  labs(title = "Number of Rows by Year, Stacked by SSBS",
       x = "Year",
       y = "Count of Rows")+
  theme(legend.position = "none") 
  
 ggplot(blank_filtered, aes(x = Sample_start_earliest_Year, fill = Region)) + 
    geom_histogram(position = "stack", binwidth = 1, color = "black") +
    # Adds color distinction for different SSBS types
    scale_fill_manual(values = palette) +
    theme_minimal() +
    labs(
         x = "Year",
         y = "Number of Unique Surveys with Failed Retrivals")+
    scale_x_continuous(breaks = seq(min(blank$Sample_start_earliest_Year), max(blank$Sample_start_earliest_Year), by = 1))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Australia 2001 fails, can use Landsat 7 to mitigate that.

