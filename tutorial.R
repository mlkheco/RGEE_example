library(rgee)
library(reticulate)
library(sf)
library(dplyr)
library(lubridate)
library(rgeeExtra)

#The rgee github page is also helpful to get started! 
#https://github.com/r-spatial/rgee
#install Python, Miniconda
reticulate::install_miniconda(force = TRUE)
rgee::ee_install()

system("curl -sSL https://sdk.cloud.google.com | bash")

ee_install(
  py_env = "rgee",
  earthengine_version = ee_version(),
  python_version = "3.12.3",
  confirm = interactive()
)



win_py_path = paste0(
  "C:/Users/CT/AppData/Local/r-miniconda/",
  "python.exe"
)
ee_install_set_pyenv(
  py_path = win_py_path,
  py_env = "rgee" # Change it for your own Python ENV
)
#Check your R session project/global options to see if it is applied!  
#it should look something like:
#"Python Codes/conda/installation/envs/rgee/python.exe"
#Make sure the "rgee" is contained in the folder path


# Install Google Cloud SDK 
#This is not always necessary, but is recommended by rgee's author
system("curl -sSL https://sdk.cloud.google.com | bash")

# Set global parameters
Sys.setenv("RETICULATE_PYTHON" = sprintf("%s/.local/share/r-miniconda/bin/python3", HOME))
Sys.setenv("EARTHENGINE_GCLOUD" = sprintf("%s/google-cloud-sdk/bin/", HOME))


reticulate::py_config()
  # python:         C:/Users/CT/AppData/Local/r-miniconda/python.exe
  # libpython:      C:/Users/CT/AppData/Local/r-miniconda/python313.dll
  # pythonhome:     C:/Users/CT/AppData/Local/r-miniconda
  # version:        3.13.9 | packaged by Anaconda, Inc. | (main, Oct 21 2025, 19:09:58) [MSC v.1929 64 bit (AMD64)]
  # Architecture:   64bit
  # numpy:           [NOT FOUND]
  # ee:             [NOT FOUND]
  # 
  # NOTE: Python version was forced by use_python() function

#I highly recommend installing the api and numpy manually before trying anything else.
#ee initialization fails sometimes and you can coerce it to go through with the ee python package!
reticulate::py_install("earthengine-api", pip = TRUE)
reticulate::py_install("numpy", pip = TRUE)

ee <- import('ee')

rgee::ee_install_upgrade() #

ee <- import('ee') #python and GEE objects must be initizated again each time R restarts



#Authenticate your account, this will redirect you to google
#Generate a token and enter it in the R console
ee_Authenticate()


ee$Initialize() #Python initialize is Required if a bug is encountered, initialize with python first before rgee.

#put your name here, this is not always necessary, but if ee_Initalize asks for your root folder location, you can just press ESC.
ee$data$createAssetHome("users/Matthew_Li") 

ee_Initialize()  # Initialize the Earth Engine API and stop it from "UPGRADING" 
#Newer versions of the GEE API sometimes doesn't work with rgee

rgee::ee_check()

#Note, This function sometimes doesn't work with newer python versions
#Downgrade to python 3.8.19 and it should work properly
Error_List <- list()
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
  
  
  
  source("select_landsat_collection.R")
  
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


# Example usage with PREDICTS, if you don't have PREDICTS, ensure you align the data cols and the range you want to extract in the function and call

a <- PREDSStime[1:20, ]

A <- process_sentinel_images(a, "Longitude", "Latitude", c('Sample_start_earliest', 'Sample_end_latest'))

View(A)

#This should pair your datasets with landsat images, check A to see if theres a new col called image_ids
#head over to leaflet integration example.