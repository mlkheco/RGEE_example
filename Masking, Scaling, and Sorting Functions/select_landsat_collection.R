  # Function to select the appropriate Landsat collection based on date
  select_landsat_collection <- function(date) {
    # Convert input to a date object if it's not already
    date <- as.Date(date, format="%Y-%m-%d")
    # LC=Operational Land Imager (OLI), LE=Enhanced Thematic Mapper Plus (ETM+), LM=MSS, LT=TM
    if (date >= as.Date("2021-01-01")) {
      # Landsat 9, Collection 2
      # (Band 1: Coastal/Aerosol, Band 2: Blue, Band 3: Green, Band 4: Red, Band 5: NIR, Band 6: SWIR 1, Band 7: SWIR 2, Band 8: Panchromatic, Band 9: Cirrus, Band 10: TIRS 1, Band 11: TIRS 2)
      return(list(collection = 'LANDSAT/LC09/C02/T1_L2', bands = c('SR_B4', 'SR_B3', 'SR_B2', 'SR_B5')))
    } else if (date >= as.Date("2013-01-01")) {
      # Landsat 8, Collection 2
      # (Band 1: Coastal/Aerosol, Band 2: Blue, Band 3: Green, Band 4: Red, Band 5: NIR, Band 6: SWIR 1, Band 7: SWIR 2, Band 8: Panchromatic, Band 9: Cirrus, Band 10: TIRS 1, Band 11: TIRS 2)
      return(list(collection = 'LANDSAT/LC08/C02/T1_L2', bands = c('SR_B4', 'SR_B3', 'SR_B2', 'SR_B5')))
      
    } 
    
    else if  (date >= as.Date("1984-01-01") && date < as.Date("2011-11-01")) {
        # Use Landsat 5 instead of Landsat 7 after May 2003 to avoid striping issue
        # (Band 1: Blue, Band 2: Green, Band 3: Red, Band 4: NIR, Band 5: SWIR 1, Band 6: Thermal, Band 7: SWIR 2)
        return(list(collection = 'LANDSAT/LT05/C02/T1_L2', bands = c('SR_B3', 'SR_B2', 'SR_B1', 'SR_B4')))
      }
  #landsat 7 was onnly fully operable for such a short period that landsat 5 covered it is easier to just ignore it
    # } else if (date >= as.Date("1999-01-01")) {
    #   # Landsat 7, Collection 2 (Before May 2003)
    #   # (Band 1: Blue, Band 2: Green, Band 3: Red, Band 4: NIR, Band 5: SWIR 1, Band 6: Thermal, Band 7: SWIR 2, Band 8: Panchromatic)
    #   return(list(collection = 'LANDSAT/LE07/C02/T1', bands = c('SR_B3', 'SR_B2', 'SR_B1', 'SR_B4')))
          # } else if (date >= as.Date("1984-01-01")) {
          #   # Landsat 5, Collection 2
          #   # (Band 1: Blue, Band 2: Green, Band 3: Red, Band 4: NIR, Band 5: SWIR 1, Band 6: Thermal, Band 7: SWIR 2)
          #   return(list(collection = 'LANDSAT/LT05/C02/T1_L2', bands = c('B3', 'B2', 'B1', 'B4')))
 else if (date >= as.Date("1982-01-01")) {
      # Landsat 4, Collection 2
      # (Band 1: Blue, Band 2: Green, Band 3: Red, Band 4: NIR, Band 5: SWIR 1, Band 6: Thermal, Band 7: SWIR 2)
      return(list(collection = 'LANDSAT/LT04/C02/T1_L2', bands = c('SR_B3', 'SR_B2', 'SR_B1', 'SR_B4')))
    } else {
      # Landsat 1-5 MSS, Collection 2
      # (Band 1: Green, Band 2: Red, Band 3: NIR, Band 4: NIR, Band 5: Thermal)
      return(list(collection = 'LANDSAT/LM01/C02/T1', bands = c('B2', 'SR_B1', 'SR_B3', 'SR_B4')))
    }
  }
