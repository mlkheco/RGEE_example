
py_run_string("
import ee  # Ensure Earth Engine is imported in the Python environment

def maskL457sr(image):
    # Masks out clouds and cloud shadows using the QA band
    qa = image.select('QA_PIXEL')
    cloudShadowBitMask = (1 << 3)  # Adjust this if the bit position is different
    cloudsBitMask = (1 << 5)       # Adjust this if the bit position is different
    qaMask = qa.bitwiseAnd(cloudShadowBitMask).eq(0).And(qa.bitwiseAnd(cloudsBitMask).eq(0))
    
    saturationMask = image.select('QA_RADSAT').eq(0)
    
    # Apply the scaling factors to the appropriate bands
    optical_bands = image.select('SR_B.*').multiply(0.0000275).add(-0.2)
    thermal_bands = image.select('ST_B.*').multiply(0.00341802).add(149.0)
    
    # Replace the original bands with the scaled ones and apply the masks
    image = image.addBands(optical_bands, None, True)
    image = image.addBands(thermal_bands, None, True)
    # Apply masks
    image = image.updateMask(qaMask).updateMask(saturationMask)
    return image
")

# Get the function reference from Python
maskL457sr <- py$maskL457sr
