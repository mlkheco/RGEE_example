# Define the Python cloud masking function as a string
py_run_string( "
def maskL8sr(image):
    # Bits are defined as follows:
    # Bit 0 - Fill
    # Bit 1 - Dilated Cloud
    # Bit 2 - Cirrus
    # Bit 3 - Cloud
    # Bit 4 - Cloud Shadow
    qa_pixel = image.select('QA_PIXEL')
    qa_mask = qa_pixel.bitwiseAnd(int('11111', 2)).eq(0)
    
    qa_radsat = image.select('QA_RADSAT')
    saturation_mask = qa_radsat.eq(0)
    
    # Apply the scaling factors to the appropriate bands
    optical_bands = image.select('SR_B.*').multiply(0.0000275).add(-0.2)
    thermal_bands = image.select('ST_B.*').multiply(0.00341802).add(149.0)
    
    # Replace the original bands with the scaled ones and apply the masks
    image = image.addBands(optical_bands, None, True)
    image = image.addBands(thermal_bands, None, True)
    image = image.updateMask(qa_mask)
    image = image.updateMask(saturation_mask)
    
    return image
")

# Get the function reference from Python
maskL8sr <- py$maskL8sr
