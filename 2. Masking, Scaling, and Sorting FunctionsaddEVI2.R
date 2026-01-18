py_run_string("
def add_evi2LS457(image):
    # Apply existing masks (assuming maskL457sr is already defined and imported)
    masked_image = maskL457sr(image)

    # Calculate evi2
    #Be sure to check variable names!, e.g. SR_B3 is the red band for LS457
    evi2 = image.expression(
    '2.5 * ((NIR* 0.0001 - RED* 0.0001) / ((NIR* 0.0001 ) * ( +2.4* RED* 0.0001) +1))',
    {
        'NIR': image.select('SR_B4').toFloat(),
        'RED': image.select('SR_B3')
    },
)
    evi2 = evi2.rename('evi2')

    # Add evi2 band to the masked image
    return masked_image.addBands(evi2)
")

# Access this function from R
add_evi2LS457 <- py$add_evi2LS457
