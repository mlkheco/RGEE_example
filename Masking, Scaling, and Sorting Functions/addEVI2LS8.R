py_run_string("
def add_evi2LS8(image):

    masked_image = maskL457sr(image)

    # Calculate evi2
    evi2 = image.expression(
    '2.5 * ((NIR* 0.0001 - RED* 0.0001) / ((NIR* 0.0001 ) * ( +2.4* RED* 0.0001) +1))',
    {
        'NIR': image.select('SR_B5').toFloat(),
        'RED': image.select('SR_B4')
    },
)
    evi2 = evi2.rename('evi2')

    # Add evi2 band to the masked image
    return masked_image.addBands(evi2)
")

# Access this new function from R
add_evi2LS8 <- py$add_evi2LS8
