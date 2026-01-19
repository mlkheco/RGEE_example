# RGEE_example
RGEE example material for Opportunist meeting 8/12/2025

These functions are LandSat specific, but can easily be converted to other satelites or datasets, be aware of the scaling factors and calculations when converting the code.

You can also look at the github page for the rgee package for more guidelines on installation and usage.

#Prerequisites:

Patiences and retrys (sometimes functions fail for no reason but works when called again)

PREDICTS dataset (publically avaliable, not included here)

~~Miniconda and python, installing GEE on python~~ (This is no longer necessary in recent rgee versions.)



File Order & usages:

1.tutorial.R (and install miniconda + python setup)

2.Initialize mask and EVI functions (reticulate py objects, has to be re-initialized everytime R restarts)

3.mapimageget2.R (Get image url for each site initialized in tutorial)

4.Leaflet integration example.R (Visualize obtained, cleaned composite image url in R console via leaflet)
