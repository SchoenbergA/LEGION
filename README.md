### LEGION
A tool for automated calculating several artifically layer (eg for maschine learning approches)

# Install from github
require(devtools)
# for actual tested version
devtools::install_github("SchoenbergA/LEGION@master")

# for current step of developing (may contain bugs and not run as expected)
devtools::install_github("SchoenbergA/LEGION@develop")

# Supported Inputformats
# Filters
Single Rasterlayers or Stacks. The functions will add a tag with filtertyp and Size after the layername. Chekc names() if the input raster has given layernames and if needed set layernames

# Indizes

RGB requires a 3 band Stack with 1=red, 2=green, 3=blue
multispectral (RGB+nir) with 1=red, 2=green, 3=blue, 4=nir
