# LEGION 0.1.0.9
Development version for patch

*bugfixes
Now uses all depending packages in description.
* new features
detct_RstHgmy - function to detect homogeneity in RasterLayers.

# LEGION 0.1.0
Release for install via Github version

# LEGION 0.0.99.0
Beta Version rdy to release 1.0
* bugfixes
filter_Rst - fixed that sobel is not computed 2 times and added "mean" to docu.

* new features
add vignette with tutorial


# LEGION 0.0.1.2
* new features
detct_RstCor - now plots the cormatrix (but requires corrplot) and has bolean switch to either return the cleand Stack or the corMatrix.

# LEGION 0.0.1.1
* bugfixes
added NA removing for filters
* new features
detct_RstCor - function to detect correlations in Stacks and returns only layers with lesser correlation than selected tresholds

# LEGION 0.0.1.0
initial version

* new features
vegInd_RGB - computes several RGB based Indices
vegInd_Nir - computes several RGB+nir based Indices
filter_Rst - computes several filtered artifically layers
filter_Stk - wrapper for 'filter_Rst' to filter all layers in a Stack
