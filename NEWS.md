# LEGION 0.0.99.2
* major corrections
spelling in function documnetation
spelling in vignette (not for 'brute force' example)
update tutorial - add brute force example

# LEGION 0.0.99.1
Some corrections 

# LEGION 0.0.99.0
Beta Version rdy to release 1.0
* bugfixes
filter_Rst - fixed that sobel is not computed 2 times and added "mean" to docu.

* new features
add vignette with tutorial
add all new examples using all the same data.


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
