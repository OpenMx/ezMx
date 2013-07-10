# ezMx

EasyMx is a library of functions for users of [OpenMx](http://openmx.psyc.virginia.edu/) and [R](http://cran.r-project.org/)

It's goals are:

1. Make getting work done with  OpenMx as easy as reasonable.
2. Help users familiar with R get into OpenMx.
3. Help users new to SEM get into doing SEM with OpenMx.


### Functions mapping from non SEM statistics to SEM

`ezMxLm(y ~ x)` # This function implements lm in SEM

ezMx_tTest(grp1 = y, grp2 = x, dependent = FALSE){

ezMx_tTest(y ~ Grp)

Synonyms for this are `ezMxANOVA()` and `ezMxRegression()`

### 1-line EFA/CFA

ezMxFactorModel(factors = 1, errorsEqual = FALSE){
ezMxFactorModel(F1 + F2 ~ x1 + x2 + x3")
ezMxFactorModel(latents=c("F1", "F2", ...), to=list(F1=c("x1", "x2", "x3"), F2=c("x3", "x4", "x5", ...))

ezMxLGCM <- function(timepoints = 4, errorsEqual = FALSE) {
	# Alternate:    ezMxFactors(to=c("x1", "x2", "x3), ... )
}

ezMxMeasurementInvariance <- function(from = c("F1", "F2", ...), to = list(F1 = c("x1", "x2", "x3"), F2 = c("x3", "x4", "x5", ...), groups = c("school")){
	# repurpose semTools code, or give them OpenMx code to handle our models?
}

ezMxACEModel<- function(twin1 = x, twin2 = y, ...){
   # Not sure how else to do it.
}

# Helpers:
ezMxSimpleThresholds <- function(model, data, useStandardNormal = TRUE) {
	#<- generates threshold expectations for a RAM or LISREL model
}

# Outputs:
ezMxPlot <- function(model) {
	# <- plots a RAM or LISREL model
}

ezMxTable <- function(model, ...) {
	#<- plot a publication-qualty table of output stats
}

# And maybe:
lavaan.to.OpenMx <- function(HW.model){
	# 
}

# HW.model ' visual  =~ x1 + x2 + x3
#            textual =~ x4 + x5 + x6
# 		   speed   =~ x7 + x8 + x9 '
# 
# mxModel(lavaan.to.OpenMx(HW.model), mxData(...), ...)
