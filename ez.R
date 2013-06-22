# base
ezmxRegression <- function(y ~ x){
	# Alternate: ezmxRegression(from="x", to="y")
} 	
ezmx_tTest <- function(grp1 = y, grp2 = x, dependent = FALSE){
	# ezmxMeansTest(y | Grp)
}
ezmxANOVA <- function(x, conditions = c(grp1, grp2, grp3)){
 # Alternate: ezmxMeansTest(y, | c(G1, G2, ...)
}
ezmxRegression <- function(y ~ x1 + x2 * x3){
	# Alternate: ezmxRegression(from=c(...), to="y")
}

ezmxFactorModel <- function(factors = 1, errorsEqual = FALSE){
	# Alternate:    ezmxFactors(latents=c("F1", "F2", ...), to=list(F1=c("x1", "x2", "x3"), F2=c("x3", "x4", "x5", ...)) ]
}
ezmxLGCM <- function(timepoints = 4, errorsEqual = FALSE) {
	# Alternate:    ezmxFactors(to=c("x1", "x2", "x3), ... )
}
ezmxMeasurementInvariance <- function(from = c("F1", "F2", ...), to = list(F1 = c("x1", "x2", "x3"), F2 = c("x3", "x4", "x5", ...), groups = c("school")){
	# repurpose semTools code, or give them OpenMx code to handle our models?
}
ezmxACEModel<- function(twin1 = x, twin2 = y, ...){
   # Not sure how else to do it.
}

# Helpers:
ezmxSimpleThresholds <- function(model, data, useStandardNormal = TRUE) {
	#<- generates threshold expectations for a RAM or LISREL model
}

# Outputs:
ezmxPlot <- function(model) {
	# <- plots a RAM or LISREL model
}
ezmxTable <- function(model, ...) {
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