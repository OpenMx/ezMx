implement
cor.test(~ CONT + INTG, data = USJudgeRatings)

plot <- function(y ~ x){
	just calls omxGraphViz()
}

ezMxPath <- function(y ~ x, verbose=T){
	# can return a list of single from to paths written out long-hand, as it were
	# from a:c to d
	# from a to d
	# from b to d
	# from c to d
		
}

# base
ezMx_tTest <- function(x = x, y = y, dependent = FALSE){
	# ezmxMeansTest(y ~ group)
	t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, ...)
	# t.test(formula, data, subset, na.action, ...)
}

ezMx_lm <- function(y ~ x){
	# Alternate: ezmxRegression(from="x", to="y")
    # exMx_lm()
} 	
   # tim bates going to do this?
   
ezMxRegression <- function(y ~ x){
	# Alternate: ezmxRegression(from="x", to="y")
    # exMx_lm()
} 	

exMxRegression <- function(y ~ x){
	# Alternate: exMxRegression(from="x", to="y")
    # exMx_lm()
} 	

exMxANOVA <- function(y~ x){
 # exMx_lm()
}

ezMxFactorModel <- function(factors = 1, errorsEqual = FALSE){
	# Alternate: ezMxFactors(latents=c("F1", "F2", ...), to=list(F1=c("x1", "x2", "x3"), F2=c("x3", "x4", "x5", ...)) ]
}

exMxLGCM <- function(timepoints = 4, errorsEqual = FALSE) {
	# Alternate:    exMxFactors(to=c("x1", "x2", "x3), ... )
}

exMxMeasurementInvariance <- function(from = c("F1", "F2", ...), to = list(F1 = c("x1", "x2", "x3"), F2 = c("x3", "x4", "x5", ...), groups = c("school")){
	# repurpose semTools code, or give them OpenMx code to handle our models?
}

exMxACEModel <- function(twin1 = x, twin2 = y, ...){
   # Not sure how else to do it.
   # tim bates going to do this?
}

# Helpers:
exMxSimpleThresholds <- function(model, data, useStandardNormal = TRUE) {
	#<- generates threshold expectations for a RAM or LISREL model
}

# Outputs:
exMxPlot <- function(model) {
	# <- plots a RAM or LISREL model
}
exMxTable <- function(model, ...) {
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
