# lavaan to OpenMx model conversion code.
# Takes the lavaan model input and reads as
# an OpenMx model with attached data file 

# required libraries for conversion
#library(OpenMx)
#library(lavaan)

lavaan.to.OpenMx <- function(lavaan.model=NULL,data=NULL,ugroup=NULL,label="Default"){
  if(is.null(data)){
    cat("No data included , add 'data=yourdataset' as argument\n")
    return(NULL);
  }
  # Compile for multi group
  if(!is.null(ugroup)){
    a <- tryCatch(lavaan(lavaan.model,data,group=ugroup),
                  error = function(e) { cat(paste("Lavaan model not secified correctly -",e,sep="\n")) 
                                        return(NULL) } )
    x <- sort(as.matrix(data[a@Data@group]))
    x <- unique(x)
  }
  # Compile for single group
  else if(is.null(ugroup)){
    a <- tryCatch(lavaan(lavaan.model,data),
                  error = function(e) { cat(paste("Lavaan model not secified correctly -",e,sep="\n")) 
                                        return(NULL) } )
  }
  # Collect model parameters and separate manifest and latents
  allPars <- data.frame(a@ParTable)
  allNames <- union(allPars$lhs, allPars$rhs)
  manifests <- a@Data@ov.names[[1]]
  thresholds <- as.character(allPars$rhs[allPars$op=="|"])
  latents <- setdiff(allNames, c(manifests, thresholds, ""))
  # Initialize starting values for creating OpenMx model
  submodel <- list()
  grouping <- -999
  for(i in 1:nrow(allPars)) {
    row <- allPars[i,]
    # Create subgroup models for multigroup analyses
    if(grouping!=row$group){
      values <- list()
      free <- list()
      ovar<-NA
      tvar<-0
      dname <- c()
      grouping <- row$group
      temp <- paste(label,row$group,sep="")
      submodel[[grouping]] <- mxModel(temp, manifestVars=manifests, latentVars=latents, type="RAM")
    }
    # Read current data line of lavaan parameter file to run mxPath functions
    if(row$label==""){row$label <- NA}
    if(is.na(row$ustart)){row$ustart <- .1}
    if(row[7]!=0){
      row$ustart <- a@Fit@x[as.numeric(row[7])]
    }
    theFrom <- as.character(row$lhs)
    theOp <- as.character(row$op)
    # Factor
    if(theOp == "=~") {
      submodel[[grouping]] <- mxModel(submodel[[grouping]], mxPath(from=theFrom, to=as.character(row$rhs), arrows=1, free=(row$free!=0),values=row$ustart,labels=as.character(row$label)))
    } 
    # Co(Var)
    else if (theOp == "~~") {
      submodel[[grouping]] <- mxModel(submodel[[grouping]], mxPath(from=theFrom, to=as.character(row$rhs), arrows=2, free=(row$free!=0),values=row$ustart,labels=as.character(row$label)))
    } 
    # Mean
    else if (theOp == "~1") {
      submodel[[grouping]] <- mxModel(submodel[[grouping]], mxPath(from="one", to=theFrom, arrows=1, free=(row$free!=0),values=row$ustart,labels=as.character(row$label)))
    } 
    # Regression
    else if (theOp == "~") {
      submodel[[grouping]] <- mxModel(submodel[[grouping]], mxPath(from=as.character(row$rhs), to=theFrom, arrows=1, free=(row$free!=0),values=row$ustart,labels=as.character(row$label)))
    }
    # Threshold
    else if (theOp == "|") {
      # define as factor in mx, fix var at 1
      vlevels <- levels(as.factor(data[,theFrom]))
      data[,theFrom] <- mxFactor(data[,theFrom], levels=c(vlevels))
      submodel[[grouping]] <- mxModel(submodel[[grouping]], mxPath(from=theFrom, to=theFrom, arrows=2, free=F,values=1))
      # initial threshold representation
      if(is.na(ovar)){
        tvar<-tvar+1; ovar<-theFrom; dname <- cbind(dname, theFrom)
        free[[1]]<-(row$free!=0)
        values[[1]]<-row$ustart
      }
      # new variable found
      else if(theFrom!=ovar){
        tvar<-tvar+1; ovar<-theFrom; dname <- cbind(dname, theFrom)
        free[[tvar]]<-(row$free!=0)
        values[[tvar]]<-row$ustart
      }
      # else add to the list
      else{
        free[[tvar]]<-cbind(free[[tvar]],(row$free!=0))
        values[[tvar]]<-cbind(values[[tvar]],row$ustart)
      }
      lengths <- c()
      for(k in 1:length(free)){lengths[k]<-length(free[[k]])}
      aa <- array(F, c(max(lengths),length(free)))
      bb <- array(NA,c(max(lengths),length(free)))
      for(k in 1:length(free)){
        aa[1:length(free[[k]]),k]<-free[[k]]
        bb[1:length(values[[k]]),k]<-values[[k]]
      }
      aac <- c()
      bbc <- c()
      for(k in 1:max(lengths)){
        for(l in 1:length(free)){
          aac <- cbind(aac, aa[k,l])
          bbc <- cbind(bbc, bb[k,l])
        }
      }
      submodel[[grouping]] <- mxModel(submodel[[grouping]], mxMatrix(type="Full",nrow=max(lengths),ncol=length(free),
                                      free= c(aac),
                                      values= c(bbc),
                                      dimnames=list(c(),c(dname)),
                                      byrow=T,
                                      name="thresh"),
                                      mxRAMObjective(A="A",S="S",F="F",M="M",thresholds="thresh")
                                      )
    }
    
    else { stop(paste("I have no idea what I'm supposed to do with an operator of type ", theOp, ".", sep=""))}
  
    # Attach data to the model
    if(!is.null(ugroup)){
      submodel[[grouping]] <- mxModel(submodel[[grouping]],mxData(subset(data,data[a@Data@group]==x[row$group]),type="raw"))
    }
    else{
      submodel[[grouping]] <- mxModel(submodel[[grouping]],mxData(data,type="raw"))
    }
  }
  # Compute model objective for multi group possibility
  alg <- as.symbol(paste(label,1,".objective",sep=""))
  model <- mxModel(label,submodel[[1]])
  if(length(submodel)>1){
    for(i in 2:length(submodel)){
      model <- mxModel(model,submodel[[i]])
      alg <-substitute(x+nalg, list(x=alg, nalg=as.symbol(paste(label,i,".objective",sep=""))))
    }
  }
  model <- mxModel(model, eval(substitute(mxAlgebra(x, name="Total"),list(x=alg))),mxAlgebraObjective("Total"))
  # Return full model based on lavaan specs
  return(model)
}

#---------------------------------#

# Test Model estimates between two programs
#1 2 Group Growth Model
#summary(lavaan(lavaan.model1,dataset,group="agegroup"))
#model <- lavaan.to.openmx(lavaan.model1,dataset,"agegroup","TestModel")
#summary(mxRun(model))
#2 No Grouping Growth Model
#summary(lavaan(lavaan.model2,dataset))
#model <- lavaan.to.openmx(lavaan.model2,dataset,NULL,"TestModel")
#summary(mxRun(model))
