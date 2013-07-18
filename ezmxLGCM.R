# Run LGCM with minimal input from user. Only variable names
# for measured growth and datafile are required

ezmxLGCM <- function(to=NULL,data=NULL,errorsEqual=F, ...){
  latents <- c('i','s')
  manifests <- to
  if(is.null(to)){
    cat("No variables listed for ezmxLGCM.\n")
    return()
  }
  if(length(to)<=2){
    cat("Too few time points listed for a ezmxLGCM.\n")
    return()
  }
  if(is.null(data)){
    cat("No data to fit ezmxLGCM with.\n")
    return()
  }
  else{
    complete.model <- mxModel("LGCM", manifestVars=manifests, latentVars=latents, type="RAM",
                              mxData(observed=data,type="raw"))
  }
  complete.model <- mxModel(complete.model,mxPath(from="i", to=manifests, arrows=1, free=F, values=1))
  complete.model <- mxModel(complete.model,mxPath(from="s", to=manifests, arrows=1, free=F, values=seq(0,(length(manifests)-1),1)))
  complete.model <- mxModel(complete.model,mxPath(from="one",to=latents,arrows=1,free=T,values=1,labels=c("mui","mus")))
  complete.model <- mxModel(complete.model,mxPath(from="one",to=manifests,arrows=1,free=F,values=0))
  
  if(errorsEqual){
    complete.model <- mxModel(complete.model,mxPath(from=manifests,to=manifests,arrows=2,free=T,values=0,labels=rep(paste("e"),length(manifests))))
  }
  else{
    complete.model <- mxModel(complete.model,mxPath(from=manifests,to=manifests,arrows=2,free=T,values=0,labels=paste("e",seq(1,length(manifests),1),sep="")))
  }
  complete.model <- mxModel(complete.model,mxPath(from=latents,to=latents,arrows=2,free=T,values=1,labels=c("vari","vars")))
  complete.model <- mxModel(complete.model,mxPath(from="i",to="s",arrows=2,free=T,values=1,labels="covis"))
  fit <- mxRun(complete.model)
  print(summary(fit,...))
  return(fit)  
}

#model <- ezmxLGCM(to=c("o1","o2","o3","o4"),data=data,errorsEqual=T)
#model <- ezmxLGCM(to=c("o1","o2","o3","o4"),data=NULL,errorsEqual=T)
#model <- ezmxLGCM(to=c("o1","o2"),data=data,errorsEqual=T)
#model <- ezmxLGCM(to=c("o1","o2","o3","o4"),data=data,errorsEqual=F)
