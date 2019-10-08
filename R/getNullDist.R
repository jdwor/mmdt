#' @keywords internal
#' @importFrom pbapply pbmapply
#' @importFrom pbmcapply pbmcmapply
#' @importFrom parallel mcmapply

getNullDist=function(method,nsim,mats,group,gridsize,parallel,pb,cores){
  if(parallel==TRUE){
    if(pb==TRUE){
      nulldists=pbmclapply(as.list(1:nsim),getNullMat,mats=mats,
                           group=group,gridsize=gridsize,method=method,
                           mc.cores=cores)
    }else{
      nulldists=mclapply(as.list(1:nsim),getNullMat,mats=mats,
                         group=group,gridsize=gridsize,method=method,
                         mc.cores=cores)
    }
  }else{
    if(pb==TRUE){
      nulldists=pblapply(as.list(1:nsim),getNullMat,mats=mats,
                           group=group,gridsize=gridsize,method=method)
    }else{
      nulldists=lapply(as.list(1:nsim),getNullMat,mats=mats,
                       group=group,gridsize=gridsize,method=method)
    }
  }

  if(method=="maxt" | method=="tfce"){
    nulldists=unlist(nulldists)
  }else if(method=="both"){
    nulldists=do.call(rbind,nulldists)
  }else{
    stop("method must be either 'maxt', 'tfce', or 'both'")
  }
  return(nulldists)
}
