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
      nulldists=unlist(nulldists)
    }else{
      nulldists=mclapply(as.list(1:nsim),getNullMat,mats=mats,
                         group=group,gridsize=gridsize,method=method,
                         mc.cores=cores)
      nulldists=unlist(nulldists)
    }
  }else{
    if(pb==TRUE){
      nulldists=pblapply(as.list(1:nsim),getNullMat,mats=mats,
                           group=group,gridsize=gridsize,method=method)
      nulldists=unlist(nulldists)
    }else{
      nulldists=lapply(as.list(1:nsim),getNullMat,mats=mats,
                       group=group,gridsize=gridsize,method=method)
      nulldists=unlist(nulldists)
    }
  }

  return(nulldists)
}
