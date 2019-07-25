#' @keywords internal
#' @importFrom ks kde

getSubDens=function(x,data,ids,mins,maxs,gridsize,H){
  un.ids=unique(ids)
  idi=as.character(un.ids)[x]
  sub1=data[ids==idi,]

  if(!is.null(H)){
    out=kde(x=as.matrix(sub1),xmin=mins,xmax=maxs,
            gridsize=gridsize,H=H)
  }else{
    out=kde(x=as.matrix(sub1),xmin=mins,xmax=maxs,
            gridsize=gridsize)
  }
  out=out$estimate
  out[is.na(out)]<-0

  return(out)
}
