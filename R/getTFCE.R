#' @keywords internal
#' @importFrom parallel mclapply
#' @importFrom raster clump

getTFCE=function(tmat,int=0.1,E=0.5,H=2,parallel=T,cores=2){
  hlim.p=seq(0,max(tmat),int)
  hlim.n=seq(0,max(-tmat),int)
  if(parallel==T){
    pos=mclapply(hlim.p,getOneTFCE,tmat=tmat,E=E,H=H,mc.cores=cores)
    pos=Reduce('+', pos)
    neg=mclapply(hlim.n,getOneTFCE,tmat=-tmat,E=E,H=H,mc.cores=cores)
    neg=Reduce('+', neg)
  }else{
    pos=lapply(hlim.p,getOneTFCE,tmat=tmat,E=E,H=H)
    pos=Reduce('+', pos)
    neg=lapply(hlim.n,getOneTFCE,tmat=-tmat,E=E,H=H)
    neg=Reduce('+', neg)
  }
  both=pos+neg
  return(both*int)
}
