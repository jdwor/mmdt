#' @keywords internal
#' @importFrom raster raster
#' @importFrom raster clump

getOneTFCE=function(x,tmat,E=0.5,H=2){
  clumps=clump(raster(tmat>x), directions=4)
  clumps=as.matrix(clumps)
  ctab=table(clumps)
  exts=clumps
  exts[!is.na(exts)]=as.numeric(ctab[exts[!is.na(exts)]])
  exts[is.na(exts)]=0
  return((exts^E)*(tmat^H))
}
