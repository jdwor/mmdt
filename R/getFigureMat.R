#' @keywords internal

getFigureMat=function(pmat,tmat){
  mat=pmat
  mat[pmat>.05]=0
  mat[pmat<.05 & tmat>0]=1
  mat[pmat<.05 & tmat<0]=-1
  mat[!(mat%in%c(1,-1))]=0
  return(mat)
}
