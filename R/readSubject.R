#' @keywords internal
#' @importFrom RNifti readNifti

readSubject<-function(x,masks,modals,ids,groups){
  mask=masks[x]
  id=ids[x]
  group=groups[x]

  ims=lapply(modals, "[[", x)
  mat=matrix(nrow=sum(mask==T),ncol=length(ims)+2)
  mat[,1]=id
  mat[,2]=group
  for(i in 1:length(ims)){
    thisim=RNifti::readNifti(ims[i])
    thisim=thisim[mask==T]
    mat[,i+2]=thisim
  }
  return(mat)
}
