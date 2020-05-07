#' @keywords internal
#' @importFrom RNifti readNifti

readSubject<-function(x,masks,modals,ids,groups){
  if(!file.exists(masks[x])){
    stop("Error reading subjects: Make sure your working directory is compatible
         with the filepaths listed in your mask and modal# vectors.")
  }
  mask=RNifti::readNifti(masks[x])
  id=ids[x]
  group=groups[x]

  ims=lapply(modals, "[[", x)
  mat=matrix(nrow=sum(mask==T),ncol=length(ims)+2)
  mat[,1]=id
  mat[,2]=group
  for(i in 1:length(ims)){
    if(!file.exists(ims[[i]])){
      stop("Error reading subjects: Make sure your working directory is compatible
         with the filepaths listed in your mask and modal# vectors.")
    }
    thisim=RNifti::readNifti(ims[[i]])
    thisim=thisim[mask==T]
    mat[,i+2]=thisim
  }
  return(mat)
}
