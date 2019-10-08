#' @keywords internal
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats rbeta

simSubject_true=function(voxels,p1,p2){
  v1=round(voxels/2,0)
  v2=voxels-v1
  diffset=rmvnorm(v1,c(-1,1),matrix(c(p1*4/9+p2,p1*4/15,
                                      p1*4/15,p1*4/9+p2),nrow=2,ncol=2))
  diffset=diffset[diffset[,1]<diffset[,2],]
  while(nrow(diffset)<v1){
    diffset=rbind(diffset,rmvnorm(v1-nrow(diffset),c(-1,1),
                                  matrix(c(p1*4/9+p2,p1*4/15,
                                           p1*4/15,p1*4/9+p2),nrow=2,ncol=2)))
    diffset=diffset[diffset[,1]<diffset[,2],]
  }
  return(rbind(diffset,rmvnorm(v2,c(1,-1),matrix(c(4/9,4/15,4/15,4/9),
                                                 nrow=2,ncol=2))))
}
