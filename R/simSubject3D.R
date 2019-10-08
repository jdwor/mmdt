#' @keywords internal
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats rbeta
#' @importFrom stats rbinom

simSubject3D=function(group,voxels,a1,b1,a2,b2){
  v1=rbinom(1,voxels,.5)
  v2=voxels-v1
  if(group==1){
    p1=rbeta(1,a1,b1)
    p2=1-p1
    diffset=rmvnorm(v1,c(-1,1,-1),matrix(c(p1*4/9+p2,p1*4/15,4/15,
                                        p1*4/15,p1*4/9+p2,4/15,
                                        4/15,4/15,4/9),nrow=3,ncol=3))
    return(rbind(diffset,rmvnorm(v2,c(1,-1,1),matrix(c(4/9,4/15,4/15,
                                                     4/15,4/9,4/15,
                                                     4/15,4/15,4/9),
                                                   nrow=3,ncol=3))))
  }else if(group==2){
    p1=rbeta(1,a2,b2)
    p2=1-p1
    diffset=rmvnorm(v1,c(-1,1,-1),matrix(c(p1*4/9+p2,p1*4/15,4/15,
                                          p1*4/15,p1*4/9+p2,4/15,
                                          4/15,4/15,4/9),nrow=3,ncol=3))
    return(rbind(diffset,rmvnorm(v2,c(1,-1,1),matrix(c(4/9,4/15,4/15,
                                                     4/15,4/9,4/15,
                                                     4/15,4/15,4/9),
                                                   nrow=3,ncol=3))))
  }else{
    stop("'group' must be 1 or 2")
  }
}
