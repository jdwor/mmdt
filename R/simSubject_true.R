#' @keywords internal
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats rbeta
#' @importFrom stats pnorm
#' @importFrom stats qbeta

simSubject_true=function(voxels,thet){
  thet=thet*.7
  v1=rbinom(1,voxels,1/3)
  v2=voxels-v1
  mu <- rep(0,2)
  sigma <- matrix(thet, nrow=2, ncol=2) + diag(2)*(1-thet)
  rawvars=mvrnorm(n=v1, mu=mu, Sigma=sigma)
  pvars=pnorm(rawvars)
  bvars=qbeta(pvars,5,5)
  bvars[,1]=3*bvars[,1]-3; bvars[,2]=3*bvars[,2]
  bvars2=mvrnorm(v2,c(0,0),matrix(c(.6,0,0,.6),nrow=2))
  return(rbind(bvars,bvars2))
}
