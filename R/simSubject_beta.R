#' @keywords internal
#' @importFrom mvtnorm rmvnorm
#' @importFrom MASS mvrnorm
#' @importFrom stats rbeta
#' @importFrom stats rbinom

simSubject_beta=function(group,voxels,a1,b1,a2,b2){
  v1=rbinom(1,voxels,1/3)
  v2=voxels-v1
  if(group==1){
    thet=rbeta(1,a1,b1)*.7
    mu <- rep(0,2)
    sigma <- matrix(thet, nrow=2, ncol=2) + diag(2)*(1-thet)
    rawvars=mvrnorm(n=v1, mu=mu, Sigma=sigma)
    pvars=pnorm(rawvars)
    bvars=qbeta(pvars,5,5)
    bvars[,1]=3*bvars[,1]-3; bvars[,2]=3*bvars[,2]
    bvars2=mvrnorm(v2,c(0,0),matrix(c(.6,0,0,.6),nrow=2))
    return(rbind(bvars,bvars2))
  }else if(group==2){
    thet=rbeta(1,a2,b2)*.7
    mu <- rep(0,2)
    sigma <- matrix(thet, nrow=2, ncol=2) + diag(2)*(1-thet)
    rawvars=mvrnorm(n=v1, mu=mu, Sigma=sigma)
    pvars=pnorm(rawvars)
    bvars=qbeta(pvars,5,5)
    bvars[,1]=3*bvars[,1]-3; bvars[,2]=3*bvars[,2]
    bvars2=mvrnorm(v2,c(0,0),matrix(c(.6,0,0,.6),nrow=2))
    return(rbind(bvars,bvars2))
  }else{
    stop("'group' must be 1 or 2")
  }
}
