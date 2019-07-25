#' @keywords internal
#' @importFrom pbapply pbmapply
#' @importFrom pbmcapply pbmcmapply
#' @importFrom parallel mcmapply

getStatMat=function(mats,group,gridsize,parallel,pb,cores){
  g.cats=unique(group)
  group[group==g.cats[1]]=1
  group[group==g.cats[2]]=2
  outmat=matrix(mats,nrow=length(group),ncol=prod(gridsize),byrow=T)
  omat1=outmat[group==1,]
  omat2=outmat[group==2,]
  num1=sum(group==1)
  num2=sum(group==2)
  rm(outmat)

  m1=colMeans(omat1)
  m2=colMeans(omat2)
  v1=colSums((t(t(omat1)-colMeans(omat1)))^2)/(num1-1)
  v2=colSums((t(t(omat2)-colMeans(omat2)))^2)/(num2-1)

  ts=(m1-m2)/sqrt(v1/num1+v2/num2)
  ts[is.na(ts)]<-0
  ts[(v1==0 | v2==0)]<-0
  ts=array(ts,dim=gridsize)

  dfnum=(v1/num1+v2/num2)^2
  dfdenom=(v1^2)/((num1-1)*num1^2)+(v2^2)/((num2-1)*num2^2)
  dfs=dfnum/dfdenom

  if(parallel==TRUE){
    if(pb==TRUE){
      ps=pbmcmapply(getStatSig,ts,dfs,
                    mc.cores=cores)
    }else{
      ps=mcmapply(getStatSig,ts,dfs,
                  mc.cores=cores)
    }
  }else{
    if(pb==TRUE){
      ps=pbmapply(getStatSig,ts,dfs)
    }else{
      ps=mapply(getStatSig,ts,dfs)
    }
  }
  ps[is.na(ps)]<-1
  ps=array(ps,dim=gridsize)

  return(list(ts=ts,ps=ps))
}
