#' @keywords internal

getNullMat=function(x,mats,group,gridsize){
  g.cats=unique(group)
  group[group==g.cats[1]]=1
  group[group==g.cats[2]]=2

  nullgroup=sample(group,length(group))

  outmat=matrix(mats,nrow=length(nullgroup),ncol=prod(gridsize),byrow=T)
  omat1=outmat[nullgroup==1,]
  omat2=outmat[nullgroup==2,]
  num1=sum(nullgroup==1)
  num2=sum(nullgroup==2)
  rm(outmat)

  m1=colMeans(omat1)
  m2=colMeans(omat2)
  v1=colSums((t(t(omat1)-colMeans(omat1)))^2)/(num1-1)
  v2=colSums((t(t(omat2)-colMeans(omat2)))^2)/(num2-1)

  ts=(m1-m2)/sqrt(v1/num1+v2/num2)
  ts[is.na(ts)]<-0
  ts[(v1==0 | v2==0)]<-0
  maxval=max(abs(ts))

  return(maxval)
}
