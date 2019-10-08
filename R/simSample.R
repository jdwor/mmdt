#' @keywords internal

simSample=function(n1,n2,voxels,a1,b1,a2,b2){
  df1=NULL
  df2=NULL
  for(i in 1:n1){
    tdf1=cbind(rep(i,voxels),rep(1,voxels),
               simSubject(group=1,voxels=voxels,
                          a1=a1,b1=b1,a2=a2,b2=b2))
    df1=rbind(df1,tdf1)
  }
  for(i in (n1+1):(n1+n2)){
    tdf2=cbind(rep(i,voxels),rep(2,voxels),
               simSubject(group=2,voxels=voxels,
                          a1=a1,b1=b1,a2=a2,b2=b2))
    df2=rbind(df2,tdf2)
  }

  df=rbind(df1,df2)
  mmdt.obj=list(ids=df[,1],groups=df[,2],modals=df[,3:ncol(df)])
  return(mmdt.obj)
}
