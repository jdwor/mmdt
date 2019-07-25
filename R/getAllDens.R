#' @keywords internal
#' @importFrom pbapply pblapply
#' @importFrom pbmcapply pbmclapply
#' @importFrom parallel mclapply

getAllDens=function(data,ids,groups,gridsize,mins,maxs,H,parallel,pb,cores){
  un.ids=unique(ids)
  cells=as.list(c(1:length(un.ids)))
  if(is.null(mins)){mins=apply(data,2,min)}
  if(is.null(maxs)){maxs=apply(data,2,max)}

  eval.points=getEvalPoints(mins,maxs,gridsize)
  mats=array(0,dim=c(gridsize,length(un.ids)))

  if(parallel==T){
    if(pb==TRUE){
      cellmats=pbmclapply(cells,getSubDens,data=data,ids=ids,
                          mins=mins,maxs=maxs,gridsize=gridsize,
                          H=H,mc.cores=cores)
    }else{
      cellmats=mclapply(cells,getSubDens,data=data,ids=ids,
                        mins=mins,maxs=maxs,gridsize=gridsize,
                        H=H,mc.cores=cores)
    }
  }else{
    if(pb==TRUE){
      cellmats=pblapply(cells,getSubDens,data=data,ids=ids,
                          mins=mins,maxs=maxs,gridsize=gridsize,H=H)
    }else{
      cellmats=lapply(cells,getSubDens,data=data,ids=ids,
                        mins=mins,maxs=maxs,gridsize=gridsize,H=H)
    }
  }

  group=rep(NA,length(un.ids))
  if(ncol(data)==2){
    for(i in 1:length(un.ids)){
      mats[,,i]<-cellmats[[i]]
      group[i]<-unique(groups[ids==un.ids[i]])
    }
  }else if(ncol(data)==3){
    for(i in 1:length(un.ids)){
      mats[,,,i]<-cellmats[[i]]
      group[i]<-unique(groups[ids==un.ids[i]])
    }
  }else if(ncol(data)==4){
    for(i in 1:length(un.ids)){
      mats[,,,,i]<-cellmats[[i]]
      group[i]<-unique(groups[ids==un.ids[i]])
    }
  }else if(ncol(data)==5){
    for(i in 1:length(un.ids)){
      mats[,,,,,i]<-cellmats[[i]]
      group[i]<-unique(groups[ids==un.ids[i]])
    }
  }else if(ncol(data)==6){
    for(i in 1:length(un.ids)){
      mats[,,,,,,i]<-cellmats[[i]]
      group[i]<-unique(groups[ids==un.ids[i]])
    }
  }

  return(list(mats=mats,group=group,id=un.ids,eval=eval.points))
}
