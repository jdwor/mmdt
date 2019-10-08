#' @title Get Multi-Modal Density Test Summary
#' @description This function creates a summary of the mmdt output.
#' @param mmdt.results an object resulting from the 'mmdt' command.
#'
#' @importFrom raster clump
#' @importFrom raster as.matrix
#' @importFrom igraph graph_from_edgelist
#' @importFrom igraph clusters
#' @importFrom stats dist
#' @return A description of overall group differences, and approximate regions that drive overall differences
#' @examples \dontrun{
#' mmdt.obj = get.mmdt.obj(masks = masks, modal1 = t1s, modal2 = flairs,
#'                         ids = ids, groups = groups)
#' results = mmdt(mmdt.obj)
#'
#' summarize.mmdt(results)}
#' @export

summarize.mmdt<-function(mmdt.results){
  by="pval.matrix.BY.corrected"%in%names(mmdt.results)
  maxt="pval.matrix.maxt.corrected"%in%names(mmdt.results)
  tfce="pval.matrix.tfce.corrected"%in%names(mmdt.results)
  groups=strsplit(mmdt.results$group.diff," minus ")[[1]]

  if(length(mmdt.results$evaluated.points)<3){
    if(by==T){
      if(sum(mmdt.results$pval.matrix.BY.corrected<.05)>0){
        clumps=raster::as.matrix(clump(raster(mmdt.results$pval.matrix.BY.corrected<.05),directions=4))
        tmat=mmdt.results$teststat.matrix
        dims.by=NULL
        signs=NULL
        ctab=sort(table(clumps),decreasing=T)
        ctab=ctab[ctab>1]
        for(i in as.numeric(names(ctab))){
          tdim=which(clumps==i,arr.ind=T)
          tvals=tmat[tdim]
          maxdim=which.max(abs(tvals))
          tdim=tdim[maxdim,]
          maxval=tvals[maxdim]
          tcoords=rep(NA,length(tdim))
          for(j in 1:length(tdim)){
            tcoords[j]=mmdt.results$evaluated.points[[j]][tdim[j]]
          }
          signs=c(signs,ifelse(maxval>0,"more","fewer"))
          dims.by=rbind(dims.by,tcoords)
        }
        cat("After BY correction, there are significant differences in subjects' densities by group.\n")
        cat("Specifically, relative to group ",groups[2],", group ",groups[1]," appears to have:\n",sep="")
        for(i in 1:nrow(dims.by)){
          cat("   - ",signs[i]," voxels near {",paste(round(dims.by[i,],3),collapse=", "),"}\n",sep="")
        }
        cat("\n")
      }else{
        cat("After BY correction, there are no significant differences in subjects' densities by group.\n")
        cat("\n")
      }
    }
    if(maxt==T){
      if(sum(mmdt.results$pval.matrix.maxt.corrected<.05)>0){
        clumps=raster::as.matrix(clump(raster(mmdt.results$pval.matrix.maxt.corrected<.05),directions=4))
        tmat=mmdt.results$teststat.matrix
        dims.by=NULL
        signs=NULL
        ctab=sort(table(clumps),decreasing=T)
        ctab=ctab[ctab>1]
        for(i in as.numeric(names(ctab))){
          tdim=which(clumps==i,arr.ind=T)
          tvals=tmat[tdim]
          maxdim=which.max(abs(tvals))
          tdim=tdim[maxdim,]
          maxval=tvals[maxdim]
          tcoords=rep(NA,length(tdim))
          for(j in 1:length(tdim)){
            tcoords[j]=mmdt.results$evaluated.points[[j]][tdim[j]]
          }
          signs=c(signs,ifelse(maxval>0,"more","fewer"))
          dims.by=rbind(dims.by,tcoords)
        }
        cat("After max-t correction, there are significant differences in subjects' densities by group.\n")
        cat("Specifically, relative to group ",groups[2],", group ",groups[1]," appears to have:\n",sep="")
        for(i in 1:nrow(dims.by)){
          cat("   - ",signs[i]," voxels near {",paste(round(dims.by[i,],3),collapse=", "),"}\n",sep="")
        }
        cat("\n")
      }else{
        cat("After max-t correction, there are no significant differences in subjects' densities by group.\n")
        cat("\n")
      }
    }
    if(tfce==T){
      if(sum(mmdt.results$pval.matrix.tfce.corrected<.05)>0){
        clumps=raster::as.matrix(clump(raster(mmdt.results$pval.matrix.tfce.corrected<.05),directions=4))
        tmat=mmdt.results$teststat.matrix
        dims.by=NULL
        signs=NULL
        ctab=sort(table(clumps),decreasing=T)
        ctab=ctab[ctab>1]
        for(i in as.numeric(names(ctab))){
          tdim=which(clumps==i,arr.ind=T)
          tvals=tmat[tdim]
          maxdim=which.max(abs(tvals))
          tdim=tdim[maxdim,]
          maxval=tvals[maxdim]
          tcoords=rep(NA,length(tdim))
          for(j in 1:length(tdim)){
            tcoords[j]=mmdt.results$evaluated.points[[j]][tdim[j]]
          }
          signs=c(signs,ifelse(maxval>0,"more","fewer"))
          dims.by=rbind(dims.by,tcoords)
        }
        cat("After TFCE correction, there are significant differences in subjects' densities by group.\n")
        cat("Specifically, relative to group ",groups[2],", group ",groups[1]," appears to have:\n",sep="")
        for(i in 1:nrow(dims.by)){
          cat("   - ",signs[i]," voxels near {",paste(round(dims.by[i,],3),collapse=", "),"}\n",sep="")
        }
        cat("\n")
      }else{
        cat("After TFCE correction, there are no significant differences in subjects' densities by group.\n")
        cat("\n")
      }
    }
  }else{
    if(by==T){
      if(sum(mmdt.results$pval.matrix.BY.corrected<.05)>0){
        coords=which(mmdt.results$pval.matrix.BY.corrected<.05,arr.ind=T)
        dmat=which(as.matrix(dist(coords))==1,arr.ind=T)
        g=graph_from_edgelist(dmat)
        clumps=clusters(g)$membership

        tmat=mmdt.results$teststat.matrix
        dims.by=NULL
        signs=NULL
        ctab=sort(table(clumps),decreasing=T)
        ctab=ctab[ctab>1]
        for(i in as.numeric(names(ctab))){
          tdim=coords[clumps==i,]
          tvals=tmat[tdim]
          maxdim=which.max(abs(tvals))
          tdim=tdim[maxdim,]
          maxval=tvals[maxdim]
          tcoords=rep(NA,length(tdim))
          for(j in 1:length(tdim)){
            tcoords[j]=mmdt.results$evaluated.points[[j]][tdim[j]]
          }
          signs=c(signs,ifelse(maxval>0,"more","fewer"))
          dims.by=rbind(dims.by,tcoords)
        }
        cat("After BY correction, there are significant differences in subjects' densities by group.\n")
        cat("Specifically, relative to group ",groups[2],", group ",groups[1]," appears to have:\n",sep="")
        for(i in 1:nrow(dims.by)){
          cat("   - ",signs[i]," voxels near {",paste(round(dims.by[i,],3),collapse=", "),"}\n",sep="")
        }
        cat("\n")
      }else{
        cat("After BY correction, there are no significant differences in subjects' densities by group.\n")
        cat("\n")
      }
    }
    if(maxt==T){
      if(sum(mmdt.results$pval.matrix.maxt.corrected<.05)>0){
        coords=which(mmdt.results$pval.matrix.maxt.corrected<.05,arr.ind=T)
        dmat=which(as.matrix(dist(coords))==1,arr.ind=T)
        g=graph_from_edgelist(dmat)
        clumps=clusters(g)$membership

        tmat=mmdt.results$teststat.matrix
        dims.by=NULL
        signs=NULL
        ctab=sort(table(clumps),decreasing=T)
        ctab=ctab[ctab>1]
        for(i in as.numeric(names(ctab))){
          tdim=coords[clumps==i,]
          tvals=tmat[tdim]
          maxdim=which.max(abs(tvals))
          tdim=tdim[maxdim,]
          maxval=tvals[maxdim]
          tcoords=rep(NA,length(tdim))
          for(j in 1:length(tdim)){
            tcoords[j]=mmdt.results$evaluated.points[[j]][tdim[j]]
          }
          signs=c(signs,ifelse(maxval>0,"more","fewer"))
          dims.by=rbind(dims.by,tcoords)
        }
        cat("After max-t correction, there are significant differences in subjects' densities by group.\n")
        cat("Specifically, relative to group ",groups[2],", group ",groups[1]," appears to have:\n",sep="")
        for(i in 1:nrow(dims.by)){
          cat("   - ",signs[i]," voxels near {",paste(round(dims.by[i,],3),collapse=", "),"}\n",sep="")
        }
        cat("\n")
      }else{
        cat("After max-t correction, there are no significant differences in subjects' densities by group.\n")
        cat("\n")
      }
    }
    if(tfce==T){
      if(sum(mmdt.results$pval.matrix.tfce.corrected<.05)>0){
        coords=which(mmdt.results$pval.matrix.tfce.corrected<.05,arr.ind=T)
        dmat=which(as.matrix(dist(coords))==1,arr.ind=T)
        g=graph_from_edgelist(dmat)
        clumps=clusters(g)$membership

        tmat=mmdt.results$teststat.matrix
        dims.by=NULL
        signs=NULL
        ctab=sort(table(clumps),decreasing=T)
        ctab=ctab[ctab>1]
        for(i in as.numeric(names(ctab))){
          tdim=coords[clumps==i,]
          tvals=tmat[tdim]
          maxdim=which.max(abs(tvals))
          tdim=tdim[maxdim,]
          maxval=tvals[maxdim]
          tcoords=rep(NA,length(tdim))
          for(j in 1:length(tdim)){
            tcoords[j]=mmdt.results$evaluated.points[[j]][tdim[j]]
          }
          signs=c(signs,ifelse(maxval>0,"more","fewer"))
          dims.by=rbind(dims.by,tcoords)
        }
        cat("After TFCE correction, there are significant differences in subjects' densities by group.\n")
        cat("Specifically, relative to group ",groups[2],", group ",groups[1]," appears to have:\n",sep="")
        for(i in 1:nrow(dims.by)){
          cat("   - ",signs[i]," voxels near {",paste(round(dims.by[i,],3),collapse=", "),"}\n",sep="")
        }
        cat("\n")
      }else{
        cat("After TFCE correction, there are no significant differences in subjects' densities by group.\n")
        cat("\n")
      }
    }
  }
}
