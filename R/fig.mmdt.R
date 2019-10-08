#' @title Plot Results of Multi-Modal Density Test
#' @description This function creates a summary of the mmdt output.
#' @param mmdt.results an object resulting from the 'mmdt' command.
#' @param type type of image to be produced. Can be "t-statistic" or "p-value". Default is "p-value".
#' @param coords a vector of length d [e.g., c(NA, NA, 3.25) for d=3] giving the coordinates at which the plane should be visualized.
#' Only necessary for plotting results that include three or more dimensions.
#' Entries should be "NA" for the two modalities to be plotted, and other entries should give the
#' value along the other dimensions at which the results should be plotted.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 scale_fill_gradient2
#' @return A raster image showing either the full t-statistic map or a map of regions at which significant differences were detected.
#' @examples \dontrun{
#' mmdt.obj = get.mmdt.obj(masks = masks, modal1 = t1s, modal2 = flairs,
#'                         ids = ids, groups = groups)
#' results = mmdt(mmdt.obj)
#'
#' fig.mmdt(results, type="p-value")}
#' @export

fig.mmdt<-function(mmdt.results, type="p-value", coords=c(NA,NA)){
  by="pval.matrix.BY.corrected"%in%names(mmdt.results)
  maxt="pval.matrix.maxt.corrected"%in%names(mmdt.results)
  tfce="pval.matrix.tfce.corrected"%in%names(mmdt.results)
  groups=strsplit(mmdt.results$group.diff," minus ")[[1]]
  evals=mmdt.results$evaluated.points

  if(type=="p-value"){
    if(length(evals)<3){
      if(by==T){
        mat=mmdt.results$pval.matrix.BY.corrected
        mat[mmdt.results$pval.matrix.BY.corrected>.05]=0
        mat[mmdt.results$pval.matrix.BY.corrected<.05 & mmdt.results$teststat.matrix>0]=1
        mat[mmdt.results$pval.matrix.BY.corrected<.05 & mmdt.results$teststat.matrix<0]=-1
        mat[!(mat%in%c(1,-1))]=0
        thistitle="Differences in group densities\nafter BY correction"
      }else if(maxt==T){
        mat=mmdt.results$pval.matrix.maxt.corrected
        mat[mmdt.results$pval.matrix.maxt.corrected>.05]=0
        mat[mmdt.results$pval.matrix.maxt.corrected<.05 & mmdt.results$teststat.matrix>0]=1
        mat[mmdt.results$pval.matrix.maxt.corrected<.05 & mmdt.results$teststat.matrix<0]=-1
        mat[!(mat%in%c(1,-1))]=0
        thistitle="Differences in group densities\nafter max-t correction"
      }else{
        mat=mmdt.results$pval.matrix.tfce.corrected
        mat[mmdt.results$pval.matrix.tfce.corrected>.05]=0
        mat[mmdt.results$pval.matrix.tfce.corrected<.05 & mmdt.results$teststat.matrix>0]=1
        mat[mmdt.results$pval.matrix.tfce.corrected<.05 & mmdt.results$teststat.matrix<0]=-1
        mat[!(mat%in%c(1,-1))]=0
        thistitle="Differences in group densities\nafter TFCE correction"
      }
      df=expand.grid(x=evals[[1]],y=evals[[2]])
      df$Color=as.factor(as.vector(mat))

      ggplot(df) + geom_raster(aes_string('x', 'y', fill = 'Color')) +
        scale_fill_manual(values=c("-1"="#B2182B",
                                   "0"="#FFFFFF",
                                   "1"="#2166AC"),
                          labels=c("-1"=paste0("More voxels\nin group ",groups[2]),
                                   "0"="No significant\ndifference",
                                   "1"=paste0("More voxels\nin group ",groups[1]))) +
        theme_bw() + xlab("Modality 1") + ylab("Modality 2") +
        theme(legend.position="right",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        guides(fill=guide_legend(
          keywidth=0.2, keyheight=0.5,
          default.unit="inch")) + ggtitle(thistitle)
    }else{
      if(length(coords)!=length(evals) | sum(is.na(coords))!=2){
        stop("'coords' must be the same length as the number of modalities analyzed,
        and must contain exactly two NA entries indicating which dimensions should be plotted.
        For example: c(NA, NA, 2.0, 1.5) will plot the results over all values of
        modalities 1 and 2, for points at which modalitiy 3 = 2.0 and modality 4 = 1.5")
      }
      if(by==T){
        mat=mmdt.results$pval.matrix.BY.corrected
        mat[mmdt.results$pval.matrix.BY.corrected>.05]=0
        mat[mmdt.results$pval.matrix.BY.corrected<.05 & mmdt.results$teststat.matrix>0]=1
        mat[mmdt.results$pval.matrix.BY.corrected<.05 & mmdt.results$teststat.matrix<0]=-1
        mat[!(mat%in%c(1,-1))]=0
        thistitle="Differences in group densities\nafter BY correction"
      }else if(maxt==T){
        mat=mmdt.results$pval.matrix.maxt.corrected
        mat[mmdt.results$pval.matrix.maxt.corrected>.05]=0
        mat[mmdt.results$pval.matrix.maxt.corrected<.05 & mmdt.results$teststat.matrix>0]=1
        mat[mmdt.results$pval.matrix.maxt.corrected<.05 & mmdt.results$teststat.matrix<0]=-1
        mat[!(mat%in%c(1,-1))]=0
        thistitle="Differences in group densities\nafter max-t correction"
      }else{
        mat=mmdt.results$pval.matrix.tfce.corrected
        mat[mmdt.results$pval.matrix.tfce.corrected>.05]=0
        mat[mmdt.results$pval.matrix.tfce.corrected<.05 & mmdt.results$teststat.matrix>0]=1
        mat[mmdt.results$pval.matrix.tfce.corrected<.05 & mmdt.results$teststat.matrix<0]=-1
        mat[!(mat%in%c(1,-1))]=0
        thistitle="Differences in group densities\nafter TFCE correction"
      }
      wmods=which(is.na(coords))
      nmods=which(!is.na(coords))
      tcoords=expand.grid(1:length(evals[[wmods[1]]]),
                          1:length(evals[[wmods[2]]]))
      for(i in nmods){
        twcoord=coords[i]
        coords[i]=which.min(abs(evals[[i]]-twcoord))
        tcoords=cbind(tcoords,coords[i])
      }
      ix=sort(c(wmods,nmods),decreasing=F,index.return=T)$ix
      tcoords=tcoords[,ix]
      mat=matrix(mat[as.matrix(tcoords)],ncol=length(evals[[wmods[1]]]),
                  nrow=length(evals[[wmods[2]]]))

      df=expand.grid(x=evals[[wmods[1]]],y=evals[[wmods[2]]])
      df$Color=factor(as.vector(mat),levels=c(-1,0,1))

      ggplot(df) + geom_raster(aes_string('x', 'y', fill = 'Color')) +
        scale_fill_manual(values=c("-1"="#B2182B",
                                   "0"="#FFFFFF",
                                   "1"="#2166AC"),
                          labels=c("-1"=paste0("More voxels\nin group ",groups[2]),
                                   "0"="No significant\ndifference",
                                   "1"=paste0("More voxels\nin group ",groups[1]))) +
        theme_bw() + xlab(paste0("Modality ",wmods[1])) +
        ylab(paste0("Modality ",wmods[2])) +
        theme(legend.position="right",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        guides(fill=guide_legend(
          keywidth=0.2, keyheight=0.5,
          default.unit="inch")) + ggtitle(thistitle)
    }
  }else if(type=="t-statistic"){
    if(length(evals)<3){
      mat=mmdt.results$teststat.matrix
      df=expand.grid(x=evals[[1]],y=evals[[2]])
      df$z=as.vector(mat)

      ggplot(df) + geom_raster(aes_string('x', 'y', fill = 'z')) +
        scale_fill_gradient2(low="#B2182B",mid="#FFFFFF",high="#2166AC",
                             midpoint=0, name="T-statistic") + theme_bw() +
        xlab("Modality 1") + ylab("Modality 2") +
        theme(legend.position="right",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        ggtitle("Test statistic map of group density differences")
    }else{
      if(length(coords)!=length(evals) | sum(is.na(coords))!=2){
        stop("'coords' must be the same length as the number of modalities analyzed,
        and must contain exactly two NA entries indicating which dimensions should be plotted.
        For example: c(NA, NA, 2.0, 1.5) will plot the results over all values of
        modalities 1 and 2, for points at which modalitiy 3 = 2.0 and modality 4 = 1.5")
      }
      mat=mmdt.results$teststat.matrix
      wmods=which(is.na(coords))
      nmods=which(!is.na(coords))
      tcoords=expand.grid(1:length(evals[[wmods[1]]]),
                          1:length(evals[[wmods[2]]]))
      for(i in nmods){
        twcoord=coords[i]
        coords[i]=which.min(abs(evals[[i]]-twcoord))
        tcoords=cbind(tcoords,coords[i])
      }
      ix=sort(c(wmods,nmods),decreasing=F,index.return=T)$ix
      tcoords=tcoords[,ix]
      mat=matrix(mat[as.matrix(tcoords)],ncol=length(evals[[wmods[1]]]),
                 nrow=length(evals[[wmods[2]]]))

      df=expand.grid(x=evals[[1]],y=evals[[2]])
      df$z=as.vector(mat)

      ggplot(df) + geom_raster(aes_string('x', 'y', fill = 'z')) +
        scale_fill_gradient2(low="#B2182B",mid="#FFFFFF",high="#2166AC",
                             midpoint=0, name="T-statistic") + theme_bw() +
        xlab("Modality 1") + ylab("Modality 2") +
        theme(legend.position="right",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        ggtitle("Test statistic map of group density differences")
    }
  }
}
