#' @title Visualize MMDT Results in a Subject's Brain Space
#' @description This function creates an mmdt object from lists of \code{nifti} filenames.
#' @param mmdt.results an object resulting from the 'mmdt' command.
#' @param type type of image to be produced. Can be "t-statistic" or "p-value". Default is "p-value".
#' @param mask a string that gives a .nii or .nii.gz filename for the subject's mask.
#' Mask will demarcate which voxels will be included, should be coded by TRUE/FALSE or 1/0,
#' and should be the same as the masks used to conduct the mmdt analyses.
#' @param modal1 a string that gives a .nii or .nii.gz filename for a subject's given imaging modality.
#' At least two modalities (modal1 and modal2) must be entered. Up to 6 can be included.
#' The same modalities used in the mmdt analyses should be entered here, in the same order.
#' @param modal2 see 'modal1' description
#' @param modal3 see 'modal1' description
#' @param modal4 see 'modal1' description
#' @param modal5 see 'modal1' description
#' @param modal6 see 'modal1' description
#'
#' @importFrom RNifti readNifti
#' @return A nifti image in a subject's brain space, in which voxel intensities represent
#' either the t-statistic or p-value of the group difference test at the given
#' voxels' locations within the density space. Importantly, statistics are not to be
#' interpreted in the brain space; these values are only for visualization of where specific
#' voxel intensity profiles fall within subjects' images.
#' @examples \dontrun{
#' masks = c("mask01.nii", "mask02.nii", "mask03.nii", "mask04.nii")
#' t1s = c("t101.nii", "t102.nii", "t103.nii", "t104.nii")
#' flairs = c("flair01.nii", "flair02.nii", "flair03.nii", "flair04.nii")
#' ids = c(1, 2, 3, 4)
#' groups = c(1, 1, 2, 2)
#'
#' mmdt.obj = get.mmdt.obj(masks = masks, modal1 = t1s, modal2 = flairs,
#'                         ids = ids, groups = groups)
#' mmdt.results = mmdt(mmdt.obj)
#'
#' tstat.mask.s1 = mmdt.to.brain(mmdt.results, type = "t-statistic",
#'                               mask = "mask01.nii", modal1 = "t101.nii",
#'                               modal2 = "flair01.nii")
#' pval.mask.s1 = mmdt.to.brain(mmdt.results, type = "p-value",
#'                              mask = "mask01.nii", modal1 = "t101.nii",
#'                              modal2 = "flair01.nii")
#' writeNifti(tstat.mask.s1, file="mmdt.tstat.01.nii.gz")
#' writeNifti(pval.mask.s1, file="mmdt.pval.01.nii.gz")}
#' @export

mmdt.to.brain<-function(mmdt.results,type="t-statistic",mask,modal1,modal2,
                        modal3=NULL,modal4=NULL,modal5=NULL,modal6=NULL){

  by="pval.matrix.BY.corrected"%in%names(mmdt.results)
  maxt="pval.matrix.maxt.corrected"%in%names(mmdt.results)
  tfce="pval.matrix.tfce.corrected"%in%names(mmdt.results)
  evals=do.call(cbind,mmdt.results$evaluated.points)
  mask=RNifti::readNifti(mask)

  modals=c(modal1,modal2,modal3,modal4,modal5,modal6)
  non.nulls=which(!is.null(modals))
  ims=modals[non.nulls]

  if(length(ims)!=length(evals)){
    stop("Must enter the same modalities that were used to create 'mmdt.results'")
  }

  mat=matrix(nrow=sum(mask==T),ncol=length(ims))
  for(i in 1:length(ims)){
    thisim=RNifti::readNifti(ims[i])
    thisim=thisim[mask==T]
    mat[,i]=thisim
  }

  if(type=="t-statistic"){
    mat=mmdt.results$teststat.matrix
  }else if(type=="p-value"){
    if(by==T){
      mat=mmdt.results$pval.matrix.BY.corrected
      mat[mmdt.results$pval.matrix.BY.corrected>.05]=0
      mat[mmdt.results$pval.matrix.BY.corrected<.05 &
            mmdt.results$teststat.matrix>0]=1
      mat[mmdt.results$pval.matrix.BY.corrected<.05 &
            mmdt.results$teststat.matrix<0]=-1
      mat[!(mat%in%c(1,-1))]=0
    }else if(maxt==T){
      mat=mmdt.results$pval.matrix.maxt.corrected
      mat[mmdt.results$pval.matrix.maxt.corrected>.05]=0
      mat[mmdt.results$pval.matrix.maxt.corrected<.05 &
            mmdt.results$teststat.matrix>0]=1
      mat[mmdt.results$pval.matrix.maxt.corrected<.05 &
            mmdt.results$teststat.matrix<0]=-1
      mat[!(mat%in%c(1,-1))]=0
    }else{
      mat=mmdt.results$pval.matrix.tfce.corrected
      mat[mmdt.results$pval.matrix.tfce.corrected>.05]=0
      mat[mmdt.results$pval.matrix.tfce.corrected<.05 &
            mmdt.results$teststat.matrix>0]=1
      mat[mmdt.results$pval.matrix.tfce.corrected<.05 &
            mmdt.results$teststat.matrix<0]=-1
      mat[!(mat%in%c(1,-1))]=0
    }
  }
  vals=lapply(1:nrow(mat),matchVox,evals=evals,
              grid=mat,num=ncol(mat))
  vals=as.vector(unlist(vals))

  outim=mask
  outim[mask==T]=vals
  return(outim)
}




