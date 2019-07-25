#' @title Get Multi-Modal Density Test Object
#' @description This function creates an mmdt object from lists of \code{nifti} filenames.
#' @param masks a vector of class \code{character} that gives .nii or .nii.gz filenames for subjects' masks.
#' Masks will demarcate which voxels will be included in the analysis, and should be coded by TRUE/FALSE or 1/0.
#' @param modal1 vector of class \code{character} that give .nii or .nii.gz filenames for a given imaging modality across subjects.
#' At least two modalities (modal1 and modal2) must be entered. Up to 6 can be included.
#' @param modal2 see 'modal1' description
#' @param modal3 see 'modal1' description
#' @param modal4 see 'modal1' description
#' @param modal5 see 'modal1' description
#' @param modal6 see 'modal1' description
#' @param ids a vector of subject ids. Must be the same length as the filenames in the 'modal#' vectors.
#' @param groups a vector of group membership. Must be two categories, and should be the same length as 'ids'.
#' @param parallel is a logical value that indicates whether the user's computer
#' is Linux or Unix (i.e. macOS), and should run the code in parallel.
#' @param cores if parallel = TRUE, cores is an integer value that indicates how many cores
#' the function should be run on.
#' @param pb is a logical value that indicates whether or not a progress bar will be shown during analysis.
#'
#' @importFrom pbapply pblapply
#' @importFrom pbmcapply pbmclapply
#' @importFrom parallel mclapply
#' @return An mmdt object to be used in the 'mmdt' function.
#' @examples \dontrun{
#' masks = c("mask01.nii", "mask02.nii", "mask03.nii", "mask04.nii")
#' t1s = c("t101.nii", "t102.nii", "t103.nii", "t104.nii")
#' flairs = c("flair01.nii", "flair02.nii", "flair03.nii", "flair04.nii")
#' ids = c(1, 2, 3, 4)
#' groups = c(1, 1, 2, 2)
#'
#' mmdt.obj = get.mmdt.obj(masks = masks, modal1 = t1s, modal2 = flairs,
#'                         ids = ids, groups = groups)}
#' @export

get.mmdt.obj<-function(masks,modal1,modal2,modal3=NULL,
                       modal4=NULL,modal5=NULL,modal6=NULL,
                       ids,groups,parallel=TRUE,cores=2,pb=TRUE){
  modals=list(modal1,modal2,modal3,modal4,modal5,modal6)
  non.nulls=which(unlist(lapply(modals,is.null))==F)
  modals=modals[non.nulls]

  if(length(unique(groups))!=2){
    stop("'groups' must have two unique categories")
  }

  lens=c(length(masks),lengths(modals),length(ids),length(groups))
  if(length(unique(lens))>1){
    stop("'masks', 'modal#', 'ids', and 'groups' must all include one entry per subject")
  }
  len=unique(lens)

  if(parallel==T){
    if(pb==T){
      data=pbmclapply(1:len,readSubject,masks=masks,modals=modals,
                      ids=ids,groups=groups,cores=cores)
      data=do.call(rbind,data)
      mmdt.obj=list(ids=data[,1],groups=data[,2],modals=data[,3:ncol(data)])
    }else{
      data=mclapply(1:len,readSubject,masks=masks,modals=modals,
                    ids=ids,groups=groups,cores=cores)
      data=do.call(rbind,data)
      mmdt.obj=list(ids=data[,1],groups=data[,2],modals=data[,3:ncol(data)])
    }
  }else{
    if(pb==T){
      data=pblapply(1:len,readSubject,masks=masks,modals=modals,
                    ids=ids,groups=groups)
      data=do.call(rbind,data)
      mmdt.obj=list(ids=data[,1],groups=data[,2],modals=data[,3:ncol(data)])
    }else{
      data=lapply(1:len,readSubject,masks=masks,modals=modals,
                  ids=ids,groups=groups)
      data=do.call(rbind,data)
      mmdt.obj=list(ids=data[,1],groups=data[,2],modals=data[,3:ncol(data)])
    }
  }
}




