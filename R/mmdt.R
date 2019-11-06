#' @title Multi-Modal Density Test
#' @description This function runs the multi-modal density test (mmdt) using an mmdt object obtained from get.mmdt.obj
#' @param mmdt.obj an mmdt object obtained using the get.mmdt.obj function.
#' @param mins a vector giving the lower intensity bounds for each modality.
#' If NULL, lower bounds will be set to the minimum observed value for each modality.
#' Length of this vector should be equal to the number of modalities being analyzed.
#' @param maxs a vector giving the upper intensity bounds for each modality.
#' If NULL, upper bounds will be set to the maximum observed value for each modality.
#' Length of this vector should be equal to the number of modalities being analyzed.
#' @param gridsize is a vector giving the number of points along each dimension at which the densities should be evaluated and tested.
#' If NULL, this value defaults to 151x151 for two modalities, 51x51x51 for three, and 21x21x21x21 for four.
#' Must be specified manually when analyzing 4-6 modalities.
#' @param H is the bandwidth matrix used for kernel density estimation. If NULL, a plug-in bandwidth estimator is used.
#' @param mc.adjust is a character vector defining the multiple comparison adjustments to use.
#' Default is "BH", which controls FDR using the Benjamini-Hochberg procedure.
#' The additional options are: "BY", which controls FDR using the Benjamini-Yekutieli procedure,
#' "maxt", which controls FWER using max-t correction,
#' and "tfce", which controls FWER using threshold-free cluster enhancement.
#' Both of the latter options use permutation to determine significance.
#' @param nperm is an integer value that gives the number of permutations desired.
#' @param parallel is a logical value that indicates whether the user's computer
#' is Linux or Unix (i.e. macOS), and should run the code in parallel.
#' @param cores if parallel = TRUE, cores is an integer value that indicates how many cores
#' the function should be run on.
#' @param pb is a logical value that indicates whether or not a progress bar will be shown during analysis.
#'
#' @importFrom stats p.adjust
#' @return A list containing subject.densities (kernel density estimates for each subject),
#' teststat.matrix (a matrix giving test statistics at each evaluated point),
#' pval.matrix.uncorrected (a matrix giving uncorrected p-values at each evaluated point),
#' pval.matrix.perm (a matrix giving permutation-corrected p-values at each evaluated point),
#' evaluated.points (a matrix giving the intensity values at which the test was evaluated for each modality).
#' @examples \dontrun{
#' mmdt.obj = get.mmdt.obj(masks = masks, modal1 = t1s, modal2 = flairs,
#'                         ids = ids, groups = groups)
#' results = mmdt(mmdt.obj)
#'
#' eval = results$evaluated.points
#' t.mat = results$teststat.matrix
#' p.mat = results$pval.matrix.perm}
#' @export

mmdt<-function(mmdt.obj,mins=NULL,maxs=NULL,
               gridsize=NULL,H=NULL,mc.adjust="BH",
               nperm=500,parallel=TRUE,cores=2,pb=TRUE){

  ids=mmdt.obj$ids
  groups=mmdt.obj$groups
  data=mmdt.obj$modals

  if(!("BH"%in%mc.adjust | "BY"%in%mc.adjust | "maxt"%in%mc.adjust | "tfce"%in%mc.adjust)){
    stop("'mc.adjust' must contain either 'BH', 'BY', 'maxt', or 'tfce'")
  }

  if(ncol(data)>2 & identical(mc.adjust,"tfce")){
    stop("TFCE is not supported for >2 modalities. Please use 'BY' or 'maxt'")
  }else if(ncol(data)>2 & "tfce"%in%mc.adjust){
    cat("TFCE is not supported for >2 modalities.\n")
    cat("The function will move forward with your other selections.\n")
    mc.adjust=mc.adjust[mc.adjust!="tfce"]
  }

  if(is.null(gridsize)){
    if(ncol(data)==2){
      gridsize=c(151,151)
      cat("gridsize not specified, defaulting to 151x151\n")
    }else if(ncol(data)==3){
      gridsize=c(51,51,51)
      cat("gridsize not specified, defaulting to 51x51x51\n")
    }else if(ncol(data)==4){
      gridsize=c(21,21,21,21)
      cat("gridsize not specified, defaulting to 21x21x21x21\n")
    }else{
      stop("gridsize must be specified with >4 modalities")
    }
  }

  cat("Estimating subject densities\n")
  mats=getAllDens(data,ids,groups,gridsize,mins,maxs,H,parallel,pb,cores)
  eval.points=mats$eval
  group=mats$group
  id=mats$id
  mats=mats$mats
  g.cats=unique(group)
  g.diff=paste0(g.cats[1]," minus ",g.cats[2])

  cat("Estimating covariate effects\n")
  resmats=getStatMat(mats,group,gridsize,parallel,pb,cores)
  tmat=resmats$ts
  pmat.ttest=resmats$ps
  rm(resmats)

  cat("Getting corrected p-values\n")

  output=list(subject.densities=mats,
              teststat.matrix=tmat,
              pval.matrix.uncorrected=pmat.ttest)

  if("BY"%in%mc.adjust){
    pmat.by=pmat.ttest
    pvals=pmat.by[!is.na(pmat.by)]
    pvals=p.adjust(pvals,method="BY")
    pmat.by[!is.na(pmat.by)]=pvals
    output[["pval.matrix.BY.corrected"]]=pmat.by
  }
  if("BH"%in%mc.adjust){
    pmat.bh=pmat.ttest
    pvals=pmat.bh[!is.na(pmat.bh)]
    pvals=p.adjust(pvals,method="BH")
    pmat.bh[!is.na(pmat.bh)]=pvals
    output[["pval.matrix.BH.corrected"]]=pmat.bh
  }
  if("maxt"%in%mc.adjust & "tfce"%in%mc.adjust){
    nullmats=getNullDist(method="both",nperm,mats,group,
                         gridsize,parallel,pb,cores)

    pmat.maxt=tmat
    pvals=tmat[!is.na(tmat)]
    pvals=as.vector(unlist(lapply(pvals,getPermSig,vec=nullmats[,1])))
    pmat.maxt[!is.na(tmat)]<-pvals
    output[["pval.matrix.maxt.corrected"]]=pmat.maxt

    pmat.tfce=getTFCE(tmat,parallel=parallel,cores=cores)
    pvals=pmat.tfce[!is.na(pmat.tfce)]
    pvals=as.vector(unlist(lapply(pvals,getPermSig,vec=nullmats[,2])))
    pmat.tfce[!is.na(pmat.tfce)]<-pvals
    output[["pval.matrix.tfce.corrected"]]=pmat.tfce
  }else if("maxt"%in%mc.adjust & !("tfce"%in%mc.adjust)){
    nullmats=getNullDist(method="maxt",nperm,mats,group,
                         gridsize,parallel,pb,cores)
    pmat.maxt=tmat
    pvals=tmat[!is.na(tmat)]
    pvals=as.vector(unlist(lapply(pvals,getPermSig,vec=nullmats)))
    pmat.maxt[!is.na(tmat)]<-pvals
    output[["pval.matrix.maxt.corrected"]]=pmat.maxt
  }else if(!("maxt"%in%mc.adjust) & "tfce"%in%mc.adjust){
    nullmats=getNullDist(method="tfce",nperm,mats,group,
                         gridsize,parallel,pb,cores)
    pmat.tfce=getTFCE(tmat,parallel=parallel,cores=cores)
    pvals=pmat.tfce[!is.na(pmat.tfce)]
    pvals=as.vector(unlist(lapply(pvals,getPermSig,vec=nullmats)))
    pmat.tfce[!is.na(pmat.tfce)]<-pvals
    output[["pval.matrix.tfce.corrected"]]=pmat.tfce
  }

  output[["evaluated.points"]]=eval.points
  output[["group.diff"]]=g.diff
  return(output)
}





