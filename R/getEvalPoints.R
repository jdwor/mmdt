#' @keywords internal

getEvalPoints=function(mins,maxs,gridsize){
  ranges=maxs-mins
  eval.points=list()

  for(i in 1:length(ranges)){
    eval.points[[i]]=seq(mins[i],maxs[i],ranges[i]/(gridsize[i]-1))
  }

  return(eval.points)
}
