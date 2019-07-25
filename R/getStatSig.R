#' @keywords internal
#' @importFrom stats pt

getStatSig=function(t,df){
  return(pt(abs(t),df,lower.tail=F)*2)
}
