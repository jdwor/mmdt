#' @keywords internal

getPermSig=function(x,vec){
  return(sum(abs(vec)>=abs(x))/length(vec))
}
