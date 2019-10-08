#' @keywords internal

matchVox=function(x,mat,evals,grid,num){
  if(num==2){
    w1=which.min(abs(mat[x,1]-evals[,1]))
    w2=which.min(abs(mat[x,2]-evals[,2]))
    return(grid[w1,w2])
  }else if(num==3){
    w1=which.min(abs(mat[x,1]-evals[,1]))
    w2=which.min(abs(mat[x,2]-evals[,2]))
    w3=which.min(abs(mat[x,3]-evals[,3]))
    return(grid[w1,w2,w3])
  }else if(num==4){
    w1=which.min(abs(mat[x,1]-evals[,1]))
    w2=which.min(abs(mat[x,2]-evals[,2]))
    w3=which.min(abs(mat[x,3]-evals[,3]))
    w4=which.min(abs(mat[x,4]-evals[,4]))
    return(grid[w1,w2,w3,w4])
  }else if(num==5){
    w1=which.min(abs(mat[x,1]-evals[,1]))
    w2=which.min(abs(mat[x,2]-evals[,2]))
    w3=which.min(abs(mat[x,3]-evals[,3]))
    w4=which.min(abs(mat[x,4]-evals[,4]))
    w5=which.min(abs(mat[x,5]-evals[,5]))
    return(grid[w1,w2,w3,w4,w5])
  }else if(num==6){
    w1=which.min(abs(mat[x,1]-evals[,1]))
    w2=which.min(abs(mat[x,2]-evals[,2]))
    w3=which.min(abs(mat[x,3]-evals[,3]))
    w4=which.min(abs(mat[x,4]-evals[,4]))
    w5=which.min(abs(mat[x,5]-evals[,5]))
    w6=which.min(abs(mat[x,6]-evals[,6]))
    return(grid[w1,w2,w3,w4,w5,w6])
  }
}
