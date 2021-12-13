makecacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse){inv<<-inverse}
  getInverse <- function(){inv}
  list(set=set, get=get,setinverse=setinverse,getInverse=getInverse)
}
cacheSolve <-function(x,...){
  inv <-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    
  }
  mat <-x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
  
}
x<-matrix(3:10,7,7)
Y<-makecacheMatrix(x)
Y$get()
Y$getInverse()
cacheSolve(Y)
Y<-makecacheMatrix(matrix(2:5,nrow=3,ncol=3))
Y$get()
Y$getInverse()
cacheSolve(Y)
Y$getInverse()
