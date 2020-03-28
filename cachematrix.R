## Put comments here that give an overall description of what your
## functions do
# The functions permit to cache the inverse value, here an usage example:
#
# > m <-matrix(c(1,3,9,4,5,6,7,8,2),3,3)
# > source("cachematrix.R")
# > cam<-makeCacheMatrix(m) 
# > cacheSolve(cam)              #first call will do the real job
# > cacheSolve(cam)              #second call will retrieve the cached inverse

## Write a short comment describing this function
#  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mat <- x                #I prefer to not use x directly but give an explicit name
  inv <- NULL
  getmatrix<-function()   {mat}
  setmatrix<-function(y)  {mat <<- y ; inv <<- NULL}
  getinverse<-function()  {inv}
  setinverse<-function(y) {inv <<- y}
  list(getmatrix=getmatrix,setmatrix=setmatrix,getinverse=getinverse,setinverse=setinverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
# using cached value if available
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinverse()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  xmat <- x$getmatrix()
  xinv <- solve(xmat,...)
  x$setinverse(xinv)
  xinv
}
