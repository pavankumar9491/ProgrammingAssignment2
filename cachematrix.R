
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


## The function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of matrix
#get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y){
         x<<-y
         inverseMatrix<<-NULL
  }
  get<- function() x
  setInverse <- function(invers) inverseMatrix<<- invers
  getInverse <- function() inverseMatrix
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)

}


## The following function calculates and returns the inverse of the matrix.
#However, it first checks to see if the inverse of the matrix has already been calculated.
#If so, it gets the inverse matrix from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix 
#in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  data<-x$get()
  inverseMatrix<-solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
