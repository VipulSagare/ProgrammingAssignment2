## Following functions are written as part of assignment2 for
## R programming course.  There are two functions which, if used properly, will result in better usage of 
## computational resources.  As matrix inverse calculations are expensive, caching of matrix is acheived
## by these functions.   makeCacheMatrix makes given matrix cachable. cacheSolve returns invers of matrix
## if it is computed. Otherwise, it computes inverse using Solve function.
#
## makeCacheMatrix is used to get and set matrix and inverse of matrix
##
## Always use makeCacheMatrix to make cachable Matrix -:)
##
makeCacheMatrix <- function(mat = matrix()) {
  invMat <- NULL
  #Set Matrix
  set <- function(y) {
    mat <<- y
    invMat <<- NULL
  }
  #get Matrix
  get <- function() mat
  # set Inverse Matrix
  setinverse <- function(inverse) invMat <<- inverse
  # get Inverse Matrix
  getinverse <- function() invMat
  #
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##
##  cacheSolve function
##
##  returns inverse of the matrix either a cached value from earlier
## computation or it uses "Solve" method to find inverse of the matrix
cacheSolve <- function(x, ...) {
  ## **** Get Cached Inverse Matrix; if it is there *****
  invMat <- x$getinverse()
  ## if it is there return it
  if(!is.null(invMat)) {
    message("getting cached data.")
    return(invMat)
  }
  ## If cached matrix is not there, get matrix 
  data <- x$get()
  ## calculate Inverse
  invMat <- solve(data)
  ## set Inverse Matrix into the cache
  x$setinverse(invMat)
  ## return inverse Matrix to caller of the function
  invMat
}