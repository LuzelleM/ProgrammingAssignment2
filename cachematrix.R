## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This special function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
## get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
## can cache its own object. 

makeCacheMatrix <- function(x=matrix())
{
  inverseM <- NULL
  set <- function(y)
  {
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)
    inverseM <<- inverse
  getinverse <- function() inverseM
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The special function cacheSolve takes the output of the makeCacheMatrix(matrix) as an 
## input and checks whether an inverse matrix of the former exists or not.
## If inverse matrix of makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
## and sets  matrix by using the solve function.

cacheSolve <- function(x,...)
{
  inverseM <- x$getinverse()
  if(!is.null(inverseM))
  {
    message("getting cached inverse data")
    return(inverseM)
  }
  data <- x$get()
  inverseM <- solve(data, ...)
  x$setinverse(inverseM)
  inverseM
}
