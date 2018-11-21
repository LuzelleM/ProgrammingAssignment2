## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This special function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
## get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
## can cache its own object. 

makeCacheMatrix <- function(x=matrix())
  #take the matrix as an input
{
  inverseM <- NULL
  set <- function(y)
    #set the value of the matrix
  {
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x #get the value of the matrix
  setinverse <- function(inverse) #set the value of the invertible matrix
    inverseM <<- inverse
  getinverse <- function() inverseM #get the value of the invertible matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The special function cacheSolve takes the output of the makeCacheMatrix(matrix) as an 
## input and checks whether an inverse matrix of the former exists or not.
## If inverse matrix of makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
## and sets  matrix by using the solve function.

cacheSolve <- function(x,...)
  #gets the value of the invertible matrix from the makeCacheMatrix function
{
  inverseM <- x$getinverse()
  if(!is.null(inverseM))
  {
    message("getting cached inverse data")
    return(inverseM)
  }
  data <- x$get() #get the original matrix data
  inverseM <- solve(data, ...) #use solve function to get the inverse of the matrix
  x$setinverse(inverseM) #set the inverse of the matrix
  inverseM #returns the inverse of the matrix
}
