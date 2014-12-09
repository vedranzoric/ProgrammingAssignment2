##  Matrix inversion is usually a costly computation 
##  and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
##  In this program we will create a solution to that problem.

## function makeCacheMatrix   
## input: inversible matrix (we will assume imput is of the correct format)
## output: list of 4 functions
##    get: returns cached original matrix
##    set: saves original matrix into cache
##    getInverse: returns cached inverse
##    setInverse: saves inverse matrix into cache
makeCacheMatrix <- function(x = matrix()) 
{
  inverse_matrix <- NULL

  set <- function(y) 
  {
      x <<- y
      # after every change of original matrix we need to delete cached inverse
      inverse_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(invrs) inverse_matrix <<- invrs
  getInverse <- function() inverse_matrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will be used for getting inverse
## We need to check if there exists a cached version of inverse matrix, and calculate the inverse only when there is not.
## input: list that corresponds to output of function makeCacheMatrix
## output: matrix which is inverse of matrix x
cacheSolve <- function(x, ...) 
{
  inverse_matrix <- x$getInverse()
  if(!is.null(inverse_matrix)) 
      return(inverse_matrix)
  
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setInverse(inverse_matrix)
  inverse_matrix
}
