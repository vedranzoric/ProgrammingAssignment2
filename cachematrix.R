##  Matrix inversion is usually a costly computation 
##  and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
 

## Returns list of 4 functions: get and set (get and set original matrix)
## getinverse and setinverse (gets and sets inverse matrix)

makeCacheMatrix <- function(x = matrix()) 
{
  inverse_matrix <- NULL
  set <- function(y) 
  {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(invrs) inverse_matrix <<- invrs
  getinverse <- function() inverse_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Based on the example, check if inverse exists, and if not calculate and cache inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) 
    return(inverse_matrix)
  
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
