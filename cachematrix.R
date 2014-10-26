## Function: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
## WARNING!! accepts only a vector of length = 4 numeric() 
makeMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- matrix(y,nrow=2)
            m <<- NULL
      }
      get <- function() {
            matrix(x,nrow=2) ##returns a matrix (square 2x2) from vector  'x'
            ## very limited: 
            ## should check possibility of making a square matrix from length of vector
            ## passed in 'x'
      }
      set_inverse <- function(inverse){
            m <<- inverse  ## stores the inverse matrix in 'm'
      }
      get_inverse <- function(){
            m  ## retrieves the inverse matrix stored in private variable 'm'
      }
      
      ## makes the private function names public (ie: available from global environment)
      list(set = set, get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}

## Function: cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## WARNING!! matrix MUST be 2x2
      m <- x$get_inverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$set_inverse(m)
      m
}