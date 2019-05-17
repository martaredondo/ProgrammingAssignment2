## Creation of 2 functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  z <- NULL
  
  setmat <- function(y){
  
        x <<- y
        z <- NULL
  
  }
  
  getmat <- function() x
  
  setinv <- function(w) z <<- w
  
  getinv <- function() z
  
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)

}


## Computes the inverse of the special matrix returned by the makeCacheMatrix fucntion.
## If the inverse has already been calculated and the matrix has not changed, 
## then the cacheSolve function retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  
  z <- x$getinv()
  
  if(!is.null(z)) {
    
    message("getting cached data")
    return(z)
    
  }
  
  data <- x$getmat()
  
  z <- solve(data, ...)
  
  x$setinv(z)
  
  z

}

# Example (1st generate matrix A in cache
#          2nd generate inverse of A)
# 
#   A <- matrix( c(1,2,3,4), nrow=2, ncol=2, byrow = TRUE)
#   matrixA <- makeCacheMatrix(A)
# 
#   cacheSolve(matrixA)
