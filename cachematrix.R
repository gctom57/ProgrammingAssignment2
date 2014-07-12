
## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {
  
  ## Initialize the inverse var
  inverse <- NULL
  
  ## Set the new matrix
  set <- function(newMat) {
    mat <<- newMat
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() mat
  
  ## Set the inverse of the matrix
  setInverse <- function(newInverse) inverse <<- newInverse
  
  ## Get the inverse of the matrix
  getInverse <- function() inverse
  
  ## Return the list of Method
  list(set = set, get = get,
       setInvers = setInverse,
       getInvers = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cachedMat, ...) {
  ## get the Inverse of the matrix inside our Cached Matrix
  inverse <- cachedMat$getInverse()
  
  ## if the invers is not null return the cached data
  if( !is.null(inverse) ) {
    return(inverse)
  }
  
  ## Otherwise get the matrix inside our Cached Matrix
  matrix <- cachedMat$get()
  
  ## Calculate the inverse using the solve function
  inverse <- solve(matrix) %*% matrix
  
  ## Set the inverse matrix in our Cached Matrix
  cachedMat$setInverse(invers)
  
  ## Return the inverse matrix
  inverse
}
