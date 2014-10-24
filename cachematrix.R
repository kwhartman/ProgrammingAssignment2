## The functions below compute and cache the inverse of a matrix.
## The first time through, the inverse of the matrix is computed
## and cached.  All subsequent times, the matrix is pulled from cache.

## This function defines four set/get functions which:
## set and get the input square matrix
## set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL # set inverse to NULL as placeholder
  
  # set is a function that sets the matrix x
  # x is the matrix of data for which the
  # inverse is to be computed; x is set to Y
  # Inv is set to NULL
  set <- function(Y) {
    x <<- Y
    Inv <<- NULL
  }
  
  # get is a function that returns the matrix x
  get <- function() x
  
  # setInv is a function that sets Inv to Jnv
  setInv <- function(Jnv) Inv <<- Jnv
  
  # getInv returns the Inv
  getInv <- function() Inv
  
  # returns the list of the four defined functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
# This function takes in a vector of set/get functions
# and computes the inverse of a square matrix unless
# it has already been computed, in which case, the inverse
# is pulled from the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of data in 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached inverse matrix")
    return(Inv)
  }
  data <- x$get()          # get the input matrix
  Invr <- solve(data,...)  # compute the inverse
  x$setInv(Invr)           # set the inverse
  Inv <- x$getInv()        # get the inverse
  Inv
}
