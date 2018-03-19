## These two functions define the way to store a matrix M and its inverse I
## (under the hypothesis that M is invertible ) avoidinig to calulate the
## inverse if it has been already performed

## This function setups the object and defines the interface 
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialization of internal objects
  I <- NULL
  set <- function( matrix ) {
    x <<- matrix
    I <<- NULL
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    I <<- inverse
  }
  
  ## Get back the matrix
  get <- function(){
    x
  }
  
  ## Get back matrix inverse 
  getInverse <- function() {
    I
  }
  
  ## Internal methods list
  list(set = set, 
       setInverse = setInverse,
       get = get,
       getInverse = getInverse
  )
}


## This function calulates the inverse or returns it if already calculated
cacheSolve <- function(x, ...) {
  
  ## get the inverse of matrix input
  inv <- x$getInverse()
  
  ## if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache  
    message("Already calculated, getting cached data")
    return(inv)
  }
  else {
    message("It has never been calculated, doing it now")
  }
  
  ## else calculates the inverse and set it in the cache
  matr <- x$get()
  inv <- solve(matr)
  x$setInverse(inv)
  
  ## returns the inverse matrix 
  return(inv)
}


