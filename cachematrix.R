## This set of functions create a matrix that has the ability to cache the inverse (assuming one is possible).

##makeCacheMatrix takes a matrix as an argument and creates an object to allow for the caching of the inverse.
## NOTE:  The inverse in not calcultated in this function, but in the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Solve) m <<- Solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function allows the caller to access the value of the inverse of the matrix argument and will store that value
## in a variable for later use (caching) to give better performance in a program that might have to calculate the inverse
## multiple times.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
}
