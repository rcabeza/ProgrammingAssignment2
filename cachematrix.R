## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function takes a matrix. And defines functions to sets the inverse of a matrix as a function
##that is accessible in the parent environment, or sets a function to the global environment.
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
##This function takes a matrix and determines if has an inverse assigned
##if does not then it calculates the inverse of the matrix

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- Solve(data, ...)
  x$setinv(m)
  m  
}