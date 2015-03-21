##Description of "makeCacheMatrix" function


##This function takes a matrix and defines and sets or gets the invers of the matrix, depending
##on if there is an inverse defined in the parent environemtn.If not, the function
##sets a function to the global environment.
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


##Description of "cacheSolve" function:

##This function takes a matrix and determines if an inverse has been calculated
##and assigned to the parent environment.If an inverse has not been cached to the parent environment
##then cacheSolve, the function calculates the function and 
##saves it as a variable in the parent environment.

cacheSolve <- function(x, ...){

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