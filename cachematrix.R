## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCachematrix <- function(x = matrix()) {  ## creating special function 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv  <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse   ## set inverse function
  getinv <- function() inv   ##get inverse function 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {                  ## check if inv is not null
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
