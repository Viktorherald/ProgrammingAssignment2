## Overall, this two set of function is used to calculate together the inverse
## of a matrix in an efficient way, to prevent repeated computations on same input.


## makeCacheMatrix is a function to initialize special vector, to store the list 
## of functions to do set the variables, perform inverse computations, and functions
## to retrieve the value. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv  <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This functions take the special vector created in makeCacheMatrix to and first
## check is there any value in 'inv' variable. If there is, direct return the values.

## Else, it will call the 'setinv' function to calculate the inverse of matrix, 
## store it to 'inv' variable, then finally return the computed value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
