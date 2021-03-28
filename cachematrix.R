## Write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse. It doesn't return anything, it just caches a matrix inverse.
## It creates a vector X that it's a matrix with n1 being the inverse matrix. We use getters and setters to access and object and mutate it
## In this case, it's a matrix. Lastly, we create a list to access the functions by name.
## We use the <<- to assign the value on the right side of the operator to an object in the parent environment
## named by the object on the left side of the operator
## We're assigning the value m1 to the object X, which is in the parent environment. The same logic applies to n1 and null

makeCacheMatrix <- function(x = matrix()) {
  n1 <- NULL
  set <- function(m1) {
    x <<- m1
    n1 <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) n1 <<- solve
  getsolve <- function() n1
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If n1 is not null, then this function returns what it cached (n1)
## If n1 is null, then this function computes the inverse of the "matrix" in makeCacheMatrix and returns this matrix, n1.
cacheSolve <- function(x, ...) {
  n1 <- x$getsolve()
  if(!is.null(n1)) {
    message("getting cached data")
    return(n1)
  }
  data <- x$get()
  n1 <- solve(data, ...)
  x$setsolve(n1)
  n1
}