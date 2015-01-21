## An example of caching potentially time-consuming computations:
## Taking the mean of a numeric matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This is actually a list containing a funtion to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
             x <<- y
             m <<- NULL
         }
       get <- function() x
       setsolve <- function(solve) m <<- solve
       getsolve <- function() m
       list(set = set, get = get,
                      setsolve = setsolve,
                      getsolve = getsolve)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       m <- x$getsolve()
       if(!is.null(m)) {
             message("getting cached data")
             return(m)
         }
       data <- x$get()
       m <- solve(data, ...)
       x$setsolve(m)
       m
}
