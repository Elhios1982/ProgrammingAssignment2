## 'makeCacheMatrix' exposes accessor methods for caching the argument 'x'
## 'cacheSolve' makes use of accessor methods of 'makeCacheMatrix' function
## and returns the inversed matrix.

## makeCacheMatrix function enables accessors methods for 'x' variable
## exposing a getter and setter function. The 'set' function uses
## '<<-' operator to assign 'x' to a different environment. The 'get' 
## function returns the value of 'x'. The function 'getsolve' returns 
## the value of variable 'm'. The function 'setsolve' uses '<<-'
## operator to assign 'x' to a different environment. 

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


## 'cacheSolve' function makes use of 'getsolve', 'setsolve' and 'set'
## functions of makeCacheMatrix to get and set the cache for variable 'm'.
## It's also in charge of implementing the function 'solve' for reversing the matrix
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
