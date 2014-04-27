## 2 functions that make it possible to cache the inverse of a matrix 
## and compute it only if it has not been computed before


## Creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # sets the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # gets the value of the matrix
    get <- function() {
        x
    }
    
    # sets the value of the inverted matrix
    setinv <- function(inv) {
        m <<- inv
    }
    
    # gets the value of the inverted matrix
    getinv <- function() {
        m
    }
    
    # returns a list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Functions that checks if the inverted matrix has already been computed
## and if so, gets the value; if not: computes the inverse of the special matrix

cacheSolve <- function(x, ...) {
    ## Checks if the inverted matrix has already been computed and cached
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If not, returns a matrix that is the inverse of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

