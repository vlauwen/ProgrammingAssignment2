## Functions that cache the inverse of a matrix

## Creates matrix object, initialize the inverse property,
## set and get methods of the matrix and inverse and the returns
makeCacheMatrix <- function( m = matrix()) {
    i <- NULL  
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    get <- function() {
        m
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        i
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the matrix returned by the makeCacheMatrix function
## Return a matrix that is the inverse of 'x', returns the inverse if it's set
## Get the matrix and calculate the inverse. Set the inverse and return matrix
    
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
