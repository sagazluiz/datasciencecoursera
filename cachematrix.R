## Functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {

	## initialize the inverse
    i <- NULL

    ## set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## get the inverse of the matrix
    getInverse <- function() {
        ## return the inverse
        i
    }

     ## return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the special matrix "makeCacheMatrix".
## the "cachesolve" should retrieve the inverse from the cache,
## if the inverse has already been calculated.

cacheSolve <- function(x, ...) {

    ## return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## get the matrix from our object
    data <- x$get()

    ## calc the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## set the inverse to the object
    x$setInverse(m)

    ## return the matrix
    m
}
