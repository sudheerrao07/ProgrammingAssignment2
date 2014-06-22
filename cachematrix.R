## This function is creates a matrix object that can cache its own inverse matrix

makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property

    i <- NULL

    ## creating a function to call matrix

    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method to get the matrix

    get <- function() {
    	## Return the matrix
    	m
    }

    ## Process of inversing the matrix

    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix

    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Create the inverse of the special matrix returned by "makeCacheMatrix" above.
## If the inverse has already been calculated (and the matrix has not changed,
## then "cachesolve" function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## Return the matrix that is the inverse of variable 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}