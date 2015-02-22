## These two functions allow for calculating an inverse of an invertible matrix.
## As the calculation of an inverse of a matrix is time-consuming, caching is 
## leveraged in order to save processing time if calculated result is stored in
## the cache.

## This function takes an invertible matrix and returns a 2x2 matrix
## that contains 4 functions for
## 1. storing given invertible matrix in cache (using assignment operator '<<-')
## 2. getting invertible matrix from cache
## 3. storing given inverse of an invertible matrix
## 4. getting inverse of an invertible matrix from cache
makeCacheMatrix <- function(x = matrix()) {
        ## Check to see if the given matrix is invertible
        if (ncol(x) != nrow(x)) {
                message("Error: The given matrix is NOT invertible. Aborted.")
                return(NULL)
        }
        inversex <- NULL
        set <- function(y) {
                x <<- y
                inversex <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inversex <<- inverse
        getinverse <- function() inversex
        matrix(list(set = set,
                    get = get,
                    setinverse = setinverse,
                    getinverse = getinverse),
               nrow=2, ncol=2)
}

## This function takes the matrix that is returned by function makeCacheMatrix.
## It returns the inverse of an invertible matrix by either:
## - doing the calculation by itself, or
## - retrieving the cached calculated result (from function makeCacheMatrix),
## by calling method getinverse, the 4th element of the given matrix.
cacheSolve <- function(x, ...) {
        ## Check to see if the given matrix is null and abort if that's the case
        if (is.null(x)) {
                message("Error: The given matrix is null. Aborted.")
                return(NULL)
        }
        ## Retrieve cached inverse of x
        inversex <- x[[4]]()
        ## Check to see cached inverse of x is not null
        if(!is.null(inversex)) {
                ## Return cached inverse of x
                message("getting cached inverse of x")
                return(inversex)
        }
        data <- x[[2]]()
        ## If given matrix x is NOT equal to cached matrix x,
        ## cache x and calculated inverse of x
        message("calculating inverse of x")
        inversex <- solve(data)
        x[[3]](inversex)
        ## Return newly calculated inverse of x
        inversex
}