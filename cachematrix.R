## Caching Matrices
## ================
## Creating a large matrix, especially if the operation is done repeatedly, e.g.
## in a loop, is a time-consuming operation.
## If the matrix has already been created previously, it is cheaper to store it
## in a cache, from where it can be directly retrieved, instead of recreating it
## at every loop.
## The same applies with other functions operating on large matrices.


## Create a matrix, or retrieve it from cache if already defined
## Parameters:
##   - x : the matrix object
## Returns:
##   - a list with environment information (???)

makeCacheMatrix <- function(x = matrix()) {
    ## By default, the inverse of the matrix is empty
    inv <- NULL

    ## Set the contents of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    ## Return the matrix object
    get <- function() x

    ## Set the inverse of the matrix
    setinverse <- function(solve) inv <<- solve

    ## Get the inverse of the matrix
    getinverse <- function() inv

    ## Return information about the object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Compute the inverse of a matrix, or retrieve it from cache if already computed
## Parameters:
##   - x : the matrix object
##   - ... : more arguments if needed
## Returns:
##   - a matrix object, the inverse matrix of x

cacheSolve <- function(x, ...) {
    ## Try and get the inverse of matrix x from cache
    inv <- x$getinverse()

    ## Do we have the inverse matrix already?
    if (!is.null(inv)) {
        ## yes we have. So return the value, and we are done
        message("getting cached data")
        return(inv)
    }

    ## Otherwise, inverse has to be computed. Let's go:

    ## Retrieve the matrix 
    data <- x$get()

    ## compute the inverse of the matrix (see R docs at
    ## http://cran.r-project.org/doc/manuals/R-intro.html#Linear-equations-and-inversion)
    inv <- solve(data, ...)

    ## store the value in the cache
    x$setinverse(inv)

    ## return computed inverse
    inv
}



## Testing
## m <- makeCacheMatrix(matrix(1:4, 2, 2))
## i <- makeCacheMatrix(cacheSolve(m))
## cacheSolve(i)
## => must return the same matrix as matrix(1:4, 2, 2)



