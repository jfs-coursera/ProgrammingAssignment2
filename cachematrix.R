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
##   - mymatrix : the matrix object
## Returns:
##   - a list with environment information (???)
## Example usage:
##   m <- makeCacheMatrix(matrix(1:4, 2, 2))

makeCacheMatrix <- function(mymatrix = matrix()) {
    ## By default, the inverse of the matrix is empty
    inv <- NULL

    ## Set the contents of the matrix
    set <- function(newmatrix) {
        ## Reset the matrix only if it has changed
        if (!isTRUE(all.equal(mymatrix, newmatrix))) {
            mymatrix <<- newmatrix
            inv <<- NULL
        }
    }

    ## Return the matrix object
    get <- function() mymatrix

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
##   - mymat : the matrix object
##   - ... : more arguments if needed
## Returns:
##   - a matrix object, the inverse matrix of mymat
## Example usage:
##   m <- makeCacheMatrix(matrix(1:4, 2, 2))
##   i <- cacheSolve(m)

cacheSolve <- function(mymat, ...) {
    ## Try and get the inverse of matrix from cache
    inv <- mymat$getinverse()

    ## Do we have the inverse matrix already?
    if (!is.null(inv)) {
        ## yes we have. So return the value, and we are done
        message("getting cached data")
        return(inv)
    }

    ## Otherwise, inverse has to be computed. Let's go:

    ## Retrieve the matrix 
    data <- mymat$get()

    ## Compute the inverse of the matrix
    inv <- solve(data, ...)

    ## Store the value in the cache
    mymat$setinverse(inv)

    ## Return computed inverse
    inv
}




## Testing
## This is not part of the assignment, but it helps...
test <- function() {
    ## some matrices (names in uppercase, so they are easier to spot)
    MATRIX1 <- matrix(1:4, 2, 2)
    MATRIX2 <- matrix(1:4, 2, 2)  ## the same as mat1
    MATRIX3 <- matrix(c(1, 5, 2, 8), 2, 2)

    print("-- create a matrix")
    m <- makeCacheMatrix(MATRIX1)

    print("-- get the matrix")
    z <- m$get()
    print(z)

    print("-- get the inverse (shoud be NULL, because not yet computed)")
    z <- m$getinverse()
    print(z)

    print("-- compute the inverse (we see the return value here)")
    z <- cacheSolve(m)
    print(z)

    print("-- get the inverse (should not be NULL now)")
    z <- m$getinverse()
    print(z)

    print("-- multiply matrix by its inverse (should obtain identity matrix)")
    z <- m$get() %*% m$getinverse()
    print(z)

    print("-- set the matrix, with the same values as above")
    m$set(MATRIX2)
    z <- m$get()
    print(z)

    print("-- get the inverse (shoud not be NULL because actually, the matrix has not changed)")
    z <- m$getinverse()
    print(z)

    print("-- set the matrix, with other values")
    m$set(MATRIX3)
    z <- m$get()
    print(z)

    print("-- get the inverse (shoud be NULL)")
    z <- m$getinverse()
    print(z)

    print("-- compute the inverse (we see the return value here)")
    z <- cacheSolve(m)
    print(z)

    print("-- get the inverse (shoud not be NULL)")
    z <- m$getinverse()
    print(z)

    print("-- multiply M by its inverse (should obtain identity matrix)")
    z <- m$get() %*% m$getinverse()
    print(z)
}

