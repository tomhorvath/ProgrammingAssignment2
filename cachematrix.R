## The functions below are written based on the vector example functions 
## provided in the description of the programming assignment 2.

## The makeCacheMatrix function reads and stores the inputted matrix. The 
## function also stores and retreives the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()
                x
        setinv <- function (inversem)
                inv <<- inversem
        getinv <- function()
                inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function return a matrix that is the inverse of 'x'. If 
## called repeateadly, the function returns the inverse matrix from the cache.
## The function also has an additional argument: a new matrix called 'y'. When 
## the new matrix is inputted the inverse of 'y' will be calculated and stored 
## in the cache. When the input matrix is changed during the repeated calls, 
## the function notifies the user about the change.

cacheSolve <- function(x, y = NULL, ...) {
        if (!is.null(y)) {
                # new matrix input
                if (!identical(x$get(), y)) {
                        # is the new matrix is similar with the previous input?
                        message ("new data was inputted")
                        x$set(y)
                }
        }
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        # matrix inversion
        inv <- solve(data)
        # cache the inverse
        x$setinv(inv)
        # cat("\014")
        message("Matrix inverse:")
        inv
}


## Execution of the functions:  ------------------------------------------------
## Step 1: Input matrices
# matrix1 <- matrix(c(1,3,2,6,4,7,7,9,8), nrow = 3, ncol = 3)
# matrix2 <- matrix(c(2,3,1,6,5,7,4,9,8), nrow = 3, ncol = 3)
## Step 2: Construct a temp 'list', which strores matrix1
# temp <- makeCacheMatrix(matrix1)
## Step 3: process matrix1 and its inverse
# cacheSolve(temp) # first run; inverse is calculated
# cacheSolve(temp) # second run; inverse is retrieved from cache
## Step 4: Alter the matrix input (matrix2)
# cacheSolve(temp, matrix2) # first run, matrix2 inverse is calculated
# cacheSolve(temp, matrix2) # second run; the inverse is retrieved from cache

