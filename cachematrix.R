## There are R functions that able to cache inverse matrix computations for matrices.
## If the content of our matrix object is not changing, its inverse matrix can be got from cache
## so  we don't need to compute it again.
## If the new value will be set to our matrix object, then inverse matrix can be recomputed.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInv <- function(inverse){
                inv <<- inverse  
        } 
        getInv <- function() {
                inv
        }
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("Getting cached data:")
                return(inv)
        }
        data <- x$get()
        if (dim(data)[1] != dim(data)[2]) {
                message("Given matrix is not square!")
                return(inv)
        }
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}