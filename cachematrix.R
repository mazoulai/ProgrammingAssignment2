## These functions create a special bject that stores a numeric matrix and caches 
## its inverse

## this function creates a special "matrix", which is really a list 
## containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) x_inv <<- inverse
        getinverse <- function() x_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets  
## the value of the matrix inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        x_inv <- x$getinverse()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data)
        x$setinverse(x_inv)
        x_inv
}
