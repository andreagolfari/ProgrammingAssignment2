## These functions can be used to speed up calculations that involve inverse matrices
## because they avoid having to repeat calculating the inverse of a matrix.
## The first function creates a list object that can store a matrix and cache its inverse
## matrix if we already computed it
## The second function checks if the inverse matrix is cached and uses it if available, otherwise
## computes the inverse and returns it

## Function makeCacheMatrix creates a matrix that is able to cache its inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        
        k <- NULL
        set <- function(y) {
                x <<- y
                k <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) k <<- inverse 
        getinverse <- function() k
        
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        # Check if a cached inverse matrix exists
        k <- x$getinverse()
        if (!is.null(k)==TRUE) {
                
                message("Using cached inverse matrix")
                return(k) # Return the cached inverse
                
        }
        
        # In case the inverse was not cached, calculate it and return it
        
        message("No cached inverse matrix; calculating inverse now")
        y <- x$get()
        k <- solve(y, ...)
        x$setinverse(k)
        
        k   # Return the inverse matrix that was just calculated
        
}

## T E S T I N G

A <- matrix(c(1, 3, 4, 4, 5, 6, 9, 8, 9), 3, 3)
B <- makeCacheMatrix(A)
#B
# Case in which we do not have a cached inverse
C <- cacheSolve(B)
C
# Case in which we already have a cached inverse
D <- cacheSolve(B)
D
