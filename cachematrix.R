## This functions will create a matrix and calculate its inverse.
## Since calculating the inverse of a large matrix is a very intensive task,
## these functions will also store the result in a variable inside the function 
## environment.

## makeCacheMatrix is a function that has three options to the user and one
## inner option. First it creates a "m" variable inside it's workspace with a 
## NULL value. The "set" option will store the matrix, the "get" option will
## retrieve the matrix and the "getinverse" will retrieve it's inverse if it is
## already calculated with the cacheSolve function.
## Usage:
##
## matrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)) #creates matrix
##
## matrix$get() #return the matrix
##
## matrix$getinverse() #return it's inverse if already calculated with 
## cacheSolve, otherwise returns NULL
##
## matrix$set(matrix(c(5,6,7,8), nrow=2, ncol=2)) #stores a new matrix in the 
## function. Also erase the cached inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                m <<- inverse
        }
        getinverse <- function() {
                m
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function will calculate, store and return the inverse of the  
## current matrix. If the inverse is already stored, it'll only retrieve the 
## result.
## Usage:
##
## cacheSolve(matrix)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
