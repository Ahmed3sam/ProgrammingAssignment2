## Put comments here that give an overall description of what your
## functions do
## the functions are used to cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is really
##a list containing a function to

##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the matrix
##4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
##the cacheSolve function return the inverse of the matrix
##firstly it checks if the inverse already computed
## if so , it gets the result and skip computations
## if not, it comutes the inverse and sets the value to the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
