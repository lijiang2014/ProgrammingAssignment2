## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to

## . set the value of the matrix
## . get the value of the matrix
## . set the value of the inverse matrix
## . get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(sol) s <<- sol
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
##   The following function calculates the inverse matrix created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

## Simple test here :
## mt1 <- matrix(c(1,2,3,4),2,2)
## mc1 = makeCacheMatrix(mt1)
##  cacheSolve(mc1)
##  cacheSolve(mc1)