## The result of a programming assignment for the coursera-class R Programming
## Because matrix inversion is usually a costly computation this pair of function
## solve and cache an invertet matrix

## makeCacheMatrix creates a special "matrix" to cache the inverse of a square matrix.
## Really its a list which containing functions to
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialization
        s <- NULL
        
        ## set - cache a new matrix
        set <- function(y) {
                ## cache the new matrix
                x <<- y
                ## init the invers matrix
                s <<- NULL
        }
        
        ## get - return the data of the original matrix
        get <- function() x
        
        ## setsolve - cache the inverted matrix
        setsolve <- function(solve) s <<- solve
        
        ## getsolve - return the data of the inverted matrix
        getsolve <- function() s
        
        ## return the list function names
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computed the inverse of the special "matrix" created
## by makeCacheMatrix
## If the invertation of the matrix is cached, cacheSolve return the
## chached data and skips the computation. Otherwise it solve the
## inverse of the square matrix, cached the inverted data and
## returned the inverted data.

cacheSolve <- function(x, ...) {
        
        ## get the inverted matrix
        s <- x$getsolve()
        
        ## if available return the cached inverted matrix
        if(!is.null(s)) return(s)

        ## otherwise get the data of the origin matrix
        data <- x$get()
        ## invert the matrix
        m <- solve(data, ...)
        ## cache the inverted matrix
        x$setsolve(s)
        
        ## and return the inverted matrix of 'x'
        s
        
}
