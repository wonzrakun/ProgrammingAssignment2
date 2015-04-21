## Matrix Inversion is an operation that might be heavy from a computational 
## point of view. 
## As such, it might be convenient to cache the inverse of a matrix, 
## instead of computing it all the times.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## CacheSolve computes the inverse of the matrix. Before, it checks if it has
## been already computed, and in such case it skips the computation. 
## If it is not the case it returns the inverse by means of the makeCacheMatrix
## function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

#example

## > matrix<-makeCacheMatrix(matrix(50:53,2,2))
## > matrix$get()
##       [,1] [,2]
## [1,]   50   52
## [2,]   51   53
## > cacheSolve(matrix)
##       [,1] [,2]
## [1,] -26.5   26
## [2,]  25.5  -25
## > cacheSolve(matrix)
## getting cached data
##       [,1] [,2]
## [1,] -26.5   26
## [2,]  25.5  -25
