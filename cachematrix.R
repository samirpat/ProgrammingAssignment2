#' makeCacheMatrix creates special matrix object that can cache its inverse

#' @param x is a square matrix that is invertible 

#'
#' @return outputs list containing four functions
#' 1. set the value of matrix
#' 2. get the value of matrix
#' 3. set the inverse of matrix
#' 4. get the inverse of matrix

#' 
#' @examples
#' mat <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    list(set = set, get = get , setinverse = setinverse , getinverse = getinverse)
}


#' cacheSolve computes inverse of a special matrix created by makeCacheMatrix.
#' If the inverse has already been calculated (and the matrix has not been changed),
#' then cacheSolve returns inverse from its cache

#' @param x is special matrix constructed from makeCacheMatrix function

#' @return outputs inverse of a matrix if it is a square invertible matrix

#' @examples
#' cacheSolve(makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2)))
cacheSolve <- function(x, ...) {
    
    i = x$getinverse()
    if(!is.null(i)){
        message("get cached matrix inverse")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}