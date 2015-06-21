##Creating a function which has a matrix as argument "mat" 
##default set to null matrix
makeCacheMatrix <- function(mat = matrix()) {      
## Initializing the value of the inverse matrix matI to NULL
        matI <- NULL
## Defining function set() where the value will be cached in Matrix
        setmatrix <- function(y) {
## Matrix is created first time
                mat <<- y
## Reset the value of matrix inverse when matrix will be reloaded newly
                matI <<- NULL
        }
## Getting the value of matrix through get() function
        getmatrix <- function() mat
## Setting the inverse of invertible square matrix via value passed as argument
        setmatrixInverse <- function(mat) matI <<- solve(mat)
## Getting/ passing the inverse of invertible square matrix via matI
        getmatrixInverse <- function() matI
## passes the value of the function makeCacheMatrix 
## via list containing four function as elements
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setmatrixInverse = setmatrixInverse,
             getmatrixInverse = getmatrixInverse)
}

## The following function is used to get the cache of the inverse matrix and
## retrieving the same when required
cacheSolve <- function(l, ...) {
## Looking for the inverse of matrix from cache through a function getmatrixInverse() in list l
        matI <- l$getmatrixInverse()
## Showing user-friendly message that 
## returning inverse of invertible square matrix if the inverse exists
        if(!is.null(matI)) {
                message("getting cached data")
                return(matI)
        }
##Loading of matrix into "data" via getmatrix() if the inverse matrix does not exist
        data <- l$getmatrix()
##Calculating the inverse of invertible square matrix (=data) via solve() function
        matI <- solve(data, ...)
##Setting the value of inverse matrix into matrix matI via function setmatrixInverse() in list l
        l$setmatrixInverse(matI)
##Returning the inverse matrix matI
        matI
}
