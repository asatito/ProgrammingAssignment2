## Description
## This function creates a special "matrix" object that can cache its inverse

## Usage 
## makeCacheMatrix(x)

## Arguments
## x An R object of type matrix.

## Value
## Reruns a list of functions to get/set the matrix and to get/set 
## the inverse of matrix.

## Examples
## x <- matrix(1:9 , nrow=3, ncol=3)
## xmv <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
       
        setMatrix <- function(y) {
                x <<- y         
                inv <<- NULL    
        }

        getMatrix <- function() x

        setInverseMatrix <- function(inverse) inv <<- inverse  
        
        getInverseMatrix <- function() inv

        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)

}

# Description
## This function computes the inverse of the special "matrix" returned by
## CacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed),then cachesolve retrieves the inverse from the cache.

## Usage 
## cacheSolve(x)

## Arguments
## x An R object of type list which allows it to manipulate the matrix.

## Value
## Return a matrix that is the inverse of 'x'

## Examples
## m <- matrix(c(4,3,3,2) , nrow=2,ncol=2)
## x <- makeCacheMatrix(m)
## xinv <- cacheSolve(x)

cacheSolve <- function(x, ...) {
     m <- x$getMatrix()
     inv <- x$getInverseMatrix()
          
     if(!is.null(inv)) {
             identityCacheMatrix <- m %*% inv
             identityMatrix <- diag(nrow(m))     
             isSameMatrix <- (dim(identityMatrix) == dim(identityCacheMatrix) 
                              && all(identityMatrix == identityCacheMatrix)
             )
         if (isSameMatrix) {    
         message("getting cached data")
         return(inv)
         }
     }
     x$setMatrix(m)
     inv <- solve(m)
     x$setInverseMatrix(inv)
     inv
}