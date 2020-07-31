## These two functions work together to create a matrix, compute its inverse, 
## and cache it. This is to make sure that the one matrix called only needs to 
## be calculated once, and still be able to retrieve the values later. 

## This function creates a special type of matrix object that can caches its 
## inverse for later retrieval 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special matrix created in the 
## makeCacheMatrix function above. When the matrix has stayed unchanged, 
## then the inverse is not calculated again, and the cached value is 
## retrived. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
