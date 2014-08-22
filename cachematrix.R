# This function creates a special "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inv) inverse <<- inv
    
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    #Only square matrix has an inverse, so we asumme x is always invertible
    
    # If the inverse is already calculated, returns that value
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Retrieve from the cache")
        return(inverse)
    }
    
    # That part is only reachable when data is not in the cache
    matrix <- x$get()
    # Calculate the inverse and return it
    inverse <- solve(matrix)
    x$setInverse(inverse)
    
    inverse
}
