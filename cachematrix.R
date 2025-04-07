## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse as NULL (not calculated yet)
    inv <- NULL
    
    # Setter function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the cached inverse when matrix changes
    }
    
    # Getter function to get the matrix
    get <- function() x
    
    # Setter function to cache the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Getter function to get the cached inverse
    getinverse <- function() inv
    
    # Return a list of all the functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and matrix hasn't changed),
## then retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
    # Get the cached inverse if it exists
    inv <- x$getinverse()
    
    # If inverse is cached, return it
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    # Otherwise, calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the calculated inverse
    x$setinverse(inv)
    
    # Return the inverse
    inv
}
