## This function creates a matrix object that will ve used to cache an inverse operation

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        ## use '<<-' to assign a value to an object from an environment different from the current one
        x <<- y
        inv <<- NULL 
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix function
## If the inverse has already been calculated, the function will retrieve the result from the cache 

cacheSolve <- function(x, ...){
          ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        
        ## if the inverse has already been calculated, it will skip the compute
        
        message("getting cached data")
        return(inv)
    }
    
    ## otherwise, it will calculate the inverse
    
    data <- x$get()
    inv <- solve(data, ...)
    
    ## set the value of the inverse in the cache using setinv function
    x$setinv(inv) 
    inv
}
