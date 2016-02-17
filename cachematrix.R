

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    ## Begins by setting the inverse to NULL as a placeholder for a future value.
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    } 
    ## Defines a function to set the matrix x, 
    ## to a new matrix y, and resets the inverse m, to NULL.
    
    get <- function() x
    ## Returns the matrix x.
    
    setinverse <- function(solve) m <<- solve
    ## Sets the inverse m, to solve
    
    getinverse <- function() m
    
    ## Returns the inverse m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    ## Returns the list containing all of the functions just defined
}



cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    ## Assigns to m the content of getinverse in the the function above i.e. 
    ## the inverse of the matrix if this has been previously calculated, null otherwise. 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Checks the content of m: 
    ## if m is null then it prints a message and returns m=null,
    ## otherwise passes x to the variable called data, finds its inverse using the 
    ## function solve and stores the inverse in the variable m which will be then returned; 
    ## Sets also setinverse to be the inverse matrix m just calculated.
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
