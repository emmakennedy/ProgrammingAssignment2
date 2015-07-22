## Takes in a matrix and creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Sets the value of the matrix
    set <-function(y) {
        x <<-y
        m <<- NULL
    }
    ## Gets the value of the matrix
    get <-function() x
    ## Sets the value of the inverse
    setinverse <- function(solve) m <<- solve
    ## Gets the value of the inverse
    getinverse <- function() m
    ## Creates a list containing the functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Checks if the matrix has not changed and the inverse has been calculated, and then returns the inverse from the cache. If not, computes the inverse of the special matric

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    ## Checks to see if the inverse was already calculated
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## If not already calculated, calculates and sets the value of the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}