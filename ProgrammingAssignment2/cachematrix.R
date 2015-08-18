## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setmatrix <- function(matrix){
        m <<- matrix
    }
    
    getmatrix <- function(){
        m
    }
    
    list(
        set = set, 
        get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' ( using solve(matrix) function to return the inverse )
    m <- x$getmatrix()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
