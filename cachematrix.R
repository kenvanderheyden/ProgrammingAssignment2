## Verified my code with test run like the following:

## create a matrix 
## > aMatrix = rbind(c(1, -1/4), c(-1/4, 1))

## store it in cache 
## > aCachedMatrix <- makeCacheMatrix(aMatrix)

## get inverted matrix from cache
## > cacheSolve(aCachedMatrix)

## check if inverted is correct
## > aMatrix %*% cacheSolve(aCachedMatrix)
## getting cached data
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## Seems correct, as the result returns the indentity matrix


## makeCacheMatrix, creates a matrix object that can cache its inverse

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


## cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    m <- x$getmatrix()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    ## solve(matrix) returns the inverse of the matrix 
    ## can be verified by multiplying the matrix with its inverted matrix, should result in the identity matrix. 
    m <- solve(data, ...) 
    
    x$setmatrix(m)
    m
}
