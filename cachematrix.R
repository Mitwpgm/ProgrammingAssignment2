
## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function. If the inverse has already 
## been calculated before(and the matrix has not changed), 
## then the cacheSolve function will retrieve the inverse from the cache.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matInv <- NULL
        
        setMat <- function(y) {
                x <<- y
                matInv <<- NULL
        }
        getMat <- function() x
        setMatInv <- function(inv) matInv <<- inv
        getMatInv <- function() matInv
        list(setMat = setMat, getMat = getMat,
             setMatInv = setMatInv,
             getMatInv = getMatInv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getMatInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMat()
        m <- solve(data, ...)
        x$setMatInv(m)
        m
}
