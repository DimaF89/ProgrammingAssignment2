## R programming assignment 2 - May 2015

## The function 'makeCacheMatrix' takes a quadratic Matrix x (of dimensio (n x n)) as argument. 
## The function 'cacheSolve'  calculates and caches the inverse of the object created by 'makeCacheMatrix
## or retrieves the cached data, respectively.

## 'makeCacheMatrix' takes a quadratic matrix as input and creates a list object from which the follwowing function are callable:
#       set(): change the input matrix x of makeCacheMatrix
#       get(): retrieve the input x of makeCacheMatrix
#       Setinv(): let's the user manually set the inverse
#       getinv(): retrieves the inverse of x by returning 'invMat'

makeCacheMatrix <- function(x = matrix()) {
        dimensions <- dim(x)
        if (class(x) != 'matrix' || dimensions[1] != dimensions[2]) {
                cat("########\nWarning: 'makeCacheMatrix' takes a matrix of type (n x n) as argument\n########")
        }
        invMat <- NULL
        
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invMat <<- solve
        getinv <- function() invMat
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)

}


## 'Write a short comment describing this function'cacheSolve' retrieves the inverse of the 'makeCacheMatrix'
#       object x via the 'getinv' function. If the inverse is already cached, the cached data will be retrieved.
#       If the inverse is not available (i.e. in invMat = NULL), then the inverse is calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getinv()
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data,...)
        x$setinv(invMat)
        invMat
}
