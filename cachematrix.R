## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # reset inverse matrix
        i = NULL
        
        # Initialize the underlying matrix object, NULLs its inverse cache
        set <- function (y){
                x <<- y
                i <<- NULL
        }
        
        ## get the matrix from input object
        get <- function () x
        
        
        ## set the inverse of the matrix
        setinv <- function(inv) i <<- inv
        
        ## get the inverse of the matrix
        getinv <- function () i
        
        ## return a fully formed matrix object
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by  
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        
        # Return the inverse if the cache exists
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## Get the matrix to data
        data <- x$get()
        
        ## calculate the inverse matrix with solve function
        i <- solve(data)
        
        ## set the inverse cache value
        x$setinv(i)
        
        ## return the inverse matrix
        i
}
