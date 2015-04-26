## We create functions that are able to cache time-consuming computations: Caching the inverse of a matrix


## The function makeVector creates a special "vector", which is really a list containing a function to
##    set the value of the vector
##    get the value of the vector
##    set the value of the inverse
##    get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL                                            ## init the inverse of x

        set <- function(y) {                                    ## set the matrix
                x <<- y
                invX <<- NULL
        }
        get <- function() x                                     ## get the matrix
        
        setInverse <- function(inverse) invX <<- inverse        ## set the inverse matrix
        getInverse <- function() invX                           ## get the inverse matrix
        
        list(set = set, get = get,                              ## return
             setInverse = setInverse,
             getInverse = getInverse)
}



## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getInverse()                                  ## try to get from cache
        
        if(!is.null(invX)) {                                    ## if in cache...
                message("getting cached data")
                return(invX)
        }

        ## if not in cache
        data <- x$get()
        invX <- solve(data, ...)                                ## get the inverse
        x$setInverse(invX)
        invX                                                    ## return

}

## TEST

# m1 <- matrix( c(1, 2, 3, 4), nrow=2, ncol=2)
# m1
# cache <- makeCacheMatrix(m1)
# cache <- makeCacheMatrix(m1)  ## gets from teh cache now!
# out <- cacheSolve(m1)
# out