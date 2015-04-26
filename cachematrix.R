## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function makeVector creates a special "vector", which is really a list containing a function to
##    set the value of the vector
##    get the value of the vector
##    set the value of the mean
##    get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
		invX <- NULL			## init the inverse of x
        
		set <- function(y) {	## set the matrix
                x <<- y
                invX <<- NULL
        }
        get <- function() x		## get the matrix
        
		setInverse <- function(inverse) invX <<- inverse		## set the inverse matrix
        getInverse <- function() invX							## get the inverse matrix
        
		list(set = set, get = get,			## return
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
