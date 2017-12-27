# This function creates a special "matrix" 
## object that can cache its inverse.
# The function takes a matrix as its argument.
makeCacheMatrix <- function(x = matrix()) {
        
        ## The object m, which will contain the inverse matrix
        #  is initially defined as NULL.
        m <- NULL
        
        ## Use the <<- operator to assign a value to an object in an 
        #  environment that is different from the current environment
        ## Now we set a value for the matrix x and reset inverse values,
        # if existing.
        set <- function(y = matrix ()) {
                x <<- y
                m <<- NULL
        }
        
        ## Return the value of the matrix 
        #  and then solve and return its inverse
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" 
#  returned by the function makeCacheMatrix defined above.  
#  If the inverse already exists and the matrix is in its original form,
#  the function below (cacheSolve) would retrieve from the cache
#  the existing inverse values of the matrix, preventing recalculation of 
#  the inverse thus improving processing efficiency.


cacheSolve <- function(x, ...) {
        
        ## Execute above-defined getinverse() function and test whether there exists 
        #  previously calculated inverse. If TRUE, fetch the data from the cache.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Otherwise, solve for the inverse.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
