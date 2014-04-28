
## This function creates a special "matrix" object that can caches its inverse.

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

        setsolve <- function() m <<- solve(x)  # Cache the inverse of the matrix
           
        getsolve <- function() m  # give the value of the cache

        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
      
        m <- x$getsolve()    ## Put the value of getsolve in the cache
        if(!is.null(m)) { ## Verify the value of the inverse in the cache
                message("getting cached data")
                return(m) ## return the inverse  of the matrix located in the cache
        }

        data <- x$get() ## if there is no value in the cache, here we put the value of the matrix in data
        m <- solve(data, ...) ## compute the inverse of the matrix and put it in the cache
        x$setsolve(m)         ## set the value of the cache
        m
}
