## Caching the Inverse of a Matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {       
        i <- NULL
        # sets the matrix to variable x and its inverse to variable i
        set <- function(y) {
                x <<- y
                # caches the matrix for cacheSolve to check changes
                i <<- NULL
                # sets the value of i to NULL
        }
        get <- function() 
                x
        #collect the value of the matrix
        setinv <- function(solve) 
                i <<- solve
        #compute the inverse 
        getinv <- function() 
                i
        #get the cached inverse value 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        # creates a list for four functions
}

## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
##If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve 
##the inverse from the cache.

cachesolve <- function(x, ...) { 
        i <- x$getinv()
        # checked if the inverse is already been calculated and if it returns the cached value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
                # if matrix hasn't changed, sends messages and returns the cached matrix
        } 
        data <- x$get()
        #run the getmatrix to get the value
        i <- solve(data, ...)
        #compute the inverse from the matrix
        x$setinv(i)
        i
        #return the inverse        
}






