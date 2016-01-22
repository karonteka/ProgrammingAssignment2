# Program to calculate the inverse of a matrix that is allocated in the cache memory



makeCacheMatrix <- function(x = matrix()) {
        final <- NULL
        set <- function(y) { #set the value of the matrix
                x <<- y
                final <<- NULL
        }
        get <- function() x  #get the value of the matrix
        setinverse <- function(inverse) final <<- inverse #set the value of the inverse
        getinverse <- function() final  #get the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
        final <- x$getinverse() 
        if(!is.null(final)) {
                message("Getting cached matrix data...")
                return(final)
        }
        data <- x$get()
        final <- solve(data, ...)#Here it actually calculate the matrix inverse
        x$setinverse(final)
        final        ## Return a matrix that is the inverse of 'x'
}
