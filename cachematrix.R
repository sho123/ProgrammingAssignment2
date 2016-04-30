## Two functions that creates a special object to store a matrix and cache
## its inverse.

## makeCacheMatrix is a function that creates a special matrix, which is a
## list containing a function to (1) set the value of the matrix, (2) get the
## value of the matrix, (3) set the value of the inverse, and (4) get the
## value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that calculates the inverse of the special matrix
## created with the makeCacheMatrix function. It first checks to see if the
## inverse has already been calculated, if so, it retrieves the inverse from
## cache. Otherwise it calculates the inverse and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data) %*% data
        x$setinverse(i)
        i
}
