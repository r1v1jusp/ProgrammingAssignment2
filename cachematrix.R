

## There are two functions in this program.
## Together these functions provide the ability to take a matrix,
## store the matrix and its inverse. The cache can then be accessed
## thus avoiding the need to recompute the inverse each time.
##
## Function makeCacheMatrix creates a special object which takes a matrix argument
## and provides ability to store the matrix and its inverse and provide access to each.

## Function cacheSolve takes an argument that is the output of the above function.
## The function first checks if the inverse has been created and if so, returns the cached value
## If not, it creates the inverse, caches it by calling appropriate function on its first argument
## and then returns the inverse.
##


##
## Function makeCacheMatrix
##
## Function makeCacheMatrix takes a matrix argument
## It creates an object that stores the matrix and its inverse and
## provides a mechanism to set the value and get the value of the matrix and the inverse.
## It returns a list, each element of which is a function
## The following functions are returned via the list
##     set: to set the value of matrix
##     get: to get the value of the matrix
##     setinverse: to set the value of the matrix inverse
##     getinverse: to get the value of the matrix inverse
##

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(
        set = set, get = get,
        setinverse = setinverse, getinverse = getinverse
    )
    
}

##
## Function cacheSolve
## Function cacheSolve takes an argument which is a list (an output of the makeCacheMatrix function)
## The function first checks if the inverse exists by calling the "getinverse" function
## If it does exist, the inverse is returned using the "getinverse" function.
## This is preceded by a message that conveys that the inverse is returned using cached data.
## If the inverse does not exist, then the function determines the inverse using the "solve" function
## and stores it in the cache using the "setinverse" function and then returns the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

