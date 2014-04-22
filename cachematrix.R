## These functions implement a matrix with cached inverse.
## This function will create a special 'Matrix'
## which is actually a list containing functions to
## get and set the value of the matrix and to retrieve its inverse
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     ## Set the value of the matrix; remove cached inverse
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(i) {inv <<- i}
     getinverse <- function() inv
     list(set = set, get = get, 
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function will retrieve the cached inverse
## if there is no cached inverse, it solves for the inverse,
## sets it to the cache and returns it.

cacheSolve <- function(x) {
        i <- x$getinverse()
        if(!is.null(i)) {
             message("getting cached data")
             return(i)
        }
        x$setinverse(solve(x$get()))
        x$getinverse()
}
