## This file consists of two functions, makeCacheMatrix() and cacheSolve()
## The two functions work together to calculate the inverse of a matrix and
##  store it in a cache in order to avoid repeating a calculation that can be
##  time-consuming

##  input: x is a matrix
##  There are three member functions:
##      get(): returns the input matrix
##      getInverse(): fetches the value of the inverse from the cache
##      setInverse(): stores the value of the inverse into the cahce

makeCacheMatrix <- function(x = matrix()) {   #input x will be a matrix
    inv <- NULL

    get <- function() { x }

    setInverse <- function(inverse) { inv <<- inverse }

    getInverse <- function() { inv }

    list(get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


##  input: an object created by the makeCacheMatrix function
##  output: the inverse of the matrix used in makeCacheMatrix
##  actions:  first attempts to fetch the inverse from the cache.
##      if the matrix isn't null, the message "getting cached data" is printed
##      and the inverse is returned
##      if the matrix fetched is null, we use the makeCacheMatrix's getter function to
##      retrive the matrix, then calculate the inverse using the solve() function.
##      Then we use the setInverse functioni to put the solution into the cache.
##

cacheSolve <- function(x, ...) {
    m <- x$getInverse()

    if(!is.null(m)) {
        message ("getting cached data")
        return(m)
    }

    data <- x$get()

    inverse <- solve(data, ...)

    x$setInverse(inverse)

    inverse
}
