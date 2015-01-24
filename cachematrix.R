

## This function creates an object which has two properties
## x - a matrix and i - the inverse of x.
## these properties are set or returned by the four functions in 
## this object.
## if the set function is called the inversed property will be deleted
## and the next time the getsolve function is called it will be created.
##The properties of this object are not available outside
## of this object.

makeCacheMatrix<- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x<<-y
        i<<-NULL
    }
    get <-function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set =set, get = get,
         setsolve= setsolve,
         getsolve = getsolve)
}


## this function takes and object created from the function makeCacheMatrix
## and creates and returns the inverse of the matrix in the object.
## If the the inverse already exists it will just use the getSolve function
## to return the existing inverse.

cacheSolve <- function(x, ...) {
    i <- x$getsolve()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
}