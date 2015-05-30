# Creates a special "vector", which is really a list containing functions to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        setmatrix <- function(inverse) {
                inverse <<- inverse
        }
        getmatrix <- function() {
                inverse
        }
                
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

cacheSolve <- function(x, ...) {
        inverse <- x$getmatrix()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setmatrix(inverse)
        inverse
}