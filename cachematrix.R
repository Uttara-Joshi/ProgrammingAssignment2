##the function finds the inverse of a matrix if it has not already been found
##If it has already been found, it just returns that previously-found value

## This function makes a list with elements :
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}




## This function finds the inverse of the matrix if it has not already been found.
##Otherwise it returns the value found previously.

cacheSolve <- function(x, ...) {
        m <- x[getsolve ()]
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x[get ()]
        m <- solve(data, ...)
        x[setsolve(m)]
        m
}
