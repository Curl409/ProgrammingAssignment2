## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: create, set, get matrix, and set/get inversed matrix.
# cacheSolve: return inversed matrix

## Write a short comment describing this function
# set: save new matrix and clear inversed matrix.
# get: return matrix
# setsolve: save inversed matrix to "invmat"
# getsolve: return inversed matrix "invmat"

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y){
        x <<- y
        invmat <<- NULL 
    }
    get <- function() x
    setsolve <- function(solved) invmat <<- solved
    getsolve <- function() invmat
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
# Return cached inversed matrix if exits.
# Otherwise compute inversed matrix 
# and save to "invmat" and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getsolve()
    if(!is.null(invmat)){
        message("getting cached data")
        return(invmat)
    }
    data <- x$get()
    invmat <- solve(data)
    x$setsolve(invmat)
    invmat
}
