## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    solveMatrix <- NULL
    
    ## Set val to x and solveMatrix to NULL in envioronment.
    set <- function(val) {
        x <<- val
        solveMatrix <<- NULL
    }
    
    ## Return solveMatrix
    get <- function() x
    
    ## Set solve to solveMatrix in envioronment.
    setSolve <- function(solve) {
        solveMatrix <<- solve
    } 
    
    ## Get solveMatrix cached from envioronment.
    getSolve <- function() solveMatrix
    
    ## Expose getters and setters
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Get solved cached matrix
    inverseMatrix <- x$getSolve()

    ## Check if inverseMatrix is different of null and return it.
    if (!is.null(inverseMatrix)) {
        message("Inverser matrix founded in cache")
        return (inverseMatrix)
    }

    ## Get instance from environment.
    data <- x$get()
    
    ## Calculate inverse of matrix
    inverseMatrix <- data %*% solve(data)

    ## Caching solve result
    x$setSolve(inverseMatrix)

    return(inverseMatrix)
}
