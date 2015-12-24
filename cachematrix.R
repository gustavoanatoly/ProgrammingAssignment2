## Put comments here that give an overall description of what your
## functions do

## In this file there are two functions that works together
## where the first one, makeCacheMatrix receive a matrix and return a list
## with getters and setters. Putting the essencial values on 
## environment optimizing the search for variables.
## Second one, we have got cacheSolve that accept a list returned 
## from makeCacheMatrix solving the matrix and putting the inverse of it
## in cache using getters and setters from makeCacheMatrix

## Usage:
## Matrix creation
## > m <- matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2)
## Creating a cache matrix
## > mc <- makeCacheMatrix(m)
## Solving and putting on cache
## > cacheSolve(mc)



## Write a short comment describing this function
## This function basically expose four main methods
## that works with setters and getters.
## Putting the important variables on environment space,
## returning a list to expose this methods.
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
## CacheSolve wrap two mais sections. First one
## look inside the cache x if there are a solved 
## matrix and if not exist, retrieve original 
## matrix from x, calculate inverse matrix
## and put on environment the result.
## Returns inverse matrix
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
    
    x$set(data)
    
    ## Calculate inverse of matrix
    inverseMatrix <- data %*% solve(data)

    ## Caching solve result
    x$setSolve(inverseMatrix)

    return(inverseMatrix)
}
