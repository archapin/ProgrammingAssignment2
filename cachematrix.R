## The computation of the inverse of a matrix can be very costly in time and
## resources.

## Here there are two functions that are used to cache the inverse matrix of a
## matrix with the objective to store the inverse in cache and to avoid computing
## it repeatedly.


## This function creates a special matrix which is a list with a series of 
## functions that (1) sets the original matrix in cache, 
## (2) it can get the matrix in cache,  (3) sets the inverse of the matrix
## in cache, and (4) get the inverse matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL 
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function()x
        setSolve <- function(solve) m_inv <<- solve
        getSolve <- function() m_inv
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)                
}


## This function computes the inverse of the matrix returned by the
## makeCacheMatrix function 

## If the inverse matrix has already been calculated (and the matrix has not
## changed), then cacheSolve retrieves the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        m_inv <- x$getSolve()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setSolve(m_inv)
        m_inv
}
