## Describes the four functions within makeCacheMatrix function list
## set, get, setmatrix, and getmatrix
## My solution utilizes a paralell syntax to the makeVector given with the 
## assignment following closely the explanation and justification given here:
## https://github.com/Phylloxera/PA2-clarifying_instructions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get, 
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## in a two-part command, returns the inverse of an input matrix 
## assigned to the list makeCacheMatrix. 
## syntax part 1 "a <- makeCacheMatrix (newmatrix)"
## syntax part 2 "cacheSolve(a)"
## in this example syntax, newmatrix is defined outside the functions.
## My solution utilizes a paralell syntax to the cachemean given with the 
## assignment following closely the explanation and justification given here:
## https://github.com/Phylloxera/PA2-clarifying_instructions.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        ## Returns a matrix that is the inverse of 'x'
        ## solve(x) without additional arguments returns the inverse of x
        
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
