## The functions implemented in this file supports solving an inverse of a 
## matrix and caching it for later use. This allows a more efficient 
## R code if the inverse of a matrix has to be retrieved very often.


## This function makes a special Matrix object where boths its matrix and its
## inverse is stored. This object also contains getter and setter functions for
## both its matrix and its inverse.
## This object is returned as a list of functions.

makeCacheMatrix <- function( x = matrix() )
{
    inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL  ## setting a new matrix for this object invalidates the 
                      ## the inverse.
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## This function solves for the inverse of a matrix. It uses the special Matrix
## object created using the makeCacheMatrix() function. If the Matrix object
## returns a NULL inverse, this means its inverse has not yet been calculated
## yet, so it calculates its inverse and stores it back in the Matrix object.
## If the Matrix object returns a valid inverse matrix, this means that its 
## inverse has been calculated before, so just returns the inverse. No further 
## calculations needed.

cacheSolve <- function( x, ... ) 
{
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if ( !is.null(inv) )
    {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve( data )
    x$setInverse( inv )
    
    inv
}
