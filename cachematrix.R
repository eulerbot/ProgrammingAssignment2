## The following functions generate a specialized list to contain a matrix and 
## its inverse. The inverse of a matrix is calculated only once, regardless of 
## how many times the inverse is requested, provided the matrix has not been 
## changed.

## L. Sailer 2014

## This could be simplified into a single function, 'makeCacheMatrix', with the
## function 'getInverse' rewritten to do the work of 'cacheSolve'

## makeCacheMatrix(x)
##  Generate a list of functions handling the matrix 'x' and its inverse 'inv'
##  Contained functions:
##            set(x) :: Replace matrix
##             get() :: Return stored matrix
##   setInverse(inv) :: Set matrix 'x''s inverse
##      getInverse() :: Return stored inverse

makeCacheMatrix <- function(x = matrix()) {

    ## This is a new matrix... the inverse has not been solved
    inv <- NULL
    
    ## Here we're resetting the matrix, so reset the inverse as well.
    set <- function( y = matrix() ) {
        ## First make sure the new & old matrices are not identical, or the
        ## inverse would be recomputed for no reason.
        if ( !identical( x, y ) ) {
            x <<- y
            inv <<- NULL
        }
    }
    
    ## Note that to get the value of the matrix, you must call 'get()' (with 
    ## parentheses)
    get <- function() x
    
    ## Set the inverse of the matrix (this must also be a matrix)
    setInverse <- function( y = matrix() ) {
        inv <<- y
    }
    
    getInverse <- function() inv
    
    ## The final output object is a list of the above functions
    list( set = set, get = get,
        setInverse = setInverse, getInverse = getInverse )
}


## cacheSolve(x, ...)
##  Return the inverse of the matrix stored in the list 'x'. 'x' must be 
##  generated with the 'makeCacheMatrix' function.

cacheSolve <- function(x, ...) {
        
    ## Check the input 'x' is made by makeCacheMatrix by checking the names in 
    ## list; if the check fails, try to invert the input anyway.
    if ( identical(c("get","getInverse","set","setInverse"),sort(names(x))) ) {
        
        if ( is.null(x$getInverse()) ) {
            
            ## Inverse hasn't been solved for this matrix; solve it, store it.
            x$setInverse( solve(x$get(), ...) )
        }
        
        ## Return the inverse
        x$getInverse()
        
    } else {## Try to solve anyway.
        solve(x, ...)
    }
    
}
