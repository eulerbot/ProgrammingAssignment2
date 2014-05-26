## The following function generates a specialized list to contain a matrix and 
## its inverse. The inverse of a matrix is calculated only once, regardless of 
## how many times the inverse is requested, provided the matrix has not been 
## changed.

## L. Sailer 2014

## makeCacheMatrix(x)
##  Generate a list of functions handling the matrix 'x' and its inverse 'inv'
##  Contained functions:
##            set(x) :: Replace matrix
##             get() :: Return stored matrix
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
    
    ## Either return or compute and return the inverse of the matrix
    getInverse <- function() {
        if ( is.null( inv ) ) {
            
            inv <<- solve( x )
        }
        inv# Return the value
    }
    
    ## The final output object is a list of the above functions
    list( set = set, get = get, getInverse = getInverse )
}
