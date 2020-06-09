################################################################################ 
#                                                                              #
# These two functions return the inverse of a matrix either calculating it or  #
# getting it from the cache in case the calculation have been already made.    #
#                                                                              #
#               ! input matrix is assumed to be invertible !                   #
#                                                                              #
################################################################################


makeCacheMatrix <- function(x = matrix()) {

##
## This function creates a special "matrix" object, which is really a list 
## containing functions to:
##
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##        
                
        inv.mat <- NULL
        set <- function(y) {
                x <<- y
                inv.mat <<- NULL
        }
        get <- function() x
        setinv.mat <- function(solve) inv.mat <<- solve
        getinv.mat <- function() inv.mat
        list(set = set, get = get,
             setinv.mat = setinv.mat,
             getinv.mat = getinv.mat)

}



cacheSolve <- function(x, ...) {

        
##
## This function calculates the inverse of the special "matrix" created with 
## the previous function. However, it first checks to see if the result has 
## already been calculated. If so, it gets the reverse of the matrix from the 
## cache and skips the computation. Otherwise, it calculates it and sets the 
## value in the cache via the setinv.mat function.
##
##              ! input matrix is assumed to be invertible!
##                

        inv.mat <- x$getinv.mat()
        if(!is.null(inv.mat)) {
                message("getting cached data")
                return(inv.mat)
        }
        data <- x$get()
        inv.mat <- solve(data, ...)
        x$setinv.mat(inv.mat)
        inv.mat
        
}