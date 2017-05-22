## function makeCacheMatrix do creates a list of funcrtions
## and returns that list to the calling function cacheSolve
## it is necessary to call before the former function passing 
## to it the matrix whose you nedd to calculate the inverse
## after you need to call cacheSolve, passing to it the list
## of function figured out from makeCacheMatrix

## example of how you can call the two functions
## 1. mat <- matrix(rnorm(16), ncol = 4, nrow = 4)
## 2. y <- makeCacheMatrix(mat)
## 3. invmat <- cacheSolve(y)


makeCacheMatrix <- function(x = matrix()) {
    # Following the same format as the assignment example
    # Creating a makeCacheMatrix object will consist of
    # four functions encapsulated in a list
    # 1. set the matrix
    # 2. get the matrix
    # 3. set the inverse of the matrix
    # 4. get the inverse of the matrix
    
    # Initially set to NULL
    # Changes when the user sets the value
    inv <- NULL
    
    # set function
    # Sets the matrix itself but not the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get function
    # Gets the matrix itself but not the inverse
    get <- function() x
    
    # Manually set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getinverse <- function() inv
    
    # Encapsulate into a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)	
}

## cacheSolve:
## Once you create this matrix, you use the cacheSolve
## function to compute the inverse and cache the result
##
## If you try using cacheSolve again on the same special
## matrix, then the pre-computed result is obtained, thus
## avoiding any recomputation.  An informative message
## will be shown in the command prompt when the pre-computed
## result is returned instead.
##

## the function cacheSolve gets the list of functions as 
## an input parameter and return the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Following the same format as the assignment example
    
    # Get the current state of the inverse and see if it
    # has been computed yet
    inv <- x$getinverse()
    
    # If it has...
    if(!is.null(inv)) {
        # Simply return the computed inverse		
        message("Getting cached matrix")
        return(inv)
    }
    
    # If it hasn't...
    # Get the matrix itself
    data <- x$get()
    
    # Find the inverse
    inv <- solve(data, ...)
    
    # Cache this result in the object
    x$setinverse(inv)
    
    # Return this new result
    inv    
}

# example of a run of the two functions
# source("cachematrix.R")
# lm <- 9
# mat <- matrix(rnorm(lm), ncol = sqrt(lm), nrow = sqrt(lm))
# lstfun <- makeCacheMatrix(mat)
# invmat <- cacheSolve(lstfun)
# invmat
# [,1]        [,2]      [,3]
# [1,] -0.02438213 -0.67951969 0.0355127
# [2,]  0.05064314  0.08233979 0.6421254
# [3,]  1.88233925 -0.51566966 0.5223179

# mat %*% invmat
# [,1]          [,2]          [,3]
# [1,]    1  1.110223e-16 -5.551115e-17
# [2,]    0  1.000000e+00 -8.673617e-18
# [3,]    0 -2.428613e-17  1.000000e+00


# all.equal(diag(ncol(mat)), mat %*% invmat)
# [1] TRUE