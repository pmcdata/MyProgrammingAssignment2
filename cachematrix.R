

##The function makeCacheMatrix creates a list of functions to:
## set the matrix
## get the matrix
## find the inverse of the matrix
## return the inverse of the matrix

## The function cacheSolve uses the list of functions created in makeCacheMatix
## If the inverse already exists, it takes the cached inverse, otherwise it creates the 
## inverse using the setinv funtion created in the makeCacheMatrix function.


## makeCacheMatrix creates a list of functions that will set and obtain the given matrix
## and then find and return the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
  
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function will use the list of functions returned by makeCacheMatrix, in order to
## return the inverse of the original matrix. If the inverse has already been created then
## this inverse will be returned, otherwise it will create the inverse of the matrix itself.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


##test
mat <- matrix(c(1,2,3,4),2,2)

thismat <- makeCacheMatrix(mat)

cacheSolve(thismat)
