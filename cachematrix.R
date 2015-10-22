## Checks to see if the inverse of the input matrix has already been computed.
## If computed, gets the inverse from cache. Else, calculates the inverse of the 
## input matrix and sets the inverse in the cache via setInv() function
## Assumptions: The matrix supplied is always invertible

## makeCacheMatrix() - Takes a matrix and creates a special vector which is a list 
## containing functions to
##      1. set the value of the vector
##      2. get the value of the vector
##      3. set the value of solve(inverse)
##      4. get the value of solve(inverse)


makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInv <- function(solve) i <<- solve
      getInv <- function() i
      list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve(): Takes the special vector created by the above function and checks if 
## the inverse has been already computed. If computed returns the inverse from cache else
## computes the inverse, sets it to cache varaible and then returns it.

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)) {
        message("getting cached Inverse")
        return(i)
    }
    mat_data <- x$get()
    i <- solve(mat_data, ...)
    x$setInv(i)
    i
}
