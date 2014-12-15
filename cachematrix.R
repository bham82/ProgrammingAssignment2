## Functions makeCacheMatrix and cacheSolve which create a
## a special matrix object that is able to cache its inverse
## to eliminate the need to recalculate it when the object
## has not been changed.

###############################################################
## makeCacheMatrix
###############################################################
## Create a special matrix object that can cache its inverse
## Includes set, get, setinv, and getinv functions
###############################################################

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

###############################################################
## cacheSolve - Function to return the inverse of a matrix
###############################################################
## Check if the inverse of X has been calculated using getinv()
## If it has return the cached inverse
## If not
##    Get the data from X
##    Calculate the Inverse
##    Set the cache value
##    Return the inverse
###############################################################

cacheSolve <- function(x, ...) {
  i <- x$getinv() 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

###############################################################
## Test Code for the functions
##   Use Code >> Comment/Uncomment Lines to test
###############################################################
 ## Test to print out cached inverse
#  temp_1 <- makeCacheMatrix(matrix(c(3,0,2,2,0,-2,0,1,1), 3, 3, TRUE))
#  cacheSolve(temp_1)
#  cacheSolve(temp_1)

 ## Test to change the matrix so cache should be updated on 1st cacheSolve
# temp_1$set(matrix(c(4,0,0,0,0,0,2,0,0,1,2,0,1,0,0,1), 4, 4, TRUE))
# cacheSolve(temp_1)
# cacheSolve(temp_1)



