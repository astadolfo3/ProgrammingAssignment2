## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#####################################################
# Function makeCacheMatrix(matrix)
#
# Description: 
#
# This function takes a matrix as an input and returns 
# a list with four functions:
# set: Sets the  matrix where the inverse can be calculated
# get: returns the original matrix
# setinv: sets the inverse in the cache
# getinv: gets the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x<<- y
    i<<- NULL
  }
  get <- function() x
  setinv <- function(inv) i<<-inv
  getinv <- function() i
  list (set=set, get=get, setinv=setinv, getinv=getinv)
    

}


## Write a short comment describing this function


#####################################################
# Function cachesolve(matrix)
#
# Description: 
#
# This function returns the inverse of a matrix:
# First it calls the function getinv() to see if the inverse for 
# this matrix exists in the cache. If so it returnes the cached value
# Otherwise calculates the inverse, stores the inverse in the cache and
# and returns the inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinv(i)
  i
}
