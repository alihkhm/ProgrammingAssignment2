## Put comments here that give an overall description of what your
## functions do

## Two functions are defined here to cache the inverse of a matrix and prevent repeating the calculation of the inverse matrix in next times

## Write a short comment describing this function
## The first function sets and gets the value of the original matrix and sets and gets the value of the calculated inverse

makeCacheMatrix <- function(x = matrix()) {     ## definition of the matrix with 'x' matrix as the argument  
  inv <- NULL                                   ## there is no inverse matrix calculated yet, assigning NULL value
  set <- function(y) {                          ## a new function enabling setting a new value to the argument of the function
    x <<- y                                     ## replacing 'x' with the new value
    inv <<- NULL                                ## resetting the 'inv' matrix with assigning NULL value 
  }
  get <- function() x                           ## reading the input matrix 'x'
  setInv <- function(solve) inv <<- solve       ## assigning the inverse of the matrix 'x' 
  getInv <- function() inv                      ## reading the inverse matrix
  list(set = set, get = get,                    ## making the output list
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
## the second function calculates the inverse of the original matrix with the vector created with the first function. 
## Before doing invers calculation, this function first checks if the inverse matrix is already calculated.
## If the inverse function exists, it returns the existing result and skips new calculations.
cacheSolve <- function(x, ...) {                          ## definition of the matrix with the vector argument from previous function
        ## Return a matrix that is the inverse of 'x'    
  inv <- x$getInv()                                       ## reads the existing value for the inverse matrix
  if(!is.null(inv)) {                                     ## if it's not NULL, then the value is already calculates and should be retuened
    message("getting cached data")
    return(inv)                                           ## returning the already calculated inverse matrix
  }                                                       ## comments after here are executed if the inverse matrix does not exist
  data <- x$get()                                         ## getting the original matrix
  inv <- solve(data, ...)                                 ## calculation of the inverse matrix
  x$setInv(inv)                                           ## setting the inverse matrix
  inv                                                     ## returning the inverse matrix
}
