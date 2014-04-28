## This file contains two functions that are meant to work together. They are used to create 
## a special object to store a matrix and cashe its inverse.

## This fuction creates a special object which is simply a list of four fuctions that will set
## the value of the matrix, get the value of the matrix, set the inverse of the matrix, and
## get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will calculate the inverse of the matrix that is created by the fuction above.
## However, it will first check to see if the inverse has already been calculated and cached. If
## the inverse has been calculated and cached already, the fuction will return the cached value.
## Otherwise, the function will calculate, cache and return the the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)){
            print("retrieving cashed inverse matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
