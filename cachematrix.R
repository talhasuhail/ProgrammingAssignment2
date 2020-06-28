rm(list=ls(all=T))

##Functions that cache the inverse of a matrix

## Creation of matrix object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #Creating an Empty Matrix Inverse Object
  m <- NULL
  
  #Function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Function to get the matrix
  get <- function() {
    x
  }
  
  #Function to set the inverse of a matrix
  set_inverse <- function(inverse) {
    m <<- inverse
  }
  
  #Function to get the inverse of a matrix
  get_inverse <- function() {
    m
  }
  
  #Return the list of all above functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## Function to create the inverse of a special matrix created by "makeCacheMatrix". If the 
##  matrix hasn't been changed and its inverse has already been calculated, then the function get it's 
## inverse from the cache

cacheSolve <- function(x, ...) {
  
  #Getting inverse of a matrix
  m <- x$get_inverse()
  
  #Determining if the inverse is already in cache or not
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Getting the matrix
  data <- x$get()
  
  #Inverse of a matrix
  m <- solve(data, ...)
  
  #Setting the inverse of a matrix
  x$set_inverse(m)
  
  
  m
}

