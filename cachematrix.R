#' These two functions are used to create a special object that store a matrix 
#' and caches the inverse of it.
#' For this assignment, assume that the matrix supplied is always invertible.


#' This function creates a special "matrix" object that can cache its inverse.
#' @method makeCacheMatrix
#' @param x a "matrix" object supplied (in this case it's invertible)
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  # Set the value of the matrix
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the value of the inverse of the matrix
  setsolve <- function(solve) s <<- solve
  
  # Get the value of the inverse of the matrix
  getsolve <- function() s
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. 
#' If the inverse has already been calculated (and the matrix has not changed), 
#' then cacheSolve should retrieve the inverse from the cache.
#' @method cacheSolve
#' @param x a "list" object (in this case it's the list returned by makeCacheMatrix)
#' @return a "matrix" object, which is the inverse of the special matrix supplied
cacheSolve <- function(x, ...) {
  # Retrieve the inverse from the cache
  s <- x$getsolve()
  
  # If the cached inverse exists, return the cached inverse
  if(!is.null(s)){
    message("getting cached inverse of matrix")
    return(s)
  }
  
  # If the cached inverse does not exist, computes the inverse of the 
  # special "matrix", and caches the value of inverse of the matrix
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  # Return the computed inverse
  s
}