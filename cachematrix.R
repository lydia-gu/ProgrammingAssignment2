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
  s <- x$getsolve()
  if(!is.null(s)){
    message("getting cached inverse of matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}