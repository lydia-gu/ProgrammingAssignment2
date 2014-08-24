#' These two functions are used to create a special object that store a matrix 
#' and caches the inverse of it.
#' For this assignment, assume that the matrix supplied is always invertible.

#' This function creates a special "matrix" object that can cache its inverse.
#' @method makeCacheMatrix
#' @param x a "matrix" object
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
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
#' @param x an object
#' @return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)){
    message("getting cached matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}