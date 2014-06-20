## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# R Programming
## Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix
## Giuseppe Sinicropi - 20 giugno 2014

## This programming assignment will require
## to write an R function is able to cache potentially time-consuming computations.

## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and their 
## may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here). 
## The assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  ## 1. Set and get the value of the matrix and save original input matrix
  set <- function(y) {
    x <<- y
    originalMatrix <- x
    inverseMatrix <<- NULL
  }
  get <- function() x
  ## 2. Set and get the value of the inverse
  setInverse <- function(inverse) 
    inverseMatrix <<- inverse
  getInverse <- function() 
    inverseMatrix
  ### Return list of functions
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Write a short comment describing this function


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  
  # If the inverse is already calculated and the original matrix is not changed, return it
  if (!is.null(inverseMatrix) & identical(x$x,x$originalMatrix)  ) {
    message("getting cached data")
    return(inverseMatrix)
  }  
  # If the inverse is not yet calculated or original matrix is changed, we have to calculate it
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  # Cache the inverse
  x$setInverse(inverseMatrix)
  # Return inverseMatrix
  inverseMatrix
}

