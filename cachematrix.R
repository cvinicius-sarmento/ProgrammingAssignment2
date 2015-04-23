################################################################
## Functions to store a given matrix and its inverse in cache ##
################################################################

## This function receives a square matrix as a parameter and
## returns a list of 4 functions:
## 1 get: retrieve the value of the 'special' matrix
## 2 set: set the value of the 'special' matrix
## 3 getInverse: retrieve the value of the inverse matrix (it doesn't calculate it here)
## 4 setInverse: set the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(new_x = matrix()){
    x<<-new_x
    inverse<<-NULL
  }
  get <- function(){
    x
  }
  getInverse <- function(){
    inverse
  }
  setInverse <- function(value){
    inverse<<-value
  }
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## This function returns the inverse of the 'special' matrix created by the function
## 'makeCacheMatrix'.
## In case of not having the inverse matrix in cache, it calculates and stores it.
## The input parameter is the returned object of the function 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached inverse matrix")
    return(inv)
  }
  message("calculating and caching inverse matrix...")
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
}

##================================================================================
## you can test the functions with the following commands:
## specMatrix <- makeCacheMatrix(matrix(c(1,3,2,4),2,2))
## cacheSolve(specMatrix) ##1st time: it calculates and stores the inverse matrix
## cacheSolve(specMatrix) ##2nd time: it retrieves the inverse matrix