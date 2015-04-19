##Functions to store a given matrix and its inverse in cache

## This function receives a square matrix as a parameter and
## returns a list of 4 functions:
## 1 get: retrieve the 'special' matrix
## 2 set: set the 'special' matrix
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
## 'makeCacheMatrix', or, in case it doesn't have an associated value, it calculates it.
## The input parameter is the returned object of the function 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  message("calculating and caching inverse...")
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
}
