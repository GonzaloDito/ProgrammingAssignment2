## The first function in this file  creates a special "matrix" object that can
## cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setInverse <- function(inverse) inv<<-inverse
  getInverse<-function()inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The objective of the last function is to compute the inverse of the special 
## "matrix" returned by the first function. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getInverse()
  if(!is.null(inv)){
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}