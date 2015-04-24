## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). This assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse, which is really a list containing a function to
## 1.set the value of a matrix.
## 2.get the value of the matrix.
## 3.set the value of inverse matrix.
## 4.get the value of inverse matrix.


makeCacheMatrix <- function(x = matrix()) {

  ## set the cached value i as Null initially               
  i<-NULL
  
  ## set a matrix
  matrix<-function(y){
          x<<- y
          i<<- NULL
  }
  
  ## get the matrix
  getmatrix<-function(){
          x
  }
  
  ## set the inverse
  inverse<-function(inv){
          i<<-inv
  }
  
  ## get the inverse
  getinverse<-function(){
          i
  }
  
  ## return a list of functions
  list(matrix=matrix, getmatrix=getmatrix, inverse=inverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## get the cached value
  i<-x$getinverse()
  
  ## if the cached value exists, then return it, if else, get the matrix, compute the inverse, store and return it.
  if(!is.null(i)){
           message("getting cached data")
           return(i)
  }
  da<-x$getmatrix()
  i<-solve(da)
  x$inverse(i)
  i
  
}
