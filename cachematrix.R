## This is a pair of functions that cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  y<-NULL
  ## Set the matrix
  set<-function(matrix){
    x<<-matrix
    y<<-NULL
  }
  ## Returns original matrix
  get<-function(){
    x
  }
  ## Set the inverse of the matrix
  setinverse<-function(inverse){
    y<<-inverse
  }
  ## Returns the inverse of the matrix
  getinverse<-function(){
    y
  }
  ## Return a list of the methods
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## Computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
## above. If the inverse has already been calculated, and the matrix has not 
## changed, then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Returns cached matrix inverse if the inverse has already been calculated
  i<-x$getinverse()
  ## Computes, caches, and returns matrix inverse if no inverse has been set
  if (is.null(i)){
    temp<-x$get()
    i<-solve(temp)
    x$setinverse(i)
  }
  else{
    message("Getting cached data")
  }
  ## Returns matrix inverse
  i
}
