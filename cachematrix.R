## As we know that Matrix inversion is usually a costly computation and and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

##The following two functions will cache the inverse of a matrix. 


## First function - "makeCacheMatrix" creates a list containing a function to first set the value of 
## a matrix, and then get its value. After that it sets the value of inverse of the matrix 
## and then get it.
## As required, this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  #setting matrix value
  inver <- NULL
  set <- function(a){
    x <<- a
    inver <<- NULL
  }
  
  #getting matrix value
  get <- function() x
  
  #setting inverse of matrix
  setinverse <- function(inverse) inver <<- inverse
  
  #getting inverse of matrix
  getinverse <- function() inver
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## The second function - "cachesolve" computes the inverse of the special "matrix" returned by 
## the above "makeCacheMatrix" function. As required, if the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## For the below function, my assumption is that the matrix supplied is always invertible.


cacheSolve <- function(x, ...) {
  
  inver <- x$getinverse()
  
  if(!is.null(inver)) {
    message("Getting cached data.")
    return(inver)
  }
  
  data <- x$get()
  inver <- solve(data) ##Getting inverse of a square matrix
  x$setinverse(inver)
  inver
}
