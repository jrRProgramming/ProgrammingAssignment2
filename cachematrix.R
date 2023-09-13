## The function makeCacheMatrix returns a list of 4 functions associated with the argument matrix x
## The functions are:
## 1. set(y) : sets the matrix to be y
## 2. get()  : gets the matrix
## 3. setinv(invers): sets the inverse of the matrix to be invers
## 4. getinv() : gets the inverse of the matrix
## 

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setinv <-function(invers) inv <<- invers
      getinv <-function() inv
      list (set=set,get=get,setinv=setinv, getinv=getinv)
}


## The function cacheSolve returns the inverse of the matrix 
## used to create the CacheMatrix by the function makeCacheMatrix.
## If the inverse has already been calculated, the inverse is 
## retrieved from the cache without computation using th getinv function
## If the inverse has not yet been calculated, the inverse is calculated using
## the solve function and this inverse is stored in the cache using
## the setinv function.
## The matrix is assumed to be invertible.

cacheSolve <- function(x, ...) {
      inv <-x$getinv()
      if(!is.null(inv)) {
            message("from cache")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
