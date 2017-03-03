##These pair of functions will caches the inverse of a matrix 

## This function creates a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        i <-NULL
        set <-function(n) {
         x <<- n
         i <<- NULL
        }
  get <- function()x
  setInverse <-function() 
   i <<- solve(x)
  getInverse <-function()i
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## This function computes the inverse of matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mymatrix <- x$get()
  i <- solve(mymatrix, ...)
  x$setInverse(i)
  i
}
