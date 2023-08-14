makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
       x <<- y
       m <<- NULL
 }

  get <- function() x
  setInverse <- function(inverse) m<<- inverse
  getInverse <- function() m

  list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


cacheSolve <- function(x, ...) {

##checking whether the solution is stored ,thus not NULL, in the cache
  m <- x$getInverse()
  if(!is.null(m)){
      message("ups, its already calculated. gettin cached data...")
      return(m)
  }
  
  data <- x$get()
## magic word here is solve: name of the function that calculates inverse matrix
  m <- solve(data,...)
  x$setInverse(m)
  m
}
