makeCacheMatrix <- function(x = matrix()) {
  myinv <- NULL
  set <- function(y) {
      x <<- y
      myinv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) myinv <<- inverse
  getinv <- function() myinv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  myinv <- x$getinv()
  if(!is.null(myinv)) {
      message("getting cached data.")
      return(myinv)
  }
  mydata <- x$get()
  myinv <- solve(mydata)
  x$setinv(myinv)
  myinv
}


