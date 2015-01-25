# Here, we are exploiting the cache to avoid repetitive computation on the unchanged data.
# Function "makeCacheMatrix" returns a list of 4 values:
# 1. Set a matrix, 2. Get a matrix, 3. Set the inverse and 4. Get the inverse 

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

# cacheSolve returns the inverse of the matrix either through calculating a-fresh or getting from the cache
# through the R function "solve"

 
cacheSolve <- function(x, ...) {
  myinv <- x$getinv()
  if(!is.null(myinv)) {
      message("getting cached data.")
      return(myinv)
  }
  mydata <- x$get()
      message("calculating the inverse!")
  myinv<- solve(mydata)
  x$setinv(myinv)
  return(myinv)
}
