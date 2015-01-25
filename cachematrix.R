## Assign value of Null to m. Set value of the matrix. Get the value of said matrix. 
##Set the inverse of this matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) ##set the value of the matrix
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x ##get the value of the matrix
  setinverse <- function(inverse) m <<- inverse ##set the inverse of the matrix
  getinverse <- function() m ##get the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ##assigns inverse of matrix to m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } ##check to see if inverse of matrix has been calculated. if yes, gets inverse
  data <- x$get() ##gets data
  m <- solve(data, ...) ##calculates inverse
  x$setinverse(m) ##sets inverse
  m ##returns m
}
