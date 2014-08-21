##  <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. 
## the two functions below  are used to create a special object that stores a matrix and cache's its inverse.



## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## i) set the value of the matrix
## ii) get the value of the matrix
## iii) set the value of the inverse of the matrix
## iv) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(inverse) m <<- inverse
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse =  setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been created.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the data and stores it in the cache via the setMatrixInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setMatrixInverse(m)
  m
}
