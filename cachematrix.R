# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  MatrInv <- NULL # sets the cache empty
  set <- function(y) {
    x <<- y
    MatrInv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) MatrInv <<- inverse
  getinverse <- function() MatrInv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function calculates the inverse of the special "matrix" created with the above function.
# It first checks to see if the inverse has already been calculated
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache via the setmean function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  MatrInv <- x$getinverse()
  if(!is.null(MatrInv)) {
    message("getting cached data.")
    return(MatrInv)
  }
  data <- x$get()
  MatrInv <- solve(data)
  x$setinverse(MatrInv)
  MatrInv
}

# tests:
input<-matrix(c(1,2.5,2.5,3.4),2,2)
tempMatrix<-makeCacheMatrix(input)
tempMatrix$get()
cacheSolve(tempMatrix)
cacheSolve(tempMatrix)
