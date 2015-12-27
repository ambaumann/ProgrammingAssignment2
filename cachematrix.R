## Functions used to store and solve cached inverse matrixs

## This function holds the cached matrix when it is
## calculated by the cacheSolve function.
## This function takes in a matrix or it can have its matrix
## set later.
## This function returns a list of 4 functions that
## are used to change the original matrix and its
## inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) inverse <<- inverseMatrix
  getInverseMatrix <- function() inverse
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## This function takes in a cache Matrix whos inverse
## matrix may or may have not been calculated.
## This function all takes in additional parameters that
## may be used to solve the inverse if the inverse needs
## to be calculated.
## This function returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
  m <- x$getInverseMatrix()
  if(!is.null(m)){
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
}
