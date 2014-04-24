## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv_matrix) inv <<- inv_matrix
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$get_inverse()
  if(!is.null(inv_mat)) {
    message("getting cached inverse matrix")
    return(inv_mat)
  }
  matrix <- x$get()
  #   calculate the inverse matrix of matrix
  inv_mat <- solve(matrix)
  x$set_inverse(inv_mat)
  inv_mat
}
