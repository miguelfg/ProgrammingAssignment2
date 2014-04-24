## 
# This file has two functions that helps to cache de computation of
# the inverse of a matrix.
# 
# The first one, makeCacheMatrix, appends 4 functions to a matrix object that
# help to set/get data and inverse of the matrix itself.
# 
# The second one, cacheSolve, actually computes the inverse of a matrix if is not cache.
# In order to get profit of this already get-cached-inverse-matrix, the argument of
# this function must be a return type of variable from the first matrix.
## 


# This function receives a matrix object and "appends to it" 4 functions to the matrix object:
# 1. get the data
# 2. set the data
# 3. get the inverse of the actual matrix
# 4. sets the inverse of the actual matrix
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


# This function computes the inverse of a matrix if it is not already cached.
# The argument x given must be an output from the function above makeCacheMatrix
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
