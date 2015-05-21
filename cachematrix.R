## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## These two functions work together to implement a caching
## strategy for matrix inversion: one object wraps the original
## matrix type inside an enriched objects (i.e. it adds a
## 'property' which is used to store the inverse of itself).
## The other function is used to access the inverse of the
## matrix and it has to be used instead of solve() function
## to invert the given matrix

## This function wraps a matrix object by storing the matrix
## together with its inverse. The inverse of the matrix is
## set from the outside, allowing a lazy computation.

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
