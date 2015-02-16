## MakeCacheMatrix() has the get and set functions for setting the initial value of matrix. ## It also has functions getInv and setInv for getting and setting the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  
  # Assigning NULL value to the matrix
  mat <- NULL
  
  # To get matrix
  get <- function() { x }
  
  # Set new Matrix
  set <- function(new_mat) {
    x <<- new_mat
    mat <<- NULL
  }
  
  # To get stored inverse of Matrix mat
  getInv <- function() {  mat }
  
  # set the Matrix mat with Inverse Matrix solution
  setInv <- function(solvedMat) {
    mat <<- solvedMat
  }
  
  #return a list of functions
  list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## cacheSolve looks for any cached value. If it finds one then it displays it.
## If no cached value is found then it gets the input matrix and calculates the
## inverse of that matrix. Then this calculated value is set as the cache value
## for the future.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Looking for cached value
  inverseMatrix <- x$getInv()
  
  # checking value of cache and using it if not NULL
  if(!is.null(inverseMatrix)) {
    message("getting cached Inverse Matrix data ....")
    return(inverseMatrix)
  }
  
  # Calculating the Inverse of Matrix if no cached matrix is found
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  
  # set inverse matrix in cache
  x$setInv(inverseMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  return(inverseMatrix)
}

