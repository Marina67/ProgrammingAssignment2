## makeCacheMatrix creates and returns a list of functions
## that used by cacheSolve to get the result matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  ## declare result matrix and initialize with NULL
  
  inverse_matrix <- NULL
  
  ## create the matrix in the working environment
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  
  ## apply function and store result in cache
  setmatrix <- function(solve) inverse_matrix <<- solve
  getmatrix <- function() inverse_matrix
  
  ## return the created functions to the working environment
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse_matrix <- x$getmatrix()
  
## return result matrix from cache if it exist 
  
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
    
  }
  liner_data <-x$get()

  ## final calculation  
  inverse_matrix <- solve(liner_data,...)
  
  x <- setmatrix(inverse_matrix)
  ## displays result
  inverse_matrix
}

