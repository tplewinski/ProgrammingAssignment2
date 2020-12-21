## R Programming, Assignment 2

##    The functions provide solution to calculate the inverse matrix.
##    


## makeCacheMatrix creates an object to hold a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #Init
  inverse  <- NULL
  
  #sets a new matrix and delete an old inverse matrix
  set <- function(y) {
    x <<- y
    inverse  <<- NULL
  }
  
  #return the matrix
  get <- function() x
  
  
  ##set calculated inverse
  setinverse <- function(i) inverse  <<- i
  
  #return the inverse
  getinverse <- function() inverse 
  
  #list methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve calculates matrix inverse once and returns when function is called
##added parameter showMessages (default= TRUE) if printed message is required 
cacheSolve <- function(x,showMessages= TRUE, ...) {
  
  #try to get inverse
  inverse_matrix <- x$getinverse()

  # return existed inverse
  if(!is.null(inverse_matrix)) {

    #Print message is needed
    if (showMessages) {
      message("getting cached data")
    }
    return(inverse_matrix)
  }

  #else section
  
  #get matrix 
  matrix <- x$get()
  
  #calculate an inverse
  inverse_matrix <- solve(matrix, ...)
  
  #cache inverse
  x$setinverse(inverse_matrix)
  
  #print inverse matrix
  inverse_matrix
}