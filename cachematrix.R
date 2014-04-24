###############################Function Definitions############################

# This function makeCacheMatrix accepts a matrix as a parameter
# and contains list of four functions
#### 1. seti() Sets the value of matrix
#### 2. geti() gets the value of matrix
#### 3. setinvi() gets the value of inverse matrix
#### 4. getinvi() gets the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverval <- NULL
  #Setting the value of matrix into cache
  seti <- function(y) {
    x <<- y
    inverval <<- NULL
  }
  
  #Getting the value of matrix from cache
  geti <- function() x
  
  #Setting the value of inverse matrix into cache
  setinvi <- function(invset) inverval <<- invset
  
  #Getting the value of inverse matrix into cache
  getinvi <- function() inverval
  
  #Creating final output as list of functions
  list(seti = seti, geti = geti,
       setinvi = setinvi,
       getinvi = getinvi)
}


# This function cacheSolve accepts function list from makeCacheMatrix as
# parameter and checks if the inverse of the matrix already existing,
# then returns the data from Cache else compute using "solve" function and
# cache using function setinvi() defined in the makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inverval <- x$getinvi()
  #Check if the matrix-inverse exists using getinvi(), then return cached data
  if(!is.null(inverval)) {
    message("getting cached data")
    return(inverval)
  }
  
  #If Inverse doesn't exist in cache then computing and storing in cache using
  # setinvi()
  data <- x$geti()
  inverval <- solve(data, ...)
  x$setinvi(inverval)
  inverval
}

#################################Usage##########################################
