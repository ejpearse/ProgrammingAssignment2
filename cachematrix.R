## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This makeCacheMatrix function sets up four functions within in: set get setinverse and getinverse
# The single argument formakeCacheMatrix is a matrix
# The set function has a single argument y which is assigned to x in the makeCacheMatrix environment
# When this function is called (set(y)) this will assign y to x
# The get function has no argument and it will return x from the makeCacheMatrix environment
# The setinverse function has a single argument solve.  This argument is assigned to m in the 
# makeCacheMatrix environment
# The getinverse function no argument and it will return m from the makeCacheMatrix environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    # The set function first assigns the argument (y) to the matrix x in the makeCacheMatrix environment
    m <<- NULL
    # The set function then assigns m a NULL values 
    
  }
  get <- function() x
  # The get function retrieves x and returns it to environment in which the function was called 
  
  setinverse <-function(solve) m <<-solve
  # The setinverse assigns m to the inverse which is an object in the makeCacheMatrix environment 
  
  getinverse <- function() m
  # The getinverse retrives m and returns it 
  
  list(set =set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  # This creates a list of the functions.  Not sure what this actually does 
}


## Write a short comment describing this function
# The cacheSolve function has a first agrument of x and other arguments that can be passed to 
# it related to the solve function.
# The first part in cacheSolve calls the getinverse function, which will return the current value 
# of x in the makeCacheMatrix which is assigned to m in the cacheSolve environment.
# The next step checks whether m is not null.  If it is not null then m is returned and the
# function ends processing
# If the m is null, then we call the get function which returns x and we assign this to data
# The solve function is applied to data, which calculates the inverse of x and this is assigned to m
# The setinverse function is then called, which assigns m in the cacheSolve environment to m in the 
# makeCacheMatrix environment 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # retrives the matrix x and assigns this to m 
  
  if(!is.null(m)) {
    message ("getting cached data")
    return (m)
  }
  # if m is not null (that is there was an inverse previously calculated), then return the cached value of m
  
  data <- x$get()
  m <- solve(data, ...)
  # Calculated the inverse of x
  x$setinverse(m)
  # Set the inverse to the new value that was calculated above
  
  m
  # Return m, that is the newly calculated inverse
  
}
