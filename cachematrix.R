## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special "matrix"
## which is really a list containing a function to: 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set, or cache, the inverse of the matrix
## 4. Get the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  
  # i is the cache that holds the inverse of the matrix 
  i <- NULL
  
  # Function to set the matrix and clear the cache
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Function to return the value of the matrix
  get <- function() x
  
  #Function to set the inverse of the matrix into the cache
  setinv <- function(inv) i <<- inv
  
  #Function to get the inverse of the matrix from the cache
  getinv <- function() i
  
  #Return the list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## The function cacheSolve calculates the inverse of the the special
## "matrix" created with makeCacheMatrix.  However, it first checks to
## see if the inverse has already been calculated and stored in the cache.
## If it has been, it gets the inverse from the cache.  Otherwise, it
## calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
  
  # Get current cache value
  i <- x$getinv()
  
  #If cache value is not null, return the cached value which contains
  #the inverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #Otherwise, inverse must be calculated
  
  #Get value of stored matrix
  m <- x$get()
  
  #Compute inverse
  inv <- solve(m, ...)
  
  #Store inverse in cache for future queries
  x$setinv(inv)
  
  #Return the inverse
  inv
}
