## Put comments here that give an overall description of what your
## functions do

#This function creates a special "matrix" object that can cache its inverse, which is really a list containing a function
makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y) {      #set the value of the matrix
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x       #get the value of the matrix
  setInverse <- function(new_inverse) cached_inverse <<- new_inverse     #set the value of the inverse
  getInverse <- function() cached_inverse                                #get the value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {           
    message("getting cached data")   
    return(inverse)
  }
  data <- x$get()                    
  inverse <- solve(data, ...)        
  x$setInverse(inverse)
  inverse
}
