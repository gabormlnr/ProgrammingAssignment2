
## This function caches an input matrix

makeCacheMatrix <- function(x = matrix()) {
  
  c <- NULL
  
  set <- function(y){
  
    x <<- y
    
    c <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv) c <<- inv
  
  getinv <- function() c
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function inverts a given matrix. 
## If already calculated and cached then brings back the cached value.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  c <- x$getinv()
  
  if(!is.null(c)){
    
    message("getting cached data")
    
    return(c)
  }
  
  data <- x$get()
  
  c <- solve(data)%%data
  
  x$setinv(c)
  
  c
}