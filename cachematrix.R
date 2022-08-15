## Computes the cached matrix
makeCacheMatrix <- function(x = matrix()) {
         
  inv <- NULL
  set <- function(y){
    
    x <<- y
    inv <<- NULL
  }
  get <- function(){return(x)}
  setinv <- function(y){
    inv <<- y
  }
  getinv <- function(){
    
    return(inv)
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## Finds the inverse of special matrix
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
  if(!is.null(inv)){
    
    message("returning cached value")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinv(inv)
}
