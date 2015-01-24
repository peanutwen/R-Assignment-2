
## The following two functions compute the inverse of a matrix,
##   and save the result in the cache. Next time, if the input matrix
##   is the same as the previous one and the inverse has been saved,
##   the saved result will be used as output

## makeCacheMatrix creates a list from the inpur matrix x
## The difference between makeCacheMatrix and makeVector is that
##   the former one also saves the matrix that is used last time

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## "inv" saves the last inverse
  ## lastInput <- NULL   ## "lastInput" saves the last matrix
  
  ## Every time makeCacheMatrix is called, onle the matrix is saved in cache
  ## "inv" still saves the previous result
  ## The last input is also saved, in case the new input is not changed.
  set <- function(y){
    x <<- y
  }
  
  get <- function() x
  
  ## save the last matrix when save its inverse
  setInv <- function(z,inverse) {   
    inv <<- inverse
    lastInput <<- z
  }
  
  getInv <- function() inv
  
  getLastInput <- function() lastInput
  
  list(set = set, get = get, setInv = setInv, getInv = getInv, getLastInput = getLastInput)
  
}

## cacheSolve returns the inverse of the input matrix.
## If the new input equals to the previous input and an inverse has already
##   been computed, it simply returns the saved inverse without computing a new one.

cacheSolve <- function(x, ...) {
  
  i <- x$getInv()  ## Get the saved inverse
  l <- x$get()     ## Get the new input
  
  if(!is.null(i)) {  ## If there is an inverse saved
    
    old <- x$getLastInput()   ## Get the last input
    
    if(identical(old, l)) {   ## If the new input is not changed
      
      message("Getting Cached Data")
      return(i)
      
    }
    
    message("New Input. New Calculatioin.")
    
  }
  
  else { message("Empty Inverse Cache. New Calculation") }
  
  i <- solve(l)
  x$setInv(l,i)
  i
}