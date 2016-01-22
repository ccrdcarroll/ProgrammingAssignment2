## A pair of functions that cache the inverse of a matrix.


## This function takes an input matrix and creates a special 
## cacheable "matrix" object with 2 functions to set and get 
## the matrix and 2 functions to set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
     if(class(x)!="matrix") warning("This is not a matrix.  There's no telling what might happen.")
     
     inv <- NULL
     
     #sets the value of the matrix
     set <-function(y){
          x <<- y
          inv <<-NULL
     }
     #returns the actual matrix
     get <- function() {x}
     
     #takes a matrix and sets it as the inverse of our cachable matrix
     setinv <- function(inverse) {inv <<- inverse}
     
     #returns the inverse matrix
     getinv <- function() {inv}
     
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function takes a special cacheable matrix and returns the
## cached inverse matrix if it exists, otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)){
          message("Retrieving Cached Inverse")
          return(inv)
     }
     
     data <- x$get()
     inv <- solve(data)
     x$setinv(inv)
     inv
}
