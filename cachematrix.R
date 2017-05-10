makeCacheMatrix <- function(x = matrix()) {
     #Declares 'inv' variable as NULL
     # this is the inverse matrix variable
     inv = NULL

     #This section sets up the functions that go into the returned list
     set = function(y) {
          # use `<<-` to make variable available in Global Environment
          x <<- y
          inv <<- NULL
     }
     get = function() x
     setinv = function(inverse) inv <<- inverse 
     getinv = function() inv
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
     inv = x$getinv()
    
     # Checks to see if the inverse has already been calculated
     if (!is.null(inv)){
          # get the inverse 'inv' from the cache 
          return(inv)
     }
     #calculates the inverse 
     ncmatrix = x$get()
     inv = solve(ncmatrix, ...)
    
     # sets the value of the inverse in the cache via the setinv function.
     x$setinv(inv)

     return(inv)
}