## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a matrix object that can cache it's inverser
makeCacheMatrix <- function(x = matrix()) {
      ## set inverse to null withinthe makeCacheMatrix environment
      inv <- NULL
      ## write first function that creates the matrix and assigns it 
      ## the value y, passed as an argument, and resets inv to null
      ## within the set function environment
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      ## write the get function which will the matrix passed
      get <- function() x
      ## write the function that inverse the matrix and saves it in 
      ## cache
      setinv <- function(solve) inv <<- solve
      ## write the function that gets the calculated inverse
      getinv <- function() inv
      ## print out as a result the a list containing the four functions
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
## Write a function that gets the matrix inverse passed above and 
## from the cache
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ##assigned through the getinv function created above the 
      ## matrix's inverse to the variable inv
      inv <- x$getinv()
      ## check if the inverse has already been calculated
      if(!is.null(inv)) {
            ## write a message to confirm the data comes from the cache
            message("getting cached data")
            ## print the inverse
            return(inv)
      }
      ## if the inverse has not been calculated get the matrix using 
      ## function get defined above
      data <- x$get()
      ## calculate the inverse using the function solve
      inv <- solve(data, ...)
      ## save the inverse to the cache
      x$setinv(inv)
      ## printout the inverse
      inv
}
