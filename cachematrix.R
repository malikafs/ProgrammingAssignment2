#Functions that cash the inverse of the matrix

#create a list containing of functions:
#set the value of function
#get the value of function
#set the value of inverse
#get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
      mx <- NULL
      set <- function(y) {
      x <<- y
      mx <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) mx <<- solve    
  getinverse <- function() mx
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}

#Calculate the inverse of matrix set above
#If it has already been done before - we get the message "getting cached data"
#and gets the inverse from the cash (avoit the calculations)
#Otherwise, it calculate the inverse and set the value via 'setinverse' function
cacheSolve <- function(x, ...) {
   mx <- x$getinverse()
      if(!is.null(mx)) {
         message("getting cached data")
      return(mx)
  }
  
      data <- x$get()
      mx <- solve(data,...)
      x$setinverse(mx)
      mx
}
