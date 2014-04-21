## This is an example of how to exploit the scoping rules of the R language for the purpose of 
## caching potentially time-consuming computations. 
## The scoping rules can be manipulated to preserve state inside of an R object

#The first function, makeCacheMatrix creates a list containing a function to:
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the matrix inverse
#4.get the value of the matrix inverse

#The second function, cacheSolve  calculates the inverse of the special "matrix" wrapper 
#created with the makeCacheMatrix. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
#via the setinverse function.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
