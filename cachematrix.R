#THe following function creates a list. The "<<-" operator has been used in this function. 
#The four functions within this function create are to set and get the value f the matrix  
#and set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks if the inverse of the matrix is already there in the cache. If it is then it fetches  
# that inverse value.If not, then it calculates the inverse with the solve() function.

cacheSolve <- function(x, ...) {
  
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
  
}
