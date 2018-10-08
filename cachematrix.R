# This pair of functions will allow you to solve a matrix, but more importantly it will cache the result
# after the first solve so that in every subsequent request to solve the same matrix, it will call upon the
# cached result, rather than solve it again

# The makeCacheMatrix function takes whichever matrix you wish to solve for it's inverse and 
# prepares it for solving by the cacheSolve function. m is where the result is cached, set replaces 
# whatever previous matrix you had asked to solve with a new one and flushes the cache. the mutator and accessor 
# functions are defined at the end

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve will check to see if the matrix has already been solved and return that answer if it has.
# If not, it will solve the matrix and store the result for future use if you call on it to solve the same
# matrix again

cacheSolve <- function(x, ...) {
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
