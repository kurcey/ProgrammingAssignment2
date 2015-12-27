## These functions are for R Programing Assignment number 2
## Computing the inverse of a square matrix and demonstrate 
## Lexical Scoping


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ##Sub function 'set' enables usere to store matrix variable x and clear matrix inversion variable m
  set <- function(y) {   
    x <<- y
    m <<- NULL
  }
  
  ##Sub function 'get' enables usere to retreive matrix variable x
  get <- function() x    
  
  ##Sub function 'setMatrixInv' enables usere to set matrix inversion variable m
  setMatrixInv <- function(MatrixInv) m <<- MatrixInv 
  
  ##Sub function 'getMatrixInv' enables usere to get matrix inversion variable m
  getMatrixInv <- function() m
  
  list(set = set, get = get,
       setMatrixInv  = setMatrixInv,
       getMatrixInv = getMatrixInv)  
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInv()
  
  ## if the matrix inversion allread exist return the inversion without additional computations and end function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## calculates inversion of matrix stored stores that result via Lexical Scoping
  ## and returns the restults of that inversion to the screen
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInv(m)
  m
}
