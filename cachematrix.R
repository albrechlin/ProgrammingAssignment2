## makeCacheMatrix caches both the input matrix and the inverse
## matrix as calculated by cacheSolve. It also has various
## closures that allow new input matrices to reset cached values
## and calling the input matrix or cached inverse matrix. Lastly,
## makeCacheMatrix returns a list that allows the closures to be
## subset in subsequent R code.
## cacheSolve either calls a cached inverse matrix from 
## makeCacheMatrix or calculates the inverse matrix and caches it
## to makeCacheMatrix.

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize vector for inverse matrix
  im <- NULL
  ## allows resetting of im for a new matrix input
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  ## calls input matrix
  get <- function() x
  ## caches inverse matrix into im vector
  setinverse <- function(inverse){
    im <<- inverse
  }
  ## calls cached inverse matrix
  getinverse <- function() im
  ## Setting a list allows the functions to be called as a subset
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated
## and the matrix has not changed, then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## check for cached inverse matrix
  im<-x$getinverse()
  if(!is.null(im)){
    ## inverse matrix cache retrieved
    message("getting cached data")
    ## return exits function
    return(im)
  }
  ## no cached matrix, calculates inverse matrix and caches it in
  ## makeCacheMatrix
  message("calculating inverse matrix")
  matrix<-x$get()
  im <- solve(matrix)
  x$setinverse(im)
  im
}
