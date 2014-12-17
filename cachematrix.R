## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #set the matrix to null as we are setting it here
  m <- NULL
  
  #assign incoming matrix to internal variable
  set <- function(y) {
    x <<- y
    #clear internal matrix variable
    m <<- NULL
  }
  
  #get the matrix
  get <- function() {
    x
  }
  
  #create function to set matrix/cache
  setinv <- function(cacheSolve) { 
    m <<- cacheSolve
  }
  
  #create function to get inverse
  getinv <- function() {
    m
  }
  
  #return a list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## `cacheSolve`: This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  #test to see if the matrix is null
  if(!is.null(m)) {
    #if the cache is not null then return the cached data
    message("getting cached data")
    return(m)
  }
  
  #get matrix data
  data <- x$get()
  #invert the matrix
  m <- solve(data) %*% data
  #set the inverse
  x$setinv(m)
  m
}

#test<-matrix(c(-1, -2, 1, 1), 2, 2)
#test
#makeCacheMatrix(test)


#cacheSolve(makeCacheMatrix(test))