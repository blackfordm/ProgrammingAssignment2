## These two functions work together to allow the inverse of a matrix to be cached and
## stored in memory so that the the computational power required to solve the inverse of a matrix
## only has to be done once and the value can be called during subsequent calculations.

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

#Initialize variables
  m <- NULL
#Assign x & m to values of arguments located in the parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL                                #clear code from any previoius cache
  }

  get <- function() x
  setinverse <- function(solve) m <<- solve  #set the value of m (located in the parent environment) as the inverse of the matrix
  getinverse <- function() m                 #retrieve value for m
  list(set = set, get = get,                 #create the matrix (by returning a list)
       setinverse = setinverse,
       getinverse = getinverse)  
  
  
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` function. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should print "getting cached data" and retrieve the inverse from the cache.
## If the inverse has not been calculated, then calculate the inverse and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()                          #get the inverse from the list made by makeCachematrix()
  if(!is.null(m)) {                            #if value is not null
    message("getting cached data")             #print "getting cached data"
    return(m)                                  #return the cached data
  }
  data <- x$get()                              #if value of m is null (inverse of matrix has not been calculated and cached yet)
  m <- solve(data, ...)                        #get data, calculate inverse
  x$setinverse(m)                              #store inverse in cache
  m                                            #print inverse of matrix to console
}
