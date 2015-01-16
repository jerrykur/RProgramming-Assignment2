## cachematrix.R
#
#  There are 2 functions in this file.
#  makeCacheMatrix() - returns a cached matrix object
#  cacheSolve() - uses a cache matrix object to solve the matrix
#  Sample Usage:
#  1. Create a square solvable matrix
#    set.seed(1)
#    mat = matric(rnorm(100), 10, 10)
#    Note: This seed worked on my system.  If it does not work, the following square matric can be used:
#       mat = matrix(c(1,5,3,5,5,6,7,8,9), nrow=3, ncol=3)
#  2. Create a cache matrix object
#    cachedMat = makeCacheMatrix(mat)
#  3. Get the solution
#    sol1 = cacheSolve(cachedMat)
#  3a. get solution a second time.  This time the message "getting cached data" should be displayed
#    sol2 = cacheSolve(cachedMat)
# 4 (optional) Compare the two solutions, they should be identical (TRUE returned)
#    identical(sol1, sol2)
#

# Creates the cached matrix object from a matrix
makeCacheMatrix <- function(x = matrix()) {
  # inverse of matrix held at the "parent" level relative to functions
  invMatrix <- NULL
  
  #set the matrix
  set <- function(y) {
    # overwrite the value of the matrix
    x <<- y
    # clear the inverse of the matrix
    invMatrix <<- NULL
  }
  # get the matrix
  get <- function() {
    return (x)
  }
  
  # set the inverse stored at the parent level
  setinv <- function(inver) {
    invMatrix <<- inver
  } 
  
  # get the inverse
  getinv <- function() {
    return (invMatrix)
  } 
  
  # return the list of methods
  return (list(set = set, get = get,
       setinv = setinv,
       getinv = getinv))
  
}


##  Return the solution matrix for the square matrix held in the passed matrix cache object.
#   this code will used the cached solution (inverse) if it exists in the matrix cached object
# parameters:
#   x : matrix cache object
cacheSolve <- function(x, ...) {

  # get the cached matrix inverse, which is the solution
  invMatrix <- x$getinv()
  # if there is a non-null matrix inverse in the cache, return it and we are done
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  
  # if here then the inverse was not in the cache so we need to compute the inverse
  # 1. get the original matrix
  oMatrix <- x$get()
  # 2. compute the inverse (can be slow..., hence the cache)
  invMatrix <- solve(oMatrix)
  # 3. save the new computed inverse to the cache so we do not have to compute it again
  x$setinv(invMatrix)
  # 4. return the matrix inverse as the solution
  return (invMatrix)

}
