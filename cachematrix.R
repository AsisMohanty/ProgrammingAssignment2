####################################
##
## One function (with sub-functions) to set up caching for an 
## invertible matrix and its inverse, and one function to retrieve from cache
## or calculate and cache the inverse the matrix inverse
##
## for Coursera rprog-010
## by ed2468
##
####################################

## makeCacheMatrix: 
## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  ##function accepts a matrix as input
  
  invs <- NULL ##create a variable to store the inverse of a matrix
  ##for this assignment, we assume the matrix is ivertible
  ##so no need to check for invertibility
  
  set <- function(y) { ## set is a new function that caches the matrix and 
    ##its inverse to the parent environment
    x <<- y
    invs <<- NULL 
  }
  
  get <- function() x  ##get retrieves the matrix
  
  setinvs <- function(solve) invs <<- solve  ##uses solve to calculate the 
  ## inverse of the matrix
  
  getinvs <- function() invs ##retrieves the inverse of the matrix from 
  ##the parent environment
  
  ##uses list to output the four sub-functions from the main function
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ##main input is ivertible matrix
  ##can call sub-functions defined in makeCacheMatrix.R
  
  invs <- x$getinvs() ##calls getinvs function to retrieve the inverse of the matrix
  ##from the parent environment
  if(!is.null(invs)) {  ##if inverse has been been calculated already
    ##invs has been changed from NULL the the inverse matrix
    ##and the message and inverse are printed 
    message("getting cached data")
    return(invs)
  }
  data <- x$get()  ##if the inverse is not already stored, the matrix is retrieved
  invs <- solve(data, ...) ##the inverse is calculated
  x$setinvs(invs)  ##the inverse is cached
  invs ##the inverse is printed
}
