##  Author: Muhammad Anwar Sabri 
##  Date written: April 25, 2015
##  Assignment # 2
##  Course: R Programming 
##  Coursera.org -- Data Science Specialization Course

##  This script uses the concept of Dynamic Programming (DP) by caching 
##  intermediate computation results for future use.
##  This script contains the following TWO(2) functions to create a 
##  special "matrix" object that stores a matrix and caches its inverse:
##  
##  (1) makeCacheMatrix() -- to cache the results (inverse) of a matrix
##  (2) cacheSolve() -- to calculate the inverse of a matrix, and
##  
##  Since calculating the inverse of a matrix is a computation-intensive 
##  and time-consuming operation, especially, for a large matrix, it makes
##  sense to "cache" (or store) the result of a matrix, and then just use 
##  the cached inverse matrix for future calls, if the matrix is unchanged.
##  

##  function makeCacheMatrix() -- takes an invertible square matrix and 
##  returns a list of functions to:
##  (1) set the values of the matrix 
##  (2) get the values of the matrix
##  (3) set the values of the inverse of the matrix 
##  (4) get the values of the inverse of the matrix
 
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inverseMatrix) m <<- inverseMatrix

	getInverse <- function() m

	list( set = set, get = get, 
		setInverse = setInverse, getInverse = getInverse)
}


##  function cacheSolve() -- takes the output of the makeCacheMatrix()
##  and calculates and returns the inverse of the special "matrix" by
##  first checking the cache to see if the inverse has already been 
##  calculated. If the matrix whose cached inverse matrix has not been 
##  changed, then it skips recalculating the inverse and simply returns
##  the cached inverse matrix using the getInverse function. Otherwise, 
##  it calculates the inverse of the matrix and sets the new inverse in
##  the cache before returning the inverse matrix m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	m <- x$getInverse()

	## check to see if the inverse of the matrix is already cached
	if(!is.null(m)) { # then, return the inverse from the cache
		message("getting the inverse from the cache...")
		return(m)
	}
	## else, calculate the inverse
	data <- x$get()
	m <- solve(data, ...)

	## set the value of the inverse matrix in the cache by calling the
	## setInverse() function
	x$setInverse(m)

	## return the newly calculated inverse
	return(m)
}
