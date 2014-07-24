## This script contains two functions to demonstrate the advantage of the scoping rules of the
## R language and how they can be manipulated to preserve state inside of an R object

## function 'makeCacheMatrix' creates a special "matrix" object
## function 'cacheSolve' computes or retrive the inverse of a matrix

## Function 'makeCacheMatrix'
## This function mainly serves to store a square matrix and its inverse. It also provides four
## externally accesible methods to store and retrieve both original matrix and its inverse.
## Those methods are returned as a list  

makeCacheMatrix <- function(x = matrix())
{
	## Initialize to NULL where the inverted matrix will be stored

	i <- NULL

	## Define 4 functions (methods):

	## set: Replace the already stored original matrix (x) by a new one (y), so the already
	##	  cached value for the inverse matrix (i) must be initialized (to NULL) again
	## get: Return to the calling fuction the original matrix (x) (it will be inverted later)
	## setinverse: Store (cache) in (i) the inverse value passed to it (inverse)
	## getinverse: Return to the calling function the previously stored (cached) value (i)
	##		   Note that it will return NULL if called BEFORE 'setinverse'

		 set <- function(y) 		{ x <<- y; i <<- NULL; }
		 get <- function() 		{ x }
	setinverse <- function(inverse) 	{ i <<- inverse }
	getinverse <- function() 		{ i }

	## Return a list of above defined functions

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function 'cacheSolve'
## This function computes the inverse of the original matrix stored in the special "matrix" 
## object returned by 'makeCacheMatrix' if it is the first time you call it. If it is not the
## first time and you have not changed the original matrix, it will return the cached inverse value

cacheSolve <- function(x, ...) 
{
      ## Call function 'getinverse' to retrieve previously computed (and cached) inverse (if any)
	
	i <- x$getinverse()

	## If the result is no NULL, we are lucky and the result was cached, so we can simply
	## return it (also print a message indicating that)

	if(!is.null(i)) { message("getting cached data"); return(i) }

	## If the result is NULL, we are not lucky. So we'll have to:
	## 1. Retrieve the original matrix from the special "matrix" object calling 'get' function
	## 2. Compute the inverse of the original matrix calling R function 'solve'
	## 3. Cache the result (for sucessive calls to 'cacheSolve') calling 'setinverse' function 
 
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)

	## Finally, return the just computed inverse matrix

	i
}
