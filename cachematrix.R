## The functions 'makeCacheMatrix' and 'cacheSolve' work together, in combination, to store the inverse of a matrix in cache.

# The 'makeCacheMatrix' function returns an object (a list of functions) that stores the special matrix and its inverse
# List of functions:
## get - returns the special matrix
## set - sets the value of the special matrix
## getInverse - returns the inverse of the special matrix
## setInverse - sets the value of the inverse of the special matrix.
#

makeCacheMatrix <- function(x = matrix())
{
	## Initialize the matrix inverse to NULL. NULL value indicates that the inverse has not been computed.
	mat_inv <- NULL

	get <- function() x

	set <- function(data = matrix())
	{
		## If the 'already existing' matrix and the 'new' matrix are not equal (in all aspects), 
		## set the value or else do nothing.
		
		if (!identical(x, data))
		{
			x <<- data
			mat_inv <<- NULL
			print("different")
		}
	}

	getInverse <- function() mat_inv

	setInverse <- function(data = NULL)
	{
		mat_inv <<- data
	}

	list (getMatrix = get, setMatrix = set, getInverse = getInverse, setInverse = setInverse)
}


## ---------------------------------------------------------------------------------------------------------

# The 'cacheSolve' function
## 1. Returns the inverse of the special matrix (if already computed)
## 2. Computes and returns the inverse of the special matrix if it is not yet computed.

cacheSolve <- function(x, ...) 
{
        ## Initialize the inverse to the current value from cache
	mat_inv <- x$getInverse()

	## If the value is null, compute inverse
	if (is.null(mat_inv))
	{
		mat_inv <- solve(x$getMatrix(), ...)
		x$setInverse(mat_inv)
	}

	mat_inv
}
