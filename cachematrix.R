##  The code implements the <<- operator which can be used to assign a value to an object 
# in an environment that is different from the current environment. 
# Below are two functions that are used to create a special "matrix" object, stores its value 
# and calculate and cache's its inverse.

## makeCacheMatrix function do the following:
# 1- set the value of the matrix
# 2- get the value of the matrix
# 3- set the value of the inverse matrix
# 4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # clear the variable
    imat <- NULL
    # create the function set to set the matrix value
    set <- function(y){
        # save the matrix value
        mat <<- y
        imat <<- NULL
    }
    # create the get function to get the matrix value
    get <- function() mat
    # create the setinv function to set the inverse matrix
    setinv <- function(inv) imat <<- inv
    # create the getinv function to get the inverse matrix
    getinv <- function() imat
    # create a list of atomic vectors to access elements of the matrix object
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}

## cacheSolve function do the following:
# it receives the matrix object as argument. Then it retrieves the inverse matrix value
# and if it is valid (cached), returns cached value instead of calculate it.
# If cached value is invalid, it calculates the inverse matrix, caches the value
# and returns the calculated value.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	imat <- x$getinv()
	# check if inverse matrix is valid
	if(!is.null(imat)){
	# is valid, then returns the value
		message("getting cached data")	
		return(imat)
	}
	# retrieve the matrix value
	data <- x$get()
	# calculate the inverse matrix
	imat <- solve(data, ...)
	# store the inverse matrix
	x$setinv(imat)
	# returns the inverse matrix
	imat
}
