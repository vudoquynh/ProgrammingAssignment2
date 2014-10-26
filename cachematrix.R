## Put comments here that give an overall description of what your
## functions do

## This function will cache the calculation of the inverse of a matrix
## It will create a special vector which is a list containing functions to 
## set the value of the matrix
## get the matrix, set the calculated value of the inverse of the matrix with the "solve" function
## and get the calculated value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	set <- function(y){
  		x <<- y
  		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set=set, get=get,
   		setmatrix=setmatrix,
   		getmatrix=getmatrix)
}


## The function cacheSolve will seek if there is already a calculation of the inverse matrix 
## by getting the value stored in the list element "getmatrix". If the content of the value is not NULL
## then the value stored has been cached and will be used, avoiding calculation again. 
## Otherwise the inverse of the matrix will be calculated and the reult stored in the list element "setmatrix"


# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
	m <- x$getmatrix() #
    	if(!is.null(m)){  # Testing if m is NULL or not , if not then cached data are used
      		message("getting cached data")
      		return(m)
    	}
    	matrix <- x$get() # if no cached data then get the data of x into matrix
    	m <- solve(matrix, ...) # calculate inverse of "x/matrix" into "m"
    	x$setmatrix(m) # puts the result of calculation in the list element "setmatrix"
    	m
}
