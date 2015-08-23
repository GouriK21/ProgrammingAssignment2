## This function returns a list of functions which does following operations on a matrix
## (a) set the value of the matrix 
## (b) get the value of the matrix 
## (c) set the value of inverse of the matrix 
## (d) get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix())
	{
	invrs<-NULL
	set<-function(y)
		{
		x<<-y
		invrs<-NULL
		}
	get<-function() 
		{
		x
		}
	setinverse<-function(inverse) 
		{
		invrs<<-inverse
		}
	getinverse<-function()
		{
		invrs
		}
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)	
	}

## This function solves for matrix inversion assuming the matrix is invertible (square matrix).
## It basically first check in cache if the matrix inverse asked for exists or not.
## If exists, the function fetches and returns the inverse matrix
## Otherwise, it calculates the inverse of the matrix, stores in cache and returns inverse.

cacheSolve <- function(x, ...) 
	{
	## Return a matrix that is the inverse of 'x'
	invrs<-x$getinverse()
	if(!is.null(invrs))
		{
		message("fetching chached data...")
		return(invrs)
		}
	data<-x$get()
	invrs<-solve(data)
	x$setinverse(invrs)
	invrs
	}
## End of function!