## Assignment: Caching the Inverse of a Matrix:
## The aim is to write a pair of functions to creat a special "matrix" object 
## and cache its inverse.


## makeCacheMatrix: a function that creates a special "matrix" object 
##that can cache its inverse.


makeCacheMatrix <- function(x = matrix()){
	inver <- NULL

##set the matrix
	set <- function(y){
		x <<- y
		inver <<- NULL
}

##get the matrix
	get <- function()x

##set the inverse of the matrix
	setinver <- function(inverse) inver <<- inverse

##get the inverse of the matrix
	getinver <- function() inver

	list(set = set, get = get, setinver = setinver, getinver = getinver)
}



##cacheSolve: a function that computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the
##inverse from the cache.

cacheSolve <- function(x,...){

##return a matrix that is the inverse of 'x'
	inver <- x$getinver()

##return the inverse that has already been calculated
	if(!is.null(inver)){
		message("getting cached data")
		return(inver)
}

##get the matrix 
	matr <- x$get()

##compute the inverse
	inver <- solve(matr,...)
	x$setinver(inver)
	inver
}
	