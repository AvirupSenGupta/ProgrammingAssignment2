# I have wrote two functions 
# (1) makeCacheMatrix: This function creates a special "matrix" object
#      that can cache its inverse.
# (2) cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the inverse
# from the cache.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

makeCacheMatrix <- function(x = matrix()) 
{
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#This function does the following task
      # (1) set the value of the vector, 
      # (2) get the value of the vector, 
      # (3) set the inverse of the matrix and 
      # (4) get the inverse of the matyrix
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	  i <- NULL
        set <- function(y) 
        {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cacheSolve <- function(x, ...) 
{
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#This function does the following task
      # (1) gets the inverse matrix, 
      # (2) checks whether inverse matrix is not NULL, 
      # (3) if the inverse is not NULL, it returns the cached inverse matrix with a message
      # (4) if the inverse is NULL, it calculates and returns the inverse matrix 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        i <- x$getinverse()
        if(!is.null(i)) 
        {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# An small example of the use of the functions
mc <- makeCacheMatrix(matrix(c(4,2,6,7), ncol = 2))
mc$get()
cacheSolve(mc)
cacheSolve(mc)