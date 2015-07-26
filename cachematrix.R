# makeCacheMatrix: This function creates a list containing the below functions 
# 1. setMatrix()      assigns the value of a matrix
# 2. getMatrix()      retrieves the value of a matrix assigned
# 3. setInverse()     assigns the inverse of a matrix
# 4. getInverse()     retrieves the inverse of a matrix assigned
# This function receives square invertible matrix as input and performs one of the above 
# actions based on the function called.
# Assumption is that the received matrix is invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # initialize the value (inverse matrix) to NULL
        i <- NULL
        
        # cache the square invertible matrix received
        setMatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # returns the cached square invertible matrix
        getMatrix <- function() x
        
        # cache the inverse of the square invertible matrix passed a actual argument
        setInverse <- function(inverse) i <<- inverse
        
        # returns the cached inverse matrix
        getInverse <- function() i
        
        # Create a list with all the functions so that they can be utilized via the list names.
        list(setmat = setMatrix, getmat = getMatrix, setinv = setInverse, getinv = getInverse)
}

# cacheSolve(): This function receives a square invertible matrix and verifies if the Inverse of the 
# given matrix is already cached. If the cached inverse matrix is already available then it is returned.
# If the cache data is not found then the inverse of the matrix is calculated, 
# also caching it further usage

cacheSolve <- function(x, ...) {
        
        # Assign the inverse matrix already cached to 'inv'. If not cached, receives NULL 
        inv <- x$getinv()
        
        # Validates the returned value so that cached data can be returned.
        if(!is.null(inv)) {
                message("Using the Cached data")
                return(inv)
        }
        
        # As the inverse of the matrix is not cached, the input matrix is assigned to 'datamat'
        datamat <- x$getmat()
        
        #Inverse of the square matrix is assigned to 'inv'
        inv <- solve(datamat)
        
        # Calculated inverse matrix is cached calling the setinv() function for further usage.
        x$setinv(inv)
        inv
}