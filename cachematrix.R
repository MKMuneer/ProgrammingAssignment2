# Inversion of Matrix in R is usually time consuming, 
# and caching the inverse of a matrix can be beneficial 
# rather than computing it again and again.
# The following two functions are used to
# 1. Retrieve the inverse of a matrix if it has alreay been computed.
# 2. If the inverse of matrix has not been computed previously;
#    compute the inverse of the matrix, and
#    save the result in the cache for future function calling.

# Function "makeCacheMatrix()" creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix = function(x = matrix()) {
     inv = NULL
     set = function(y) {
          x <<- y
          inv <<- NULL
     }
     get = function() x
     setinverse = function(inverse) inv <<- inverse
     getinverse = function() inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Function "cacheSolve()" is written is such a way that it will
# return the inverse of the matrix.
# It will first check that if the inverse of the given matrix 
# has already been computed. If so, it'll retrieve
# the previous result from the cache via "getinverse()" function
# and skips the computation. If not, it will compute the inverse, 
# set the value in the cache via "setinverse()" function.

cacheSolve = function(x, ...) {
     inv = x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data.")
          return(inv)
     }
     data = x$get()
     inv = solve(data)
     x$setinverse(inv)
     inv
}

# For testing of the functions and code:

# > SAMPLE = c(3,0,2,2,0,-2,0,1,1)
# > x = matrix(SAMPLE, 3, 3, byrow = T)
# > m = makeCacheMatrix(x)


# > m$get()
# [,1] [,2] [,3]
# [1,]    3    0    2
# [2,]    2    0   -2
# [3,]    0    1    1


# No cache in the first function call:
# > cacheSolve(m)
# [,1] [,2] [,3]
# [1,]  0.2  0.2    0
# [2,] -0.2  0.3    1
# [3,]  0.2 -0.3    0


# Retrieving from the cache in the second function call:
# > cacheSolve(m)
# getting cached data.
# [,1] [,2] [,3]
# [1,]  0.2  0.2    0
# [2,] -0.2  0.3    1
# [3,]  0.2 -0.3    0