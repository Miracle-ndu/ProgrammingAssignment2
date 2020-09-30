## This functions create a special "matrix" object that can cache its inverse 
## and computes/retrieves the inverse of the special "matrix"

## x is the matrix that the user will input in the console, this function 
## creates a vector that is a list with a function that sets and gets the value 
## of the vector and the "matrix" inverse

makeCacheMatrix <- function(x = matrix()) {
   invx <- NULL
   set <- function(y){
       x <<- y
       invx <<- NULL
}
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() invx
   list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created by the 
## MakeCacheMatrix function above. If the inverse has been already calculated 
## (and the matrix has not changed), then it should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
       invx <- x$getinverse()
       if (!is.null(invx)){
         message("getting cached data")
         return(invx)
       }
      matx <- x$get()
      invx <- solve(matx, ...)
      x$setinverse(invx)
      invx
}