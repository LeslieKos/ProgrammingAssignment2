
## This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {

          m<-NULL
          set <-function(y) {
                  x <<- y ##y is local to this set function
                  m <<- NULL ## set the variable whose scope is outside of this function
            }
          get <- function() x ## Returns the variable x
          setinverse <-function(s) m <<- s  ## Take an inverse value and store it in the variable m
          getinverse <- function() m  ## return the variable m
          list(set = set, 
               get = get, 
               setinverse = setinverse,
               getinverse = getinverse)
  
  }


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         m <- x$getinverse()
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
        }
        data <- x$get()
         
        m <- solve(data, ...)
        x$setinverse(m)
        m
  
  
}


mymatrix <-matrix(rnorm(16),4,4)
mymatrix
a <- makeCacheMatrix(mymatrix)
cacheSolve(a)
