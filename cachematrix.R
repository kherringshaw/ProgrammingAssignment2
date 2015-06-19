## Functions compute the inverse of the special "matrix" returned by 
##makeCacheMatrix

## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse containing a function to:

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## The cacheSolve function calculates the inverse of the special "matrix" 
## created with the above function. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Else it calculates inverse.

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

## Function to check if matrix is cached by comparing
## time of first run to time of second run.

test = function(x){
  
  temp = makeCacheMatrix(x)
  
  start1 = Sys.time()
  cacheSolve(temp)
  time1 = Sys.time() - start1
  print(time1)
  
  start2 = Sys.time()
  cacheSolve(temp)
  time2 = Sys.time() - start2
  print(time2)
  
  if(time1>time2)
    print("matrix has been cached")
  else
    print("error")
}

## test output

## > r = rnorm(1000000)
## > y = matrix(r, nrow=1000, ncol=1000)
## > test(y)
## Time difference of 1.144065 secs
## getting cached data
## Time difference of 0.0009999275 secs
## [1] "matrix has been cached"

