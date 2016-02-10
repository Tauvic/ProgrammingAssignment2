
## Cache matrix
## calculate inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  get<-function() x
  list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
}


## Write a short comment describing this function

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

#create matrix
x <- stats::rnorm(16)
dim(x) <- c(4,4)

#make cache
m<-makeCacheMatrix(x)

#solve
cacheSolve(m)
cacheSolve(m)

