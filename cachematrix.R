## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function to set matrix to cache.
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL 
  sf<-function(y){
    x<<-y
    i<<-NULL
  } #Set Y to function
  g<-function()x #set x to function
  setinv<-function(inv)i<<-inv #Solve for inverse of matrix
  getinv<-function()i
  list(sf=sf,
       g=g,
       setinv=setinv,
       getinv=getinv) #list of parameters
}

#Function to solve for inverse matrix and return to the operator.
cacheSolve <- function(x, ...) {
  i<-x$getinv()
  if (!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  data<-x$g()
  i<-solve(data,...)
  x$setinv(i)
  i
}

#Test Case
B<-matrix(c(1,2,3,4),2,2)
B1<-makeCacheMatrix(B)
cacheSolve(B1)
