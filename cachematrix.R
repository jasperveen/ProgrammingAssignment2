## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: return a list of functions to: 
## 1. Set the value of the matrix 
## 2. Get the value of the matrix 
## 3. Set the value of the inverse 
## 4. Get the value of the inverse 


makeCacheMatrix <- function(x = matrix()) {



  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
    
}


## Write a short comment describing this function

## cacheSolve: returns the inverse of the matrix. It first checks if 
## the inverse has already been computed. If so, it gets the result and skips the 
## computation. If not, it computes the inverse, sets the value in the cache via 
## setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
{
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
}
