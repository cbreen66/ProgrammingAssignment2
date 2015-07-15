## Two functions that return the inverse matrix of "x"

## makeCacheMatrix () caches an inverse matrix using four nested functions

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmat <- function(solve) m<<- solve
  getmat <- function() m
  list(set=set, get=get, setmat=setmat, getmat=getmat)
}

## cacheSolve () computes the inverse of a matrix created by makeCacheMatrix () 
## Also checks cached data to retrieve previous calculations

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmat()
  if(!is.null(m)){
    return(m)
  }
  matrix<-x$get ()
  m<-solve(matrix, ...)
  x$setmat(m)
  m
}