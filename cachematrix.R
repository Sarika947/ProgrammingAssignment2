## Rather than repetedly computing the inverse of a matrix over and over,
## ie. in a loop, it may be benificial to cache the matrix  and recall the values 
## rather than repetedly recalculate them

## This function creates a matrix object whose inverse is then cached

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}

## If the matrix remains unchanged, and the inverse has already been calculated
## this function will simply return the previously calculated value

cacheSolve <- function(x, ...) {
   inv<-x$getinverse()
   if(!is.null(inv)){
     message("getting data from cache")
     return(inv)
   }
   matrix<-x$get()
   inv<-solve(matrix,...)
   x$setinverse(inv)
   inv
}
