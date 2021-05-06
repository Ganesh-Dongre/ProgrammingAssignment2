## Put comments here that give an overall description of what your
## functions do

# Basically I have created two functions makeCacheMatrix() and cacheSolve() that cache the inverse of a matrix
# makeCacheMatrix() is a function that generates a special "matrix" object for the input that can cache its inverse (which is an invertible square matrix)
# library(Mass) is used to calculate for both non squared and squared matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL #initializing as null
set <- function(y){
  x<<-y
  inv<<-NULL
}

get<-function()x    #function to get matrix x
setinv<-function(inverse)inv<<-inverse
getinv<-function(){
  inver<-ginv(x)
  inver%*%x          #function to obtain inverse o the matrix
  
}
list(set=set, get=get, setinv=setinv,getinv=getinv)
}


##The inverse of the unique "matrix" returned by makeCacheMatrix() above is computed by cacheSolve(). The cachesolve can retrieve the inverse from the cache if the inverse has already been computed (and the matrix has not changed).

cacheSolve <- function(x, ...) {
       inv<-x$getinv()
       if(!is.null(inv)){
         message("Getting cached data!")
         return(inv)
       }
       data<-x$get()
       inv<-solve(data, ...)          #calculates the inverse value
       x$setinv(inv)
       inv
}
#Run
f <- makeCacheMatrix(matrix(1:8,2,4))
f$getinv()
cacheSolve(f)

