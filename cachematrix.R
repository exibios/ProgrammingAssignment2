## Put comments here that give an overall description of what your
## functions calculate the inverse of a matrix when not cached
## set function of the makeCacheMatrix instance helps you to test with several matrices

## Write a short comment describing this function
## This function will handle an object where we can get and set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  #initialize variables
  x_inv=NULL
  set<-function(w){
    #double << is needed because the environment handling these variables is the parent
    x<<-w
    x_inv<<-NULL
  }
  get<-function() x
  set_inv<-function(z) x_inv<<-z
  get_inv<-function() x_inv
  list(set=set,get=get,
       get_inv=get_inv,set_inv=set_inv)
}


## Write a short comment describing this function
## This function will use makeCacheMatrix as input to interact with an instance to know 
## if exists an inverse of a matrix calculated to retrieve it instead of calculating it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv<-x$get_inv()
  if (!is.null(x_inv)){
    print ("Retrieving data from cache!")
    return(x_inv)
  }
  dato<-x$get()
  print ("Calculating Inverse ... ")
  x_inv<-solve(dato)
  x$set_inv(x_inv)
  x_inv
}