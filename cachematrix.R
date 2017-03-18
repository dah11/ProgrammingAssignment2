##the following functions calculate and cache the inverse of an
##invertible matrix


##mackeCacheMatrix creates matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #set our inverse to null when matrix object created
  myInvr <- NULL
  
  #function to set my matrix
  setMe <- function(newMatrix) {
    x <<- newMatrix
    #each time matrix is changed reset the inverse to null
    myInvr <<- NULL
  }
  
  #function to get my matrix
  getMe <- function() {
    return(x)
  }
  
  #function to set my matrix inverse
  setInvr <- function(newInvr) {
    myInvr <<- newInvr
  }
  
  #function to get my matrix inverse
  getInvr <- function() {
    return(myInvr)
  }
  
  #make function stuff available
  list(set = setMe, get = getMe,setInvr = setInvr,getInvr = getInvr)
}


##cacheSolve calculates the inverse of matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the current inverse
  myInvr <- x$getInvr()
  
  #if myInvr not null then has been calculated and should be grabbed from cache
  if(!is.null(myInvr)) {
    message('grabbing inverse from cache')
    return(myInvr)
  }
  
  #if myInvr is null, get matrix and calculate inverse
  data <- x$get()
  message('calculating inverse')  
  myInvr <- solve(data,...)
  x$setInvr(myInvr)
  myInvr
}
