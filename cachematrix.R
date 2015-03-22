## Pair of functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Initialize the inverse matrix to NULL
  m<-NULL
  
  ## y is the matrix passed into makeCacheMatrix
  ##sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
  ## sets the value of y to NULL (provides a default if cacheSolve has not yet been used)
  
  set<-function(y){
      x<<-y
      m<<-NULL
  }
  
  ##Takes the inverse and sets it to the value m in the 
  ##makeCatcheMatrix frame and returns the m from that frame. 
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  

  ##lists out the values of the function in the MakeCaceMatrix frame
  list(set=set, get=get,     #creates a list to house the four functions
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  
## Return a matrix that is the inverse of 'x'
##Goes to the x environment   
  m<-x$getmatrix()      ## if an inverse has already been calculated this gets it
  if(!is.null(m)){      ## check to see if cacheSolve has been run before
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()          ##run the get function to get the value of the input matrix
  m<-solve(matrix, ...)    ##compute the value of the inverse of the input matrix
  x$setmatrix(m)           ##run the setmatrix function on the inverse to cache the inverse

  m                      ## return the inverse
}
