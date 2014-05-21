## The functions are designed to calculate the inverse of a  matrix given by 
## the user and store it in the memory in order to save time and do not repeat 
## the computation whenever the inverse matrix will be needed.

## "makeCacheMatrix" function is in fact a list of 4 functions: 
## "set": sets the values of the matrix 
## "get": displays the matrix
## "setInverse": puts the calculated inverse matrix into the cache
## "getInverse": returns the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y=matrix()){
    x<<-y     # operator '<<-' enables to define the matrix in the other 
              # enviroment than local
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) m<<-inverse
  getInverse<-function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## "cacheSolve" returns a matrix that is the inverse of matrix 'y' set
## in function "makeCacheMatrix". If the calculations have been already done 
## and there is an inverse matrix which is cached, the function does not
## repeat the calculations but gets the calculated matrix from the cache and
## returns it.

cacheSolve <- function(x, ...) {
# m is the matrix from the cache, if there is none m=NULL
  m<-x$getInverse() 
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }

  a<-x$get()  
  inverse<-solve(a)     # function solve(data) finds the inverse matrix
  x$setInverse(inverse) # the calculated inverse matrix
                        # is passed to the cache
  inverse
}
