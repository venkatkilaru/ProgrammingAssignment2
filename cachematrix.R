## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<- NULL
  }
  get<- function(){x}
  
  
  getInverse<- function(){inv}
  setInverse<- function(inverse){
    inv<<- inverse
  }
  
  list(get=get,set=set,getInverse=getInverse,setInverse=setInverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv<- x$getInverse()
  if(!is.null(inv))
  {
    message("inverse already exists")
    return(inv)
  }
  
  mat<- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}
