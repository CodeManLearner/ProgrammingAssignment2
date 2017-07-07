## First function takes a square matrix and returns a list of functions to both set and get the matrix and inverse
## This list is then used as the input to the next function so the use of <-- to write to write outside the environment is present

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<-function(y){
    x<<-y #<-- has been used to write to the outside environment
    i<<-NULL #<-- has been used to write to the outside environment
  }
  get <-function() x
  seti<-function(inverse) i<<-inverse #<-- has been used to write to the outside environment
  geti<-function() i
  list(set=set, get=get,
       seti=seti,
       geti=geti)
}


## Second funtion takes the output of the first as mentioned above and returns the invers of the matrix first entered to the first function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$geti()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  
  x$seti(i)
  
  return(i)
}