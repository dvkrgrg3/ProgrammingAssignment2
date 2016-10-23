## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

##The function below takes a matrix object as it's argument which is stored in 
##the variable x.The variable m stores the inverse of the input matrix and it's 
##value is initialised to NULL.The function returns a list which stores the value
##of the matrix as well as it's inverse as the elements of the list .It also has 
##functions which are used to set the value of the matrix , retireve it's value, set the 
##value of the inverse of the matrix as well as to retrieve it's value 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This functions takes cachematrix object as it's argument and returns 
## the stored inverse matrix. However if the stored value is NULL
##it calculates the inverse of the matrix using the solve function of R ,stores
## the value in the input cached matrix object and returns the inverse matrix 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
