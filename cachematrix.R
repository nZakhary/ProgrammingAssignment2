## Put comments here that give an overall description of what your
## functions do
# first function is responsible for caching a matrix , retrieving the cached matrix
#			,caches the inverse of the matrix and retrieve the cached inverse of the matrix	
# the second 




## Write a short comment describing this function
# the function set : sets the value of the matrix
# the function get : gets the cached value of the matrix
# the function setMatrixInv : takes the calculated inverse of the function as an argument  
#								and caches it in a variable "inv"
#the function getMatrixInv : retrieves the cached matrix inverse 
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setMatrixInv <- function(inverse) inv <<- inverse
  getMatrixInv <- function() inv
  list(set = set, get = get,
       setMatrixInv = setMatrixInv,
       getMatrixInv = getMatrixInv
      )
}


## Write a short comment describing this function
# input : matrix x 
# steps of function : 1- the function gets the cached inverse 
#					  of the matrix and stores it in #the variable "inv"
#					  2-  the value of the variable "inv"  is checked if it's NULL 
#						  then this is the first time the inverse is computed 
#								so it continues the code
#						  else it returns the cached inverse
#					  3- get the cached matrix and store it in variable "data"
#					  4- calculate the inverse of the matrix and store it in "inv"
#					  5- cache the inverse of the matrix and return the value of the inverse 
# output : the cached inverse

cacheSolve <- function(x, ...) {
  inv <- x$getMatrixInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setMatrixInv(inv)
  inv
}

'''
temp_matrix = c(2,4,6,8)
dim(temp_matrix) = c(2,2)
my_matrix<- makeCacheMatrix(temp_matrix) 
my_inv <-cacheSolve(my_matrix)
print(my_inv)
'''
