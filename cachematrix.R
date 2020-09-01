## Put comments here that give an overall description of what your
## functions do

#If mean is a function whose code is already given in the assignment then solve is also a function like mean. 
#R does't understand any purpose functions, so, if I can replicate the same function makeVector 
#as makeCacheMatrix and cachemean as cacheSolve so I think the purpose is solved. 
#This is very simple to understand because computer will perform whatever the function does.
# mean() - it calculates means
# solve() - it will calculate inverse

#That's the analogy is what I can understand and applied it below. :-)

## Write a short comment describing this function

#Below are two functions that are used to create a special object that stores a square matrix 
#which can be inversed and cache's its inverse.

#The first function, makeCacheMatrix creates a special "matrix", which is really a data containing a 
#function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {  #This function creates a special "matrix" object that can cache its inverse.
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

#The below function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
#from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
#value of the inverse in the cache via the setinverse function.

#Moreover, the below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  #here the original inverse happens as we use the solve function
  x$setinverse(m)
  m
}





#Testing
set.seed(10)
mat1 <- matrix(rnorm(9), 3, 3) # We can test with any big square matrix as well
mat1_inv <-solve(mat1)

mat_new <- makeCacheMatrix(mat1)
mat_new_inv <- cacheSolve(mat_new)

mat1_inv == mat_new_inv

