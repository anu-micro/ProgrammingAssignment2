## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#we have two functions makeCacheMatrix and cacheSolve
#makeCacheMatrix: This function creates a special "matrix" object 
                  #that can cache its inverse.
#makeCacheMatrix consists of a list containing set, get, setinv, getinv

library(MASS) #library mass helps to calculate inverse for squared 
              #as well as non-squared matrix
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL #initialzing inverse as NULL
  set <- function(y){
                      x<<- y
                      inv <<- NULL
                    }
  get <- function() {x} #function to get matrix x
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function() {
                            inver<-ginv(x)#ginv function is include in the
                                          #MASS library to calculate inverse of 
                                          #a matrix(squared/non-squared)
                            inver%*%x #%*%multiplies two matrices
                            }
  list(set = set, get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}

## Write a short comment describing this function
#cacheSolve: This function computes the inverse of the special "matrix" 
              #returned by makeCacheMatrix above.
#this function is used to cache the data
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){      #checking whether inverse is NULL i.e. 
                          #if inverse is calculated before or not
                    message("getting cached data")
                    return(inv)
                    }
  mat <-x$get()
  inv <- solve(mat, ...)#calculates inverse values
  x$setInverse(inv)
  inv ##returns the inverse of matrix x
}
