## Put comments here that give an overall description of what your
## functions do

#The two functions below are used to create a special object that 
#stores a matirx variable and cache's its inverse 

## Write a short comment describing this function

#The first function, makeCacheMatrix creates a special object, which is a list
#containing a function to 
#1.set the value of the matrix 
#2.get the value of the matrix 
#3.set the inverse of the matrix 
#4.get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
      i<-NULL
      set<-function(y){
            x<<-y
            i<<-NULL
      }
      get<-function() x
      setinverse<-function(inverse) i<<-inverse
      getinverse<-function() i
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function

#The second function calculates the inverse of the matrix created in the first function.
#It first checks to see if the inverse has already been calculated. If so , it gets the 
#inverse from the cache and skips the computation. Otherwise, it calculates the inverse 
#of the matrix and sets the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
      i<-x$getinverse()
      if(!is.null(i)){
             message("getting the cached data")
             return(i)
      }
      data<-x$get()
      i<-solve(data,...)
      x$setinverse(i)
      i
      ## Return a matrix that is the inverse of 'x'
}
