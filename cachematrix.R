##   Matrix inversion is usually a costly computation and there may be some benefit 
##   to caching the inverse of a matrix rather than compute it repeatedly. 
##
##   The following two functions are used to calculate the inverse of a matrix
##   using solve(). It takes advantage of R's scoping rules to cache the result

##   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { 
     # sets the inverse equal to NULL     
     inv <- NULL                                  
     
     # sets the value of the matrix. inv is re-set to NULL incase x has been redefined
     set <- function(y){
          x <<- y 
          inv <<- NULL 
     }
     
     #get the value of the matrix
     get <- function() x
     
     #set the inverse of the matrix
     setInverse <- function(solve) inv <<- solve
     
     #return the inverse of the matrix
     getInverse <- function() inv

     #return the list
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##   cacheSolve: This function computes the inverse of the special "matrix" returned by
##   the makeCacheMatrix function. If the inverse has already been calculated, and the
##   matrix has not changed), then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     
     # Retrive the value for the inverse
     inv <- x$getInverse()
     
     # If inverse exists then pull from the cache
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)       
     }
     
     # If the inverse doesn't exist then calculate the inverse using the solve() function
     message("newly calculating data")
     data <- x$get()                         #place the data in data
     inv <- solve(data, ...)                 #calculate the inverse
     x$setInverse(inv)                       #set the inverse value 
     inv                                     #return the inverse value
}
     