## The chachematrix.R contains two functions.They create,store and recall
## matrix as well as calculate and store the inverse of the matrix within the cache.

## The first funciton contains 4 sub-functions (set the matrix, get the matrix
## set the inverse matrix and get the inverse matrix) that allows user to be able to set, obtain
## calculate and store or call the inverse matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL ## Create an object named invert to store the inverse matrix.
  +  set <- function(y){ ##This function set and store the matrix in cache
    +    x <<- y  
    +    invert <<- NULL 
    +  }
  +  get <- function() x # This function get the matrix
  +  setInverse <- function(solve) invert <<- solve #solve and set the inverse matrix
  +  getInverse <- function() invert #get the inverse matrix
  +  list(set = set, get = get,
          +       setInverse = setInverse,
          +       getInverse = getInverse)  ## store eveything in a list containing set, get 
                                            ## getinverse and setinverse.
}


## This function calculated the inverse of the matrix. It screens to see if the matrix has been
## calculate before and store in the cache or not. If it does, return the value from cache.
## If not, calculate the inverse matrix and store it in the cache.

cacheSolve <- function(x, ...) { ## Take a matrix called x
  +  invert <- x$getInverse() ## call the matrix cache 
  +  if(!is.null(invert)){    ## If invert is not null meaning it has been calculated before                
    +    message("getting cached data") ## inform the user results can be obtained from cache
    +    return(invert)       ## It will find the value and return the calculated value.  
    +  }                      
                              ## If invert can not be found within the cache
  +  data <- x$get()          ## get the matrix from the makeCaCheMatrix function                   
  +  invert <- solve(data, ...) ## calculate the inverse of the matrix               
  +  x$setInverse(invert)     ## Store the caculated solution using setinverse in cache                    
  
}
