## Put comments here that give an overall description of what your
## functions do
# The functions makeCacheMatrix and cacheSolve allow to calculate and store the inverse of a matrix, such that when it is needed again, 
# it can just be simply retrieved instead of having to recalculate it.  

## Write a short comment describing this function
# the makeCacheMatrix function creates an object type list that stores
# the original input matrix and the cached variable, here the matrix inverse. It also stores the functions that will
# be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {     # Uses as argument a matrix
    inv <- NULL                                 # inv is the variable storing our matrix inverse and it is reset to NULL every time makeCacheMatrix is called
                                                # the next function allows to run the function with a new matrix
    set <- function(y) {                        # takes an input matrix
        x <<- y                                 # saves this input matrix
        inv <<- NULL                            # reset the matrix inverse to NULL
        
    }                                           # the 3 next functions will be used by cacheSolve () 
    get <- function() x                         # this returns the original matrix
    setsolve <- function(solve) inv <<- solve   # used by cacheSolve () when it is called for the 1st time. It stores the computed matrix inverse in the variable inv using superassignment
                                                # superassignment is used here because inv was created in the makeCacheMatrix function, ie outside the cacheSolve function environment
    getsolve <- function() inv                  # returns the cached value on subsequent accesses
    list(set = set, get = get,                  # this is the list of the internal functions defined in makeCacheMatrix and used by cacheSolve
         setsolve = setsolve,
         getsolve = getsolve)
}
}


## Write a short comment describing this function
# The function cacheSolve  will first check if the matrix inverse has been calculated.
# If not, it calculates it, stores it in the variable inv created by the call of 
# makeCacheMatrix and then returns it. If the inverse has been calculated
# then cacheSolve just fetches it and returns it.

cacheSolve <- function(x, ...) {       # uses as argument the object x created by makeCacheMatrix
    inv <- x$getsolve()                # uses the getsolve function that is stored inside x to retrieve the values of the inv variable
    if(!is.null(inv)) {                # if the inverse was already calculated, ie if x$getsolve() returned non-NULL value
        message("getting cached data") # returns this message to the console 
        return(inv)                    # ... and returns it
    }                                  # if x$getsolve() returned a NULL value, the next piece of code will be run 
    data <- x$get()                    # first gets the original matrix
    inv <- solve(data, ...)            # then calculates its inverse
    x$setsolve(inv)                    # then sores it in inv (in other words caches it). This will set (change) the value of inv to the newly calculated matrix inverse
    inv                                # finally returns the matrix inverse
}
    
    
}
## Return a matrix that is the inverse of 'x'