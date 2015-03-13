## The functions written below will calculate the inverse of a matrix. The essence of it lies in using the 
## scoping rules in R to make use of a stored input of an object from one environment in another environment.
## The value of the object will remain intact even though we move out of the environment in which it was defined. 

## This function creates a list of 4 functions which contains the function that will create the matrix of which
## we need to calculate the inverse of and also contains the function that will calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
        set <- function(y = matrix()) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) i <<- solve
        getSolve <- function() i
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Using the liste functions inside the makeCacheMatrix function, this function just eveluates if the inverse
## we need is already present in the input from the previous function or not. If it is, it returns the 'cached' value
## in the input (this is where the scoping comes in use). If not, it calculates the inverse making use of the data it gets
## from one of the functions in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        i <- x$getSolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setSolve(i)
        i
}
