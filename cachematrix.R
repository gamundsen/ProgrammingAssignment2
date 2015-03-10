## THis is a pair of functions that defines a matrix class 
## that allows you to calculate, cache and retreive it's inverse

## This function defines the new matrix class and it's functions

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(m) {
        x<<-m
        inv<<-NULL
    }
    get <- function() x
    setInv <- function(inverse) inv<<-inverse
    getInv <- function() inv
    
    list(set=set, get=get, setInv=setInv, getInv=getInv)

}


## Returns a matrix that is the inverse of 'x', 
##calculating the inverse if is hasn't been solved for
## or if the matrix has changed since it was last solved for

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
    
}
