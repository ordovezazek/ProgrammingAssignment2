
## function to cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<-inverse
    getInverse <- function() i
    list(get = get, set = set,
         setInverse = setInverse,
         getInverse = getInverse)
}


## solving for the inverse manipulating the cached matrix above

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    i
}
