## This function allows the user to input numbers, number of rows and number of
## columns for a matrix. It will create the matrix and then alow the user to 
## find the inverse of the matrix

## makeCacheMatrix will take the list of values, number of rows and number of
## columns and create a matrix. It will also save that matrix and build the
## function that will save & search for the inverse values of the matrix 

makeCacheMatrix <- function(x,nrow,crow) {
        m <- NULL
        
        set <- function(y) {
                z <<- y
                m <<- NULL
        }
        
        z <- matrix(x,nrow,crow)
        get <- function() z
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
     
}


## the cacheSolve function will check to see if there is an inverse value for
## the matrix that was created; it will then pull the cached value if it exists
## if it does not exist it will create it and save it for future use.

cacheSolve <- function(x, ...) {
        
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                print(data)
                m <- solve(data)
                x$setinverse(m)
                m
}
