## when called the first time, makeCacheMatrix sets the value 
## of matrix 'x' and initializes at null its inverse.
## cachesolve function returns the inverse of a matrix "x", 
## looking first at the cache and, if it is empty, 
## computes it and sets the inverse at the cache


## makeCacheMatrix returns a list of four functions (set, get, ... 
## ... setInverse and getInverse)
## The function starts by deleting whatever is in the cache
## Then sets the value of the new matrix at 'x' and sets the cache at NULL
## The set is done through the '<<-' operator, which meants that ...
## ... the set values are going to be found in the parent environment ...
## and thus accessible for the other functions
## the three other funcions are defined

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL		# cached mean
        set <- function(y) {
                x <<- y
                m <<- NULL	# invalidate cache
        }

        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cachesolve function returns the inverse of a matrix "x".
## First looks for if such inverse exists in the "cache"
## If the inverse is in the chache returns that matrix
## If not, gets the matrix, computes the inverse and sets the inverse in the cache 
## Finally, returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()		#gets the inverse from the cache
        if(!is.null(m)) {		 
                message("getting cached data")
                return(m)
        }
        data <- x$get()		#gets the matrix
        m <- solve(data)	#computes the inverse
        x$setInverse(m)		#sets the inverse in the cache
        m
}
