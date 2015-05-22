## This R script will create an inverse of a matrix and store the inverse in cache.
## If the matrix, in cache is called, the cache will provide the output.
## If the matrix is not in cache, the new matrix inverse will be calculated and then cached.
## Function test patterns
## 3  5
## -7 2
## 5 6
## 7 8
## "getting cached data"
## 5 6
## 7 8

## This will inverse the matrix and cache the inverse
makeCacheMatrix <- function(x=matrix()) {
        minv<- NULL  ## Sets up empty placeholder
        set<- function(y) {
              x<<- y  ## Places the matrix into holder
              minv<<- NULL
        }
        get<- function() x
        setinverse<- function(inverse) minv <<- inverse ## Determines the inverse of matrix
        getinverse<- function() minv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)  ## Establishes the variables for matrix
}

## THis test to see if the matrix is in cache, and if so returns the cache.  If not, it calculates the inverse.
cacheSolve<- function(x, ...) {
        minv<- NULL
        minv<- x$getinverse()                      ## Brings reversed matrix into function
        if(!is.null(minv)) {                       ## If value is not null, then the inverse is cached
                message("getting cached data.")    ## Indicates the matrix was cached
                return(minv)                       ## Returns the inversed matrix
        }
        data<- x$get()                             ## Brings new matrix in
        minv<- solve(data, ...)         
        x$setinverse(minv)                         ## Inverts the matrix and sets the new inverse
        minv
}