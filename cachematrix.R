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
        minv<- NULL
        set<- function(y) {
              x<<- y
              minv<<- NULL
        }
        get<- function() x
        setinverse<- function(inverse) minv <<- inverse
        getinverse<- function() minv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## THis test to see if the matrix is in cache, and if so returns the cache.  If not, it calculates the inverse.
cacheSolve<- function(x, ...) {
        minv<- NULL
        minv<- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data.")
                return(minv)
        }
        data<- x$get()
        minv<- solve(data, ...)
        x$setinverse(minv)
        minv
}