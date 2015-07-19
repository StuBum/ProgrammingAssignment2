#define the first function which gives a list of functions to be used later on in cacheSolve
makeCacheMatrix <- function(x = matrix()) {#needs the input to be a matrix
     imatrix <- NULL #set inverse to null
     set <- function(y) { #ability to set the matrix x to y and wipe the current inverse 
          x <<- y   
          imatrix <<- NULL
     }
     get <- function() x # function to get original matrix
     setinverse <- function(inverse) imatrix <<- inverse #function to set the solved inverse to 'imatrix'
     getinverse <- function() imatrix #function to tell you the saved inverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse) #produce the output which is a list of functions
}

#define second function which gives you the inverse
cacheSolve <- function(z, ...) {#input the list of functions from makeCacheMatrix
     imatrix <- z$getinverse() #lookup current inverse matrix
     if(!is.null(imatrix)) { #check if current inverse actually exists or not
          message("getting cached data")
          return(imatrix) #if inverse exists, return it, otherwise...
     }
     omatrix <- z$get() #get the original matrix
     imatrix <- solve(omatrix) #find the inverse
     z$setinverse(imatrix) #set the inverse in makeCacheMatrix for when it's next checked
     #z$set(imatrix) can use this to invert calculated inverse
     #to see if it returns the original matrix as inverse of inverse of m = m
     imatrix #output the inverse
}