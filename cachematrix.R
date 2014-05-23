## cachematrix.R creates a special matrix object that caches its inverse matrix.
## This allows the inverse to be used repeatedly while only being calculated once.

## Creates a special object that allows you to set the value of the matrix,
## get the value of the matrix, set the value of the inverse matrix and 
## get the value of the inverse matrix. Per the assignment instructions, it
## is assumed that the matrix supplied is invertable.

makeCacheMatrix <- function(x = matrix()) {
        
        M <- NULL               
        set <- function(y) {    
                x <<- y         
                M <<- NULL      
        }
        get <- function() x
        setINV <- function(solve) M <<- solve  
        getINV <- function() M
        list (set = set, get = get,            
              setINV = setINV,
              getINV = getINV)
}


## This function calculates the inverse of the matrix created in the function
## above. First, it determines if it has been previously cached. If it has, it
## returns the cached inverse matrix. If it has not been cached, it calculates
## the inverse matrix which will then be saved in cache for future use.

cacheSolve <- function(x, ...) {
                
        M <- x$getINV()
        if(!is.null(M)) {
                message("getting cached inverse matrix")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setINV(M)
        M
}

## The above functions can be tested with the following
## after sourcing the 2 functions -- (Thank you to Michael
## for his explanation in the forums)

# a <- makeCacheMatrix()                # initializes
# a                                     # shows that a is now a list of functions
# a$set(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))     # sets the matrix
# a$get()                                              # gets the matrix
# cacheSolve(a)                                        # calculates the inverse matrix
# cacheSolve(a)                                        # returns the cached inverse matrix
