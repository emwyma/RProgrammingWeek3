## These functions are to demonstrate efficiency of caching data, instead of running it each time

## This function creates a vector, which stores a list of functions. 
## These functions will be called upon later 

makeCacheMatrix <- function(x = matrix()) {
        stored_minv<-NULL                ## Creating a empty space to store a matrix inverse if needed later
        set <- function(matrix_value) {  ## defines a set function to assign 
                x <<- matrix_value       ##value of matrix, another part of our vector
                stored_minv <<- NULL     ##if there is a new matrix, reset the inverse to NULL, because we haven't calculated it yet 
        }
        get <- function () x                     ## This function just returns the matrix argument

        setinverse <- function(inverse) stored_minv <<- inverse ## assigns value of stored_minv in parent environment
        getinverse <- function () stored_minv      ##gets the value of the stored matrix inverse where called
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
        ## Creating a list is needed to refer to the functions with the $ operator later, it allows us to treat thed data as table
}
## The function below is the one that either CALLS the stored matrix inverse, OR calculates a new matrix inverse for us
## This is the 'working' part of the function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        stored_minv <- x$getinverse()  ## this calls the matrix inverse 
        if(!is.null(stored_minv)){     ## if the variable is NOT NULL, we return the matrix inverse below 
                message("getting cached matrix inverse data")
                return(stored_minv)     
        }
        data <- x$get()                ## if not stored we assign data the get function from above 
        stored_minv <- solve(data,...) ## we use the solve function given, to solve for the matrix inverse then
        x$setinverse(stored_minv)      ## here we set the NEW solved for matrix inverse 
        stored_minv                    ## and simply return the matrix inverse
}

