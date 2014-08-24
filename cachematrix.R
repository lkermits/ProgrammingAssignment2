#The pair of functions, makeCacheMatrix and cacheSolve, caches and returns
#an inverse matrix using the global assignment operator ‘<<-‘.

#The function, makeCacheMatrix creates a list which sets and gets the matrix, 
#then sets and gets the inverse matrix.
#The function, makeCacheMatrix, receives a matrix as the input variable.
#It is assumed that the matrix passed is invertible,
#and the number of rows and columns are the same, i.e. ‘a’.
#An empty inverse matrix ‘m’ is created.
#The ‘set’ function will create new internal global variables after 
#the functions are called once. It will reset the matrix and the inverse
#matrix being passed from makeCacheMatrix to cacheSolve.
#The function ‘get’ returns the value of the original matrix.
#The ‘setinverse’ and ‘getinverse’ functions are called and defined
#respectively when ‘setinverse’ is called by cacheSolve.
#A list object is returned.


#The function, cacheSolve, gets a previously calculated inverse or
#calculates it if there is none. The inverse is stored.
#If the matix ‘x’ has an NA in row1, column1, 
#then the inverse has not been calculated previously and
#the passed matrix is empty. Only one value of the matrix
#needs to be checked 
#assuming that all values passed into the function are valid
#as stated in the assignment instructions. 
#If the value of the passed matrix is other than NA,
#then the inverse matrix has been previously cached, and is returned.


makeCacheMatrix <- function(x = matrix()) {
    a <- nrow(x)
    m <- matrix(,a,a)
    set <- function(y) {
        x <<- y
        m <<- matrix(,a,a)
    }
    get <- function() x
    setinverse <- function(sol) m <<- sol
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

#The function, cacheSolve, gets a previously calculated inverse 
#matrix OR if there is none, it
#calculates the inverse with the function ‘solve’.
#The object is stored.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()   
    if(!is.na(m[1,1])) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}
