## Put comments here that give an overall description of what your
## functions do

## asignment2.R
##
## makeCacheMatrix - Takes a matrix as input and stores its inverse
## makeInverse - Returns  and caches the inverse of the matrix 

#Example:
# matrix 3x3
## x <- matrix (c(1,1,0,1,0,1,0,1,0), ncol=3)

#inverse:
#> solve(x)
#[,1] [,2] [,3]
#[1,]    1    0   -1
#[2,]    0    0    1
#[3,]   -1    1    1

# h = makeCacheMatrix(x)
# cacheSolve (h)    #First time calculates inverse
# cacheSovle (h)    #Next time returns cached inverse


# function makeCacheMatrix
# returns: none
# Stores matrix original and inverse.
# Contains functions called by cacheSolved()
# - getOriginal() Return original matrix
# - setInverse (i) i is the inverse of the origial matrix
# - getInverse () Return cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    
    # inverse matrix cached. Null at start
    inverse  <- NULL
    
    # Returns original matrix
    getOriginal <- function () {x}
    
    #called by cacheSolve()) during first cacheSolve()
    setInverse <- function (i) {inverse <<- i}
    
    #return cached inverse
    getInverse <- function () {inverse}
    
    #List of internal functions
    list (getOriginal = getOriginal,
          setInverse = setInverse,
          getInverse = getInverse)               
}


# function cacheSolve(x)
# x is a makeCacheMatrix object
# Returns the inverse of the matrix, cached if is available
cacheSolve <- function(x, ...) {
    # get inverse in object x
    i <- x$getInverse()
    
    if (!is.null(i)) {
        #if inverse is cached
        message ("Getting cached data")
        # don't do anything
    } else {
        #if not cached, get original matrix
        original <- x$getOriginal()
        
        # invert it
        i <- solve(original)
        
        # store inverse in cache
        x$setInverse(i)        
    }
        
    # return inverse value
    return (i)
}


# Test a small matrix 3x3
test1 <- function () {
    message ("Creating matrix...")
    x <- matrix (c(1,1,0,1,0,1,0,1,0), ncol=3)
    h <- makeCacheMatrix(x)
    print(x)    
    message ("Getting inverse (1)...")
    tmp <- cacheSolve(h)
    print (tmp)
    message ("Getting inverse (2)...")
    tmp <- cacheSolve(h)
    print (tmp)
}

# Test a big matrix 1000x1000 (1 Sec with i5)
test2 <- function () {
    message ("Creating big matrix...")
    x <- matrix (rnorm(n=1000000), ncol=1000)
    h <- makeCacheMatrix(x)
    #print(x)    
    message ("Getting inverse (1)... slow")
    tmp <- cacheSolve(h)
    #print (tmp)
    message ("Getting inverse (2)... fast")
    tmp <- cacheSolve(h)
    invisible (tmp)
}
 
