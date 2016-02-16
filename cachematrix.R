# R Course | Programming assignment 2 : Lexical Scoping


makeCacheMatrix <- function(x = matrix()) {
        mtrx <- NULL
        set <- function(y) {
                x <<- y
                mtrx <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) mtrx <<- solve
        getSolve <- function() mtrx
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

cacheSolve <- function(x, ...) {
        mtrx <- x$getSolve()
        if(!is.null(mtrx)) {
                message("getting cached matrix")
                return(mtrx)
        }
        data <- x$get()
        mtrx <- solve(data, ...)
        x$setSolve(mtrx)
        mtrx
}

test_matrix <- makeCacheMatrix(matrix(c(1,2,3,4,5,4,3,2,1), nrow=3, ncol=3))

#  Returns original matrix
test_matrix$get()  

# Computes, caches, and returns new matrix inverse
cacheSolve(test_matrix)

# Returns matrix inverse (redundant output)
test_matrix$getSolve()

