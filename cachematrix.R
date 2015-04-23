# These functions allow for creating a CacheMatrix list object which contains a
# matrix and is also capable of caching its inverse in order to prevent having
# to perform that expensive operation multiple times.

# Create a new CacheMatrix list object.
#
# Args:
#   x: The initial matrix that this object should represent.
#
# Returns:
#   A CacheMatrix list object representing the parameter x.

makeCacheMatrix <- function(x = matrix()) {
    cached.inverse <- NULL

    # Set the internal matrix for this CacheMatrix to a new value. This will
    # clear the inverse cache as a result.
    Set <- function(y) {
        x <<- y
        cached.inverse <<- NULL
    }

    # Get the matrix that this CacheMatrix represents.
    Get <- function() x

    # Set the cached inverse of this matrix. Meant only to be called by
    # cacheSolve.
    SetInverse <- function(inverse) cached.inverse <<- inverse

    # Get the cached inverse of this matrix or NULL if no inverse has been
    # cached. Meant only to be called by cacheSolve.
    GetInverse <- function() cached.inverse

    # All functions are public because some implementation must be kept in
    # cacheSolve as described by assignment instructions
    list(Set = Set, Get = Get, SetInverse = SetInverse,
        GetInverse = GetInverse)
}


# Returns the inverse of a CacheMatrix list object. It will first check for a
# cached version of the inverse, but if not found, it will calculate the inverse
# and cache it for future invocations of cacheSolve.
#
# Args:
#   x: A CacheMatrix list object returned by a call to makeCacheMatrix. The
#       matrix represented by this object must be invertible.
#
# Returns:
#   The inverse of x.

cacheSolve <- function(x, ...) {
    # Grab cached inverse from x
    inverse <- x$GetInverse()

    # Check that inverse was actually cached
    if ( is.null(inverse) )
    {   # The inverse is not yet cached, so solve and cache it now
        inverse <- solve(x$Get(), ...)
        x$SetInverse(inverse)
    }

    # Return inverse which is now guaranteed to be set
    inverse
}
