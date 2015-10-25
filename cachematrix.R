## makeCacheMatrix and cacheSolve are functions designed to find the value of a matrix
## and then find the mean of the value, and then find the inverse of the mean

## makeCacheMatrix finds the mean of your matrix while creating 2 new functinos to setmean and getmean

makeCacheMatrix <- function(mValue = numeric()) {
  
  ##Set the mMean object to NULL, so that we can check to see if it exists in the cacheSolve function
  mMean <- NULL
  
  ##Create new function to pass the new MValue if passed
  set <- function(newMValue) {
    mValue <<- newMValue
    mMean <<- NULL
  }
  
  ##Create the get function
  get <- function() { 
    mValue
  }
  
  ##Create the setmean function to return the mean with proper scoping
  setmean <- function(mean) {
    mMean <<- mean
  }
  
  ##Create getmean Function
  getmean <- function() {
    mMean
  } 
  
  #Create the list with newly created functions
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

##cacheSolve functions pulls the value of the mean from cache, checks to see if it exists, and if it does pulls from cache, otherwise 
##calculates the mean

cacheSolve <- function(mValue, ...) {
  ##Get the value of the mean if it exists
  mMean <- mValue$getmean()
  
  ##Check to see if the value exists, if it does pulls from cache
  if(!is.null(mMean)) {
    message("getting cached data")
    ##Returns inverse value of the mean
    return(solve(mMean))
  }
  
  ##If the mean wasn't null, then find the mean and invert it
  
  ##Get the data from makeCacheMatrix
  newData <- mValue$get()
  
  ##Calculate the mean of the data
  mMean <- mean(newData, ...)
  
  ##Set the mean
  mValue$setmean(mMean)
  
  ##Returns inverse value of the mean
  solve(mMean)
}