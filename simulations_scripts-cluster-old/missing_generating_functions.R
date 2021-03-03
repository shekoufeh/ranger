
# Missing completely at random
MCAR <- function ( data , colName , percentage ){
  n <- nrow( data )
  numbers <- seq(from = 1, to = n)
  idx_na <- sample(numbers,percentage * n)
  data [ idx_na , colName ] <- NA
  return ( data )
}

# Missing at random
MAR <- function ( data , colName , refName , percentage){
  n <- nrow( data )
  numbers <- seq( from = 1 , to = n)
  if ( is.factor(data[,refName])) {
    x <- as.numeric(data [,refName])
    xc <-x
  } else {
    x <-data [ , refName ]
    xc <- (x-min( x ) ) / (max(x)-min( x ) )
  }
  prob <- xc /sum( xc )
  idx_na <- sample( numbers , percentage * n , prob = prob )
  data [ idx_na , colName ] <- NA
  return ( data )
}

# Missing not at random
MNAR <- function ( data , colName , percentage ){
  n <- nrow( data )
  numbers <- seq ( from = 1 , to = n)
  if( is.factor ( data [ , colName ] ) ) {
    x <- as.numeric( data [ , colName ] )
    xc <- x
  } else {
    x <- data [ , colName ]
    xc <- (x-min( x ) ) / (max( x)-min( x ) )
  }
  prob <- xc /sum( xc )
  idx_na <- sample( numbers , percentage*n , prob = prob )
  data [ idx_na , colName ] <- NA
  return ( data )
}

