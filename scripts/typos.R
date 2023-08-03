# Key Information --------------------------------------------------------------
# Title: Typos
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: This script defines a series of functions that create typos (induce 
#          errors) during the simulation
# Attribution: based on functions from the fastLink package 
#              (https://cran.r-project.org/package=fastLink)
# ---------------------------------------------------------------------------- #


# function that transposes characters randomly
transpos <- function(x) {
	n <- nchar(x)
  
	if(n > 1) {
	## Transposition: 0 Beginning, 1 End
	if(n == 2){
	  begend <- 1
	} else {
	  begend <- rbinom(1, 1, 0.5)
	} 

	if(begend == 1) {
		index <- (round(nchar(x)/2, 0) + 1):nchar(x)
		} else {
		index <- 1:round(nchar(x)/2, 0)
		}

	diff <- 3
	while(diff > 1){
		diff.o <- diff
		index.2 <- sort(sample(index, 2), decreasing = T)
		diff <- abs(index.2[2] - index.2[1])
	}

	x.0 <- x
	substr(x, index.2[1], index.2[1]) <- substr(x.0, index.2[2], index.2[2])
	substr(x, index.2[2], index.2[2]) <- substr(x.0, index.2[1], index.2[1])
	}

	return(x)
}

# function that introduces typos in a characeter 
# by switching letters close to one another on a qwerty keyboard
typo <- function(x) {

	pairs <- rbind(LETTERS, toupper(c("s","v", "x", "f", "w", "d", "f", "j", "o",
	                                  "k", "l", "k", "n", "m", "p", "o", "w", "e",
	                                  "a", "r", "y", "b", "q", "z", "u", "x")))

	n <- nchar(x)

	if(n > 2) {
	## Typo: 0 Beginning, 1 End
    begend <- rbinom(1, 1, 0.5)

	if(begend == 1) {
		index <- (round(nchar(x)/2, 0) + 1):nchar(x)
		} else {
		index <- 2:round(nchar(x)/2, 0)
		}

	index.2 <- sort(sample(index, 1), decreasing = T)

	substr(x, index.2[1], index.2[1]) <- pairs[ , (pairs[1, ] == substr(x, index.2[1], index.2[1])) == 1 ][2]
	}
	return(x)
}

# function that randomly deletes characters
deletion <- function(x) {
	n <- nchar(x)

	if(n > 1) {
	## Typo: 0 Beginning, 1 End
    begend <- rbinom(1, 1, 0.5)

	if(begend == 1) {
		index <- (round(nchar(x)/2, 0) + 1):nchar(x)
		} else {
		index <- 2:round(nchar(x)/2, 0)
		}
    
  ## ensure that . doesn't get deleted (creates huge numbers)
  period <- regexpr("\\.", x)
  if(n > 3) index <- index[!(index %in% period)]
  
  if(length(index) == 0) index <- period - 1
  
	index.2 <- sample(index, 1)

	substr(x, index.2[1], index.2[1]) <- " "
	x <- gsub(" ", "", x, fixed = TRUE)
	}
	return(x)
}

# typo function but for numbers
typo_num <- function(x) {
  pairs <- rbind(as.character(0:9), 
                 as.character(c(9, 2, 1, 2, 3, 4, 5, 6, 7, 8)))
  
  n <- nchar(x)
  
  if(n > 1) {
    ## Typo: 0 Beginning, 1 End
    begend <- rbinom(1, 1, 0.5)
    
    if(begend == 1) {
      index <- (round(nchar(x)/2, 0) + 1):nchar(x)
    } else {
      index <- 2:round(nchar(x)/2, 0)
    }
    
    ## ensure that . doesn't return NA
    period <- regexpr("\\.", x)

    index <- index[!(index %in% period)]
    
    if(length(index) == 0) index <- period - 1
    
    index.2 <- sort(sample(index, 1), decreasing = T)
    
    if(index.2 == period) return(x)
    
    substr(x, index.2[1], index.2[1]) <- pairs[ , (pairs[1, ] == substr(x, index.2[1], index.2[1])) == 1 ][2]
  } else {
    x <- pairs[2, pairs[1, ] == x]
  } 
  return(x)
}

