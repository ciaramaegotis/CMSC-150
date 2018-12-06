#Gotis, Ciara Mae R.
#2015-10323
#Regression

#gets the sum of an array slice
getSummation <- function(slice, start, end, expOrder) {
    sum = 0
    while (start < end+1){
        sum = sum + (slice[start]^expOrder)
        start = start + 1
    }
    return(sum)
}

#special function for the constant column summation
constantSummation <- function(raw_data, slice, start, end, expOrder) {
    sum = 0
    while (start < end+1){
        sum = sum + ((slice[start]^expOrder)*raw_data[2, start])
        start = start + 1
    }
    return(sum)
}

#fills the matrix with the correct values from the summation functions
fillUpMatrix <- function(augMatrix, n, raw_data){
    row_counter = 1
    exponentOrder = 0
    while (row_counter < n+1){
        col_counter = 1
        while (col_counter < n+2){
            if (col_counter == n+1){
                augMatrix[row_counter, col_counter] = constantSummation(raw_data, raw_data[1,], 1, ncol(raw_data), row_counter-1)
            }else{
                augMatrix[row_counter, col_counter] = getSummation(raw_data[1,], 1, ncol(raw_data), exponentOrder)
            }
            col_counter = col_counter + 1
            exponentOrder = exponentOrder + 1
        }
        row_counter = row_counter + 1
        exponentOrder = row_counter - 1
    }
    return(augMatrix)
}

#create a string function
toStringFunction <- function(arrayValues, n) {
    functionDeclaration = "plotValues <- function(x) "
    counter = 1
    while (counter < n+1){
        if (counter == n){
            functionDeclaration = paste(functionDeclaration, arrayValues[(n+1)-counter], "*x^", n-counter, sep = "")
        }else{
            functionDeclaration = paste(functionDeclaration, arrayValues[(n+1)-counter], "*x^", n-counter, " + ", sep = "")
        }
        
        counter = counter + 1
    }
    return(functionDeclaration)
}

#returns an array that contains the y values after calling the stringified function (plotValue)
accumulate <- function(raw_data, accumulatedValues) {
    counter = 1
    while (counter < ncol(accumulatedValues)+1){
        accumulatedValues[1, counter] = plotValues(raw_data[1, counter])
        counter = counter + 1
    }
    return (accumulatedValues)
}
