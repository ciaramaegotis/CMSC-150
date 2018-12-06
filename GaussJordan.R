pivot <- function(augMatrix, current_row) {
  #check if it is diagonally dominant
  listOfComparators = abs(augMatrix[current_row:nrow(augMatrix), current_row])
  if (max(listOfComparators) != augMatrix[current_row, current_row]){
    temp = augMatrix[current_row,] 
    augMatrix[current_row,] = augMatrix[which.max(listOfComparators)+current_row-1, ]
    augMatrix[which.max(listOfComparators)+current_row-1, ] = temp
  }
  return(augMatrix)
}

normalize <- function(augMatrix, current_row) {
  #normalize the row
  augMatrix[current_row, ] = augMatrix[current_row, ]/augMatrix[current_row, current_row]
  return(augMatrix)
}

gaussJordanMethod <- function(augMatrix) {
  current_row = 1
  while (current_row < nrow(augMatrix)+1){
    augMatrix = pivot(augMatrix, current_row)
    augMatrix = normalize(augMatrix, current_row)
    inner_row = 1
    while (inner_row <= nrow(augMatrix)){
      if (inner_row != current_row){
        #find the Jnot based on the current_row
        jnot = augMatrix[inner_row, current_row]
        augMatrix[inner_row, current_row:ncol(augMatrix)] =  augMatrix[inner_row, current_row:ncol(augMatrix)] - (augMatrix[current_row, current_row:ncol(augMatrix)]*jnot)
      }
      inner_row = inner_row + 1
    }
    current_row = current_row + 1
  }
  return(augMatrix[, ncol(augMatrix)])
}