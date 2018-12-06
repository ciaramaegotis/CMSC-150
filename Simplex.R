normalize <- function(augMatrix, row, col){
  augMatrix[row,] = augMatrix[row,]/augMatrix[row, col]
  return(augMatrix)
}

gaussJordanSimplex <- function(augMatrix){
  iteration = 0
  min = min(augMatrix[nrow(augMatrix),])
  while (min < 0){
    column = which.min(augMatrix[nrow(augMatrix),])
    row = which.max(1/(augMatrix[, ncol(augMatrix)]/augMatrix[, column]))
    #now that we have the row and column, normalize the row
    augMatrix = normalize(augMatrix, row, column)
    row_counter = 1
    while (row_counter <= nrow(augMatrix)){
      jnot = augMatrix[row_counter, column]
      if (row_counter != row){
        col_counter = 1
        while (col_counter <= ncol(augMatrix)){
          augMatrix[row_counter, col_counter] = augMatrix[row_counter, col_counter] - (augMatrix[row, col_counter]*jnot)
          col_counter = col_counter + 1
        }
      }
      row_counter = row_counter + 1
    }
    min = min(augMatrix[nrow(augMatrix), ])
    iteration = iteration + 1
  }
  return(augMatrix)
}

getSolution <- function(augMatrix){
  zero_counter = apply(augMatrix, 2, function(c)sum(c==0))
  one_counter = apply(augMatrix, 2, function(c)sum(c==1))
  final_array = matrix(0L, nrow = 1, ncol = ncol(augMatrix)-1, byrow = TRUE)
  counter = 1
  while (counter < ncol(augMatrix)){
    if (zero_counter[counter] == nrow(augMatrix)-1 && one_counter[counter] == 1){
      final_array[counter] = augMatrix[match(c(1), augMatrix[, counter]), ncol(augMatrix)]
    }else{
      final_array[counter] <- NA
    }
    counter = counter + 1
  }
  return(final_array)
}

matrix = matrix(c(7, 11, 1, 0, 0, 0, 0, 77, 10, 8, 0, 1, 0, 0, 0, 80, 1, 0, 0, 0, 1, 0, 0, 9, 0, 1, 0, 0, 0, 1, 0, 6, -150, -175, 0, 0, 0, 0, 1, 0), nrow = 5, ncol = 8, byrow=TRUE)
print(matrix)
matrix = gaussJordanSimplex(matrix)
print(matrix)
getSolution(matrix)