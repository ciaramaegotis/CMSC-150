setUpConstraints <- function(total_manufacturer, demands, supply, shipping_costs, plants, warehouses){
  matrix = matrix(0L, nrow = length(plants)+length(warehouses)+1, ncol = (length(plants)*length(warehouses))+(length(plants)+length(warehouses)+2), byrow = TRUE)
  colnames(matrix) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "Z", "ANS")
  slack_index = (length(plants)*length(warehouses))+1
  row_counter = 1
  marker = 1
  counter = 1
  while (row_counter <= length(plants)){
    while (counter <= marker+(length(warehouses)-1)){
      matrix[row_counter, counter] = 1
      counter = counter + 1
    }
    matrix[row_counter, ncol(matrix)] = supply[row_counter]
    matrix[row_counter, slack_index] = 1
    slack_index = slack_index + 1
    marker = counter
    row_counter = row_counter + 1
  }
  demands_limit_index = row_counter + length(warehouses)
  counter = 1
  marker = 1
  original = 1
  while (row_counter < demands_limit_index){
    while (counter <= length(plants)){
      matrix[row_counter, marker] = 1
      marker = marker + length(warehouses)
      counter = counter + 1
    }
    matrix[row_counter, ncol(matrix)] = demands[original]
    matrix[row_counter, slack_index] = -1
    slack_index = slack_index + 1
    original = original + 1
    marker = original
    counter = 1
    row_counter = row_counter + 1
  }
  objective_function = as.vector(t(shipping_costs))
  counter = 1
  while (counter <= length(plants)*length(warehouses)){
    matrix[row_counter, counter] = objective_function[counter]
    counter = counter + 1
  }
  matrix[row_counter, slack_index] = 1
  print(matrix)
  return(matrix)
}

normalize <- function(augMatrix, row, col){
  augMatrix[row,] = augMatrix[row,]/augMatrix[row, col]
  return(augMatrix)
}

getFunctions <- function(matrix){
  variables = colnames(matrix)
  functions = matrix(0L, nrow = nrow(matrix), ncol = 1, byrow=TRUE)
  row_counter = 1
  while (row_counter <= nrow(matrix)){
    col_counter = 1
    string = paste("", matrix[row_counter, ncol(matrix)], " = ")
    while (col_counter < ncol(matrix)-1){
      if (matrix[row_counter, col_counter] != 0){
          string = paste(string, matrix[row_counter, col_counter], "*", variables[col_counter], " + ")
      }
      col_counter = col_counter + 1
    }
    string = substr(string, 1, nchar(string)-3)
    functions[row_counter] = string
    row_counter = row_counter + 1
  }
  print(functions)
  return(functions)
  #return the list of functions
}

modifyFunction <- function(newFunction, index, matrix){
  return(matrix)
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
  colnames(final_array) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "Z")
  counter = 1
  while (counter < ncol(augMatrix)){
    if (zero_counter[counter] == nrow(augMatrix)-1 && one_counter[counter] == 1){
      final_array[counter] = augMatrix[match(c(1), augMatrix[, counter]), ncol(augMatrix)]
    }else{
      final_array[counter] = 0
    }
    counter = counter + 1
  }
  return(final_array)
}
