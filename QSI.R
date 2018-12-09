solveQSI <- function(input, functions, x_array){
  counter = 1
  isFound = FALSE
  while (counter < length(x_array)-1){
    if (input >= x_array[counter] && input <= x_array[counter+1]){
      isFound = TRUE
      svalue(chosen_func_qsi_output) <- functions[counter]
      eval(parse(text = functions[counter]))
      svalue(output_qsi) <- evaluateFunction(input)
      break
    }
    counter = counter + 1
  }
  if (isFound == FALSE){
    glabel(text = "Not within bounds.", markup = FALSE, editable = FALSE, handler = NULL,
           action = NULL, container = qsi, toolkit = guiToolkit())
  }
}

getFunctions <- function(array, raw_data, numOfEquations){
  funcArray = matrix(0L, nrow = ncol(raw_data)-1, ncol = 1, byrow = TRUE)
  counter = 1
  funcArray_counter = 1
  while(counter < numOfEquations+1){
    if (counter == 1){
      string = paste("evaluateFunction <- function(x) return(", " 0*x^2 + ", array[counter], "*x + ", array[counter+1], ")")
      funcArray[funcArray_counter] = string
      counter = counter + 2
    }else{
      string = paste("evaluateFunction <- function(x) return(", array[counter], "*x^2 + ", array[counter+1], "*x + ", array[counter+2], ")")
      funcArray[funcArray_counter] = string
      counter = counter + 3
    }
    funcArray_counter = funcArray_counter + 1
  }
  return(funcArray)
}

getInternalFunctions <- function(raw_data){
  arrayOfFunctions = matrix(0L, nrow = (2*(ncol(raw_data)-2)), ncol = 1, byrow=TRUE)
  arrayOfFunctions_iterator = 1
  counter = 2
  while (counter < ncol(raw_data)){
    string = paste((raw_data[1, counter]^2), "a", (counter-1), " + ", raw_data[1, counter], "b", (counter-1), " + c", (counter-1), " = ", raw_data[1, counter] )
    arrayOfFunctions[arrayOfFunctions_iterator] = string
    preAugMatrix[arrayOfFunctions_iterator, ] = c((raw_data[1,counter]^2), raw_data[1,counter], 1, raw_data[2,counter])
    arrayOfFunctions_iterator = arrayOfFunctions_iterator + 1
    
    string = paste((raw_data[1, counter]^2),"a", (counter), " + ", raw_data[1, counter], "b", (counter), " + c", (counter), " = ", raw_data[1, counter] )
    preAugMatrix[arrayOfFunctions_iterator, ] = c((raw_data[1,counter]^2), raw_data[1,counter], 1, raw_data[2,counter])
    arrayOfFunctions[arrayOfFunctions_iterator] = string
    arrayOfFunctions_iterator = arrayOfFunctions_iterator + 1
    counter = counter + 1
  }
  #glabel(text = arrayOfFunctions, markup = FALSE, editable = FALSE, handler = NULL,
  #                action = NULL, container = g, toolkit = guiToolkit())
  return(preAugMatrix);
}

getExternalFunctions <- function(raw_data, preAugMatrix){
  arrayOfFunctions = matrix(0L, nrow = 2, ncol = 1, byrow = TRUE)
  counter = 1
  string = paste((raw_data[1, 1]^2), "a", counter, "+", raw_data[1, 1], "b", counter, "+ c 1 = ", raw_data[2, 1])
  arrayOfFunctions[1,] = string
  
  preAugMatrix[2*(ncol(raw_data)-2)+1,] = c((raw_data[1,1]^2), raw_data[1, 1], 1, raw_data[2, 1])
  
  counter = ncol(raw_data)-1
  string = paste((raw_data[1, counter+1]^2), "a", counter, " + ", raw_data[1,counter+1], "b", counter, "+c",counter," = ", raw_data[2,counter+1])
  arrayOfFunctions[2, ] = string
  
  preAugMatrix[2*(ncol(raw_data)-2)+2,] = c((raw_data[1,counter+1]^2), raw_data[1, counter+1], 1, raw_data[2, counter+1])
  
  #glabel(text = arrayOfFunctions, markup = FALSE, editable = FALSE, handler = NULL,
  #      action = NULL, container = g, toolkit = guiToolkit())
  return(preAugMatrix)
  
}

getConnectingFunctions <- function(raw_data, preAugMatrix){
  arrayOfFunctions = matrix(0L, nrow = (ncol(raw_data)-2), ncol = 1, byrow = TRUE)
  counter = 1
  while (counter < ncol(raw_data)-1){
    string = paste((raw_data[1, counter+1]*2), "a", counter, "+", "b", counter, "=",(raw_data[1, counter+1]*2), "a", (counter+1), "+b", (counter+1))
    arrayOfFunctions[counter, ] = string
    preAugMatrix[nrow(preAugMatrix)-(ncol(raw_data)-2)+counter,] = c(raw_data[1, counter+1]*2, 1, raw_data[1,counter+1]*2, 1)
    counter = counter + 1
  }
  #glabel(text = arrayOfFunctions, markup = FALSE, editable = FALSE, handler = NULL,
  #      action = NULL, container = g, toolkit = guiToolkit())
  return(preAugMatrix);
}

setUpMatrix <-function(augMatrix, preAugMatrix, raw_data, numOfEquations){
  column = 1
  row_counter = 1
  while(row_counter < (2*(ncol(raw_data)-2))){
    if (row_counter == 1){  #special case
      augMatrix[1,1] = preAugMatrix[1,2]
      augMatrix[1,2] = preAugMatrix[1,3]
      augMatrix[1,numOfEquations+1] = preAugMatrix[1,4]
      
      augMatrix[2,3] = preAugMatrix[2,1]
      augMatrix[2,4] = preAugMatrix[2,2]
      augMatrix[2,5] = preAugMatrix[2,3]
      augMatrix[2,numOfEquations+1] = preAugMatrix[2,4]
      column = 3
    }else{
      augMatrix[row_counter,column] = preAugMatrix[row_counter,1]
      augMatrix[row_counter,column+1] = preAugMatrix[row_counter,2]
      augMatrix[row_counter,column+2] = preAugMatrix[row_counter,3]
      augMatrix[row_counter,numOfEquations+1] = preAugMatrix[row_counter,4]
      
      column = column + 3
      augMatrix[row_counter+1,column] = preAugMatrix[row_counter+1,1]
      augMatrix[row_counter+1,column+1] = preAugMatrix[row_counter+1,2]
      augMatrix[row_counter+1,column+2] = preAugMatrix[row_counter+1,3]
      augMatrix[row_counter+1,numOfEquations+1] = preAugMatrix[row_counter+1,4]
      
    }
    row_counter = row_counter + 2
  }
  
  counter = (2*(ncol(raw_data)-2))+1
  while (counter < (2*(ncol(raw_data)-2))+3){
    if (counter == (2*(ncol(raw_data)-2)+1)){
      augMatrix[counter,1] = preAugMatrix[counter,2]
      augMatrix[counter,2] = preAugMatrix[counter,3]
      augMatrix[counter,numOfEquations+1] = preAugMatrix[counter,4]
    }else{
      index = ((ncol(raw_data)-2)*3)
      augMatrix[counter,index] = preAugMatrix[counter,1]
      augMatrix[counter,index+1] = preAugMatrix[counter,2]
      augMatrix[counter,index+2] = preAugMatrix[counter,3]
      augMatrix[counter,numOfEquations+1] = preAugMatrix[counter,4]
    }
    counter = counter + 1
  }
  
  sec_counter = 0
  counter = (2*(ncol(raw_data)-2))+3
  while(counter < numOfEquations+1){
    if (counter == (2*(ncol(raw_data)-2))+3){
      augMatrix[counter,1] = preAugMatrix[counter,2]
      augMatrix[counter,3] = 0- preAugMatrix[counter,3]
      augMatrix[counter,4] = 0- preAugMatrix[counter,4]
    }else{
      index = (3*sec_counter)
      augMatrix[counter,index] = preAugMatrix[counter,1]
      augMatrix[counter,index+1] = preAugMatrix[counter,2]
      augMatrix[counter,index+3] = 0-preAugMatrix[counter,3]
      augMatrix[counter,index+4] = 0-preAugMatrix[counter,4]
    }
    sec_counter = sec_counter + 1
    counter = counter + 1
  }
  return(augMatrix)
}
