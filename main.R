library(gWidgets2)
options(guiToolkit="RGtk2")

## Not run:
w <- gwindow("gnotebook example")
nb <- gnotebook(container=w)

pr <- ggroup(container = nb, label="Polynomial Regression", horizontal = FALSE)
qsi <- ggroup(container = nb, label="Quadratic Spline Interpolation", horizontal = FALSE)
s <- ggroup(container = nb, label="Simplex Minimization", horizontal = FALSE)

a <- gfilebrowse("Upload csv file...",cont=qsi, 
     handler=function(h,...){
       print(svalue(a))
       df1 <<- read.csv(svalue(a),header=FALSE,sep=",")
       vector_1 = data.matrix(as.numeric(df1$V1), rownames.force=NA)
       vector_2 = data.matrix(as.numeric(df1$V2), rownames.force=NA)
       raw_data = matrix(0L, nrow = 2, ncol = nrow(vector_1), byrow=TRUE)
       raw_data[1, ] = vector_1
       raw_data[2, ] = vector_2
       raw_data <<- raw_data
       obj <- gtable(t(raw_data), container=qsi)
     })

b <- gfilebrowse("Upload csv file...",cont=s, 
                 handler=function(h,...){
                   print(svalue(b))
                   df1 <<- read.csv(svalue(b),header=FALSE,sep=",")
                   matrix = data.matrix(df1, rownames.force=NA)
                   plants = c("Denver", "Pheonix", "Dallas")
                   warehouses = c("Sacramento, California", "Salt Lake City, Utah", "Albuquerue, Mexico", "Chicago, Illinois", "New York City, New York")
                   total_manufacturer = matrix[1:3, 2:6]
                   demands = matrix[7, 2:6]
                   supply = matrix[8:10, 1]
                   shipping_costs =  matrix[8:10, 2:6]
                   source("Simplex.R")
                   setUpConstraints(total_manufacturer, demands, supply, shipping_costs, plants, warehouses)
                 })

c <- gfilebrowse("Upload csv file...",cont=pr, 
                 handler=function(h,...){
                   print(svalue(c))
                   df1 <<- read.csv(svalue(c),header=FALSE,sep=",")
                   vector_1 = data.matrix(as.numeric(df1$V1), rownames.force=NA)
                   vector_2 = data.matrix(as.numeric(df1$V2), rownames.force=NA)
                   raw_data = matrix(0L, nrow = 2, ncol = nrow(vector_1), byrow=TRUE)
                   raw_data[1, ] = vector_1
                   raw_data[2, ] = vector_2
                   raw_data <<- raw_data
                   obj <- gtable(t(raw_data), container=pr)
                 })

pr_input<- gedit(text = "Enter degree", width = 25, coerce.with = as.numeric, initial.msg="",
      handler = function(h,...){
        source('Regression.R')
        source("GaussJordan.R")
        augMatrix = matrix(0L, nrow = svalue(pr_input), ncol = svalue(pr_input)+1, byrow=TRUE)
        augMatrix = fillUpMatrix(augMatrix, svalue(pr_input), raw_data)
        setOfVar= gaussJordanMethod(augMatrix)
        functionPoly = toStringFunction(setOfVar, svalue(pr_input))
        #display the function in GUI
        functionOutput <- glabel(text = functionPoly, markup = FALSE, editable = FALSE, handler = NULL,
                                 action = NULL, container = pr)
        parse(text = functionPoly)
        eval(parse(text = functionPoly))
        plotValues <<- plotValues
      }, action = NULL, container = pr)

pr_evaluate <- gedit(text = "Num to evaluate", width = 25, coerce.with = as.numeric, initial.msg="",
               handler = function(h,...){
                 #display the output in GUI
                 output <- glabel(text = plotValues(svalue(pr_evaluate)), markup = FALSE, editable = FALSE, handler = NULL,
                                  action = NULL, container = pr)
               }, action = NULL, container = pr)

qsi_input<- gedit(text = "Enter number", width = 25, coerce.with = as.numeric, initial.msg="",
                  handler = function(h,...){
                    source("QSI.R")
                    numOfEquations = (3*(ncol(raw_data)-1))-1
                    augMatrix = matrix(0L, nrow = numOfEquations, ncol = numOfEquations + 1, byrow=TRUE)
                    preAugMatrix = matrix(0L, nrow=numOfEquations, ncol = 4, byrow = TRUE)
                    preAugMatrix <<- preAugMatrix
                    preAugMatrix = getInternalFunctions(raw_data)
                    preAugMatrix = getExternalFunctions(raw_data, preAugMatrix)
                    preAugMatrix = getConnectingFunctions(raw_data, preAugMatrix)
                    preGaussJordan = setUpMatrix(augMatrix, preAugMatrix, raw_data, numOfEquations)
                    source("GaussJordan.R")
                    afterGaussJordan = gaussJordanMethod(preGaussJordan)
                    functions = getFunctions(afterGaussJordan, raw_data, numOfEquations)
                    function_table <- gtable(functions, container = qsi)
                    solveQSI(svalue(qsi_input), functions, raw_data[1,])
                    }, action = NULL, container = qsi)
