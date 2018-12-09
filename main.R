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
                   tablePractice <- glayout(homogeneous = FALSE, spacing = 10, container = s)
                   tablePractice[1, 2] <- "Demands"
                   tablePractice[2, 2] <- "Supply"
                   tablePractice[2, 3] <- "California"
                   tablePractice[2, 4] <- "Utah"
                   tablePractice[2, 5] <- "New Mexico"
                   tablePractice[2, 6] <- "Illinois"
                   tablePractice[2, 7] <- "New York City"
                   tablePractice[2, 1] <- "Plants"
                   tablePractice[3, 1] <- "Denver"
                   tablePractice[4, 1] <- "Pheonix"
                   tablePractice[5, 1] <- "Dallas"
                   
                   supply_1 <- gedit(text = supply[1], width = 10, coerce.with = as.numeric, initial.msg="",
                                    handler = function(h,...){
                                      supply[1] = svalue(supply_1)
                                      supply <<- supply
                                    }, action = NULL)
                   supply_2 <- gedit(text = supply[2], width = 10, coerce.with = as.numeric, initial.msg="",
                                     handler = function(h,...){
                                       supply[2] = svalue(supply_2)
                                       supply <<- supply
                                     }, action = NULL)
                   supply_3 <- gedit(text = supply[3], width = 10, coerce.with = as.numeric, initial.msg="",
                                     handler = function(h,...){
                                       supply[3] = svalue(supply_3)
                                       supply <<- supply
                                     }, action = NULL)
                   tablePractice[3, 2] <- supply_1
                   tablePractice[4, 2] <- supply_2
                   tablePractice[5, 2] <- supply_3
                   
                   demands_1 <- gedit(text = demands[1], width = 10, coerce.with = as.numeric, initial.msg="",
                                      handler = function(h,...){
                                        demands[1] = svalue(demands_1)
                                        demands <<- demands
                                      }, action = NULL)
                   demands_2 <- gedit(text = demands[2], width = 10, coerce.with = as.numeric, initial.msg="",
                                      handler = function(h,...){
                                        demands[2] = svalue(demands_2)
                                        demands <<- demands
                                      }, action = NULL)
                   demands_3 <- gedit(text = demands[3], width = 10, coerce.with = as.numeric, initial.msg="",
                                      handler = function(h,...){
                                        demands[3] = svalue(demands_3)
                                        demands <<- demands
                                      }, action = NULL)
                   demands_4 <- gedit(text = demands[4], width = 10, coerce.with = as.numeric, initial.msg="",
                                      handler = function(h,...){
                                        demands[4] = svalue(demands_4)
                                        demands <<- demands
                                      }, action = NULL)
                   demands_5 <- gedit(text = demands[5], width = 10, coerce.with = as.numeric, initial.msg="",
                                      handler = function(h,...){
                                        demands[5] = svalue(demands_5)
                                        demands <<- demands
                                      }, action = NULL)
                   tablePractice[1, 3] <- demands_1
                   tablePractice[1, 4] <- demands_2
                   tablePractice[1, 5] <- demands_3
                   tablePractice[1, 6] <- demands_4
                   tablePractice[1, 7] <- demands_5
                   
                   sc_33 <- gedit(text = shipping_costs[1, 1], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[1, 1] = svalue(sc_33)
                                    shipping_costs <<- shipping_costs
                                  }, action = NULL)
                   sc_34 <- gedit(text = shipping_costs[1, 2], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[1, 2] = svalue(sc_34)
                                    shipping_costs <<- shipping_costs
                                  }, action = NULL)
                   sc_35 <- gedit(text = shipping_costs[1, 3], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[1, 3] = svalue(sc_35)
                                    shipping_costs <<- shipping_costs
                                  }, action = NULL)
                   sc_36 <- gedit(text = shipping_costs[1, 4], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[1, 4] = svalue(sc_36)
                                    shipping_costs <<- shipping_costs
                                  }, action = NULL)
                   sc_37 <- gedit(text = shipping_costs[1, 5], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[1, 5] = svalue(sc_37)
                                    shipping_costs <<- shipping_costs
                                  }, action = NULL)
                   
                   sc_43 <- gedit(text = shipping_costs[2, 1], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[2, 1] = svalue(sc_43)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   sc_44 <- gedit(text = shipping_costs[2, 2], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[2, 2] = svalue(sc_44)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   sc_45 <- gedit(text = shipping_costs[2, 3], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[2, 3] = svalue(sc_45)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   sc_46 <- gedit(text = shipping_costs[2, 4], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[2, 4] = svalue(sc_46)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   sc_47 <- gedit(text = shipping_costs[2, 5], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[2, 5] = svalue(sc_47)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   
                   sc_53 <- gedit(text = shipping_costs[3, 1], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[3, 1] = svalue(sc_53)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   sc_54 <- gedit(text = shipping_costs[3, 2], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[3, 2] = svalue(sc_54)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   sc_55 <- gedit(text = shipping_costs[3, 3], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[3, 3] = svalue(sc_55)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   sc_56 <- gedit(text = shipping_costs[3, 4], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[3, 4] = svalue(sc_56)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   sc_57 <- gedit(text = shipping_costs[3, 5], width = 10, coerce.with = as.numeric, initial.msg="",
                                  handler = function(h,...){
                                    shipping_costs[3, 5] = svalue(sc_57)
                                    shipping_costs <<- shipping_costs
                                  }, action =1, NULL)
                   tablePractice[3, 3] <- sc_33
                   tablePractice[3, 4] <- sc_34
                   tablePractice[3, 5] <- sc_35
                   tablePractice[3, 6] <- sc_36
                   tablePractice[3, 7] <- sc_37
                   
                   tablePractice[4, 3] <- sc_43
                   tablePractice[4, 4] <- sc_44
                   tablePractice[4, 5] <- sc_45
                   tablePractice[4, 6] <- sc_46
                   tablePractice[4, 7] <- sc_47
                   
                   tablePractice[5, 3] <- sc_53
                   tablePractice[5, 4] <- sc_54
                   tablePractice[5, 5] <- sc_55
                   tablePractice[5, 6] <- sc_56
                   tablePractice[5, 7] <- sc_57
                   
                   #create a button that would do the following below
                   solve <- gbutton("Solve!", container = s, handler = function(h,...){
                     source("Simplex.R")
                     print("solve")
                     print(supply)
                     matrix = setUpConstraints(total_manufacturer, demands, supply, shipping_costs, plants, warehouses)
                     initLabel <- glabel("Initial Tableau", container=s)
                     obj <- gtable(matrix, container=s, editable=TRUE)
                     functions = getFunctions(matrix)
                     functions_counter = 1
                     while (functions_counter <= length(functions)){
                       functionOutput <- glabel(text = functions[functions_counter], markup = FALSE, editable = TRUE, handler = function(h,...){print("EDITTED")},
                                                action = NULL, container = s)
                       functions_counter = functions_counter + 1
                     }
                     #TO DO: create global button to solve
                     matrix = gaussJordanSimplex(matrix)
                     gaussLabel <- glabel("Final Matrix", container=s)
                     gaussJordan <- gtable(matrix, container=s, editable=TRUE)
                     solutions = getSolution(matrix)
                     solLabel <- glabel("Solution Set", container = s)
                     solTable <- gtable(solutions, container=s, editable=TRUE)
                   })
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
        augMatrix = matrix(0L, nrow = svalue(pr_input)+1, ncol = svalue(pr_input)+2, byrow=TRUE)
        augMatrix = fillUpMatrix(augMatrix, svalue(pr_input)+1, raw_data)
        setOfVar= gaussJordanMethod(augMatrix)
        functionPoly = toStringFunction(setOfVar, svalue(pr_input)+1)
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
