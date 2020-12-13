# -----------------------------------------------------------------------------
# Test if complex numbers are correctly transformed into colors
# -----------------------------------------------------------------------------


# Test transformation function phaseColhsv
testCase1 <- function() {

  # Load the array with complex numbers
  # This is, actually, test case 1 from the test 'test-funsWork.R'.
  # The array is called "w"
  load("1wmatCase001.RData")

  # A pointer to this array has to be handed to the color transform functions
  pW <- viscomplexr:::newPointer(w)
  rm(w)

  # load reference colors (loaded object's name is 'Col')
  load("1wmatCase001_Colhsv.RData")

  # make empty pointer for storing the outcome
  pOutCol <- viscomplexr:::newPointer(NULL)

  # call the transformation
  viscomplexr:::phaseColhsv(pCompArr = pW, pHsvCol = pOutCol)

  # Compare reference and actual outcome
  rslt <- all.equal(Col, pOutCol$value)

  # Clean up
  rm(Col)
  pOutCol$value <- NULL
  rm(pOutCol)
  pW$value <- NULL
  rm(pW)

  return(rslt)
}



# Test transformation function phaseAngColhsv
testCase2 <- function() {

  # Load the array with complex numbers
  # This is, actually, test case 1 from the test 'test-funsWork.R'.
  # The array is called "w"
  load("1wmatCase001.RData")

  # A pointer to this array has to be handed to the color transform functions
  pW <- viscomplexr:::newPointer(w)
  rm(w)

  # load reference colors (loaded object's name is 'Col')
  load("1wmatCase001_AngColhsv.RData")

  # make empty pointer for storing the outcome
  pOutCol <- viscomplexr:::newPointer(NULL)

  # call the transformation
  viscomplexr:::phaseAngColhsv(pCompArr = pW, pHsvCol = pOutCol)

  # Compare reference and actual outcome
  rslt <- all.equal(Col, pOutCol$value)

  # Clean up
  rm(Col)
  pOutCol$value <- NULL
  rm(pOutCol)
  pW$value <- NULL
  rm(pW)

  return(rslt)
}



# Test transformation function phaseModColhsv
testCase3 <- function() {

  # Load the array with complex numbers
  # This is, actually, test case 1 from the test 'test-funsWork.R'.
  # The array is called "w"
  load("1wmatCase001.RData")

  # A pointer to this array has to be handed to the color transform functions
  pW <- viscomplexr:::newPointer(w)
  rm(w)

  # load reference colors (loaded object's name is 'Col')
  load("1wmatCase001_ModColhsv.RData")

  # make empty pointer for storing the outcome
  pOutCol <- viscomplexr:::newPointer(NULL)

  # call the transformation
  viscomplexr:::phaseModColhsv(pCompArr = pW, pHsvCol = pOutCol)

  # Compare reference and actual outcome
  rslt <- all.equal(Col, pOutCol$value)

  # Clean up
  rm(Col)
  pOutCol$value <- NULL
  rm(pOutCol)
  pW$value <- NULL
  rm(pW)

  return(rslt)
}



# Test transformation function phaseModAngColhsv
testCase4 <- function() {

  # Load the array with complex numbers
  # This is, actually, test case 1 from the test 'test-funsWork.R'.
  # The array is called "w"
  load("1wmatCase001.RData")

  # A pointer to this array has to be handed to the color transform functions
  pW <- viscomplexr:::newPointer(w)
  rm(w)

  # load reference colors (loaded object's name is 'Col')
  load("1wmatCase001_ModAngColhsv.RData")

  # make empty pointer for storing the outcome
  pOutCol <- viscomplexr:::newPointer(NULL)

  # call the transformation
  viscomplexr:::phaseModAngColhsv(pCompArr = pW, pHsvCol = pOutCol)

  # Compare reference and actual outcome
  rslt <- all.equal(Col, pOutCol$value)

  # Clean up
  rm(Col)
  pOutCol$value <- NULL
  rm(pOutCol)
  pW$value <- NULL
  rm(pW)

  return(rslt)
}



# Test transformation function phaseModColBw
testCase5 <- function() {

  # Load the array with complex numbers
  # This is, actually, test case 1 from the test 'test-funsWork.R'.
  # The array is called "w"
  load("1wmatCase001.RData")

  # A pointer to this array has to be handed to the color transform functions
  pW <- viscomplexr:::newPointer(w)
  rm(w)

  # load reference colors (loaded object's name is 'Col')
  load("1wmatCase001_ModColBw.RData")

  # make empty pointer for storing the outcome
  pOutCol <- viscomplexr:::newPointer(NULL)

  # call the transformation
  viscomplexr:::phaseModColBw(pCompArr = pW, pBwCol = pOutCol)

  # Compare reference and actual outcome
  rslt <- all.equal(Col, pOutCol$value)

  # Clean up
  rm(Col)
  pOutCol$value <- NULL
  rm(pOutCol)
  pW$value <- NULL
  rm(pW)

  return(rslt)
}



# Test transformation function phaseAngColBw
testCase6 <- function() {

  # Load the array with complex numbers
  # This is, actually, test case 1 from the test 'test-funsWork.R'.
  # The array is called "w"
  load("1wmatCase001.RData")

  # A pointer to this array has to be handed to the color transform functions
  pW <- viscomplexr:::newPointer(w)
  rm(w)

  # load reference colors (loaded object's name is 'Col')
  load("1wmatCase001_AngColBw.RData")

  # make empty pointer for storing the outcome
  pOutCol <- viscomplexr:::newPointer(NULL)

  # call the transformation
  viscomplexr:::phaseAngColBw(pCompArr = pW, pBwCol = pOutCol)

  # Compare reference and actual outcome
  rslt <- all.equal(Col, pOutCol$value)

  # Clean up
  rm(Col)
  pOutCol$value <- NULL
  rm(pOutCol)
  pW$value <- NULL
  rm(pW)

  return(rslt)
}



# Test transformation function phaseModAngColBw
testCase7 <- function() {

  # Load the array with complex numbers
  # This is, actually, test case 1 from the test 'test-funsWork.R'.
  # The array is called "w"
  load("1wmatCase001.RData")

  # A pointer to this array has to be handed to the color transform functions
  pW <- viscomplexr:::newPointer(w)
  rm(w)

  # load reference colors (loaded object's name is 'Col')
  load("1wmatCase001_ModAngColBw.RData")

  # make empty pointer for storing the outcome
  pOutCol <- viscomplexr:::newPointer(NULL)

  # call the transformation
  viscomplexr:::phaseModAngColBw(pCompArr = pW, pBwCol = pOutCol)

  # Compare reference and actual outcome
  rslt <- all.equal(Col, pOutCol$value)

  # Clean up
  rm(Col)
  pOutCol$value <- NULL
  rm(pOutCol)
  pW$value <- NULL
  rm(pW)

  return(rslt)
}




# The actual tests
test_that("Color transformations are correct", {
  expect_true(testCase1())
  expect_true(testCase2())
  expect_true(testCase3())
  expect_true(testCase4())
  expect_true(testCase5())
  expect_true(testCase6())
  expect_true(testCase7())
})



