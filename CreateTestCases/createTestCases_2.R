# ------------------------------------------------------------------------------
# This script produces color matrix files to be used for testing the internal
# functions of viscomplexr that transform complex number arrays into color
# arrays.
# ------------------------------------------------------------------------------

# The array is called "w"
load("tests/testthat/1wmatCase001.RData")

# A pointer to this array has to be handed to the color transform functions
pW <- viscomplexr:::newPointer(w)
rm(w)

pCol <- viscomplexr:::newPointer(NULL)


viscomplexr:::phaseColhsv(pCompArr = pW, pHsvCol = pCol)
Col <- pCol$value
save(Col, file = "tests/testthat/1wmatCase001_Colhsv.RData")


viscomplexr:::phaseAngColhsv(pCompArr = pW, pHsvCol = pCol)
Col <- pCol$value
save(Col, file = "tests/testthat/1wmatCase001_AngColhsv.RData")


viscomplexr:::phaseModColhsv(pCompArr = pW, pHsvCol = pCol)
Col <- pCol$value
save(Col, file = "tests/testthat/1wmatCase001_ModColhsv.RData")


viscomplexr:::phaseModAngColhsv(pCompArr = pW, pHsvCol = pCol)
Col <- pCol$value
save(Col, file = "tests/testthat/1wmatCase001_ModAngColhsv.RData")


viscomplexr:::phaseModColBw(pCompArr = pW, pBwCol = pCol)
Col <- pCol$value
save(Col, file = "tests/testthat/1wmatCase001_ModColBw.RData")


viscomplexr:::phaseAngColBw(pCompArr = pW, pBwCol = pCol)
Col <- pCol$value
save(Col, file = "tests/testthat/1wmatCase001_AngColBw.RData")


viscomplexr:::phaseModAngColBw(pCompArr = pW, pBwCol = pCol)
Col <- pCol$value
save(Col, file = "tests/testthat/1wmatCase001_ModAngColBw.RData")










