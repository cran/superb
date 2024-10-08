context("Testing Winer and Mauchly statistical tests and Shrout & Fleiss ICC functions")


test_that("Testing Welch degree of freedom", {
    dta <- data.frame(cbind(
        DV.1 = c(3., 6., 2., 2., 5.),
        DV.2 = c(4., 5., 4., 4., 3.),
        DV.3 = c(2., 7., 7., 8., 6.),
        DV.4 = c(6., 8., 4., 6., 5.),
        grp  = c(1., 1., 2., 2., 2.)
    ))

    expect_equal( WelchDegreeOfFreedom(dta, "DV.1", "grp"), 1.8988764 )
})


test_that("Testing Winer test", {
    dta <- data.frame(cbind(
        col1 <- c(3., 6., 2., 2., 5.),
        col2 <- c(4., 5., 4., 4., 3.),
        col3 <- c(2., 7., 7., 8., 6.),
        col4 <- c(6., 8., 4., 6., 5.)
    ))

    expect_equal( WinerCompoundSymmetryTest(dta), 0.6733123 )
})


test_that("Testing Mauchly test", {
    dta <- data.frame(cbind(
        col1 <- c(3., 6., 2., 2., 5.),
        col2 <- c(4., 5., 4., 4., 3.),
        col3 <- c(2., 7., 7., 8., 6.),
        col4 <- c(6., 8., 4., 6., 5.)
    ))

    expect_equal( MauchlySphericityTest(dta), 0.5824426 )
})


test_that("Testing Shrout and Fleiss functions", {
    dta <- data.frame(cbind(
        clus <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
        col1 <- c(2, 4, 4, 6, 4, 5, 8, 8, 5, 8, 9, 9)
    ))

    expect_equal( ShroutFleissICC1(dta, 1, 2), 0.434343434 )
    expect_equal( ShroutFleissICC11(dta[, 1], dta[,2]), 0.434343434 )

    dta2 <- data.frame(cbind(
        clus <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
        col1 <- c(1, 3, 3, 5, 3, 4, 7, 7, 4, 7, 8, 8),
        col1 <- c(2, 4, 4, 6, 4, 5, 8, 8, 5, 8, 9, 9),
        col1 <- c(3, 5, 5, 7, 5, 6, 9, 9, 6, 9, 10, 10)
    ))

    expect_equal( ShroutFleissICC1(dta2, 1, 2:4), 0.7543859649 )
    expect_equal( ShroutFleissICC1k(dta2[, 1], dta2[,2:4]), 0.7543859649 )

})



context("Testing the plotting sub-functions")

test_that("Testing is.stat.function", {
    expect_equal(superb:::is.stat.function("superbPlot.bar"), FALSE)
    expect_equal(superb:::is.stat.function("mean"), TRUE)
})


test_that("Testing is.plot.function", {
    expect_equal(superb:::is.superbPlot.function("adsff"), FALSE)
    expect_equal(superb:::is.superbPlot.function("superbPlot"), FALSE)
    expect_equal(superb:::is.superbPlot.function("superbPlot.bar"), TRUE)
    expect_equal(superb:::is.superbPlot.function("superbPlot.line"), TRUE)
    expect_equal(superb:::is.superbPlot.function("superbPlot.point"), TRUE)
    expect_equal(superb:::is.superbPlot.function("superbPlot.pointjitter"), TRUE)
    expect_equal(superb:::is.superbPlot.function("superbPlot.pointjitterviolin"), TRUE)
})


test_that("Testing the built-in plotting function", {
    dta <- data.frame( 
                    dose       = cbind(c(0.5,0.5,1,1,2,2)),
                    supp       = cbind(c("OJ","VC","OJ","VC","OJ","VC")),
                    center     = cbind(c(13,8,22,17,26,26)),
                    lowerwidth = cbind(c(-1,-.5,-1.5,-1,-1,-2)),
                    upperwidth = cbind(c(+1,+.5,+1.5,+1,+1,+2))
            )
    tg  <- ToothGrowth
    tg$DV <- tg$len

    p1 <- superbPlot.bar(dta, "dose", 
        "supp", ".~.", tg, list(color="black"), list(color="purple") )
    p2 <- superbPlot.line(dta, "dose", 
        "supp", ".~.", tg, list(color="black"), list(color="purple") )
    p3 <- superbPlot.point(dta, "dose", 
        "supp", ".~.", tg, list(), list() )
    p4 <- superbPlot.pointjitter(dta, "dose", 
        "supp", ".~.", tg, list(color="black"), errorbarParams = list(color="purple")  ) +
        scale_y_continuous("mean ratings") + scale_color_hue(l=40, c=35)
    p5 <- superbPlot.pointjitterviolin(dta, "dose", 
         "supp", ".~dose", tg, list(color="black"), errorbarParams = list(color="purple") ) +
        scale_y_continuous("mean ratings") + scale_color_hue(l=40, c=35)
    expect_error( print(p1), NA )		# ok switched to dodge2
    expect_error( print(p2), NA )
    expect_error( print(p3), NA )
    expect_error( print(p4), NA )		
    expect_error( print(p5), NA )	
	
	
	# the newer plot layouts
	p6 <- superbPlot.boxplot(dta, "dose", "supp", ".~.", tg,  list(color="red") ) 
	p7 <- superbPlot.lineBand(dta, "dose", "supp", ".~.", tg)
	p8 <- superbPlot.raincloud(dta, "dose", "supp", ".~dose", tg) 
	p9 <- superbPlot.halfwidthline(dta, "dose", "supp", ".~.", tg) 

    expect_error( print(p6), NA )		
	expect_error( print(p7), NA )
	expect_error( print(p8), NA )		
	expect_error( print(p9), NA )
	
})


#test_that("Testing the runDebug functions", {
#    expect_equal( getOption("superb.feedback"), c("design","warnings","summary") )
#    expect_equal( runDebug("design","THIS IS A TEST OF runDebug",c(),list()), NULL)
#})


