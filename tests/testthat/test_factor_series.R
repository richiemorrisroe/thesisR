data(healthoptmind)
rand <- dat[,grep("RAND", x=names(dat))]
factors <- 1:12
rand.fs <- fit_factor_series(rand, factors=factors)
test_that("class of fs object is factor_series", {
    expect_equal(class(rand.fs), c("factor_series", "fa"))
})
