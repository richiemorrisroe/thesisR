data(healthoptmind)
rand <- dat[,grep("RAND", x=names(dat))]
maas <- dat[,grep("MAASQ", x=names(dat))]
factors <- 1:8
rand.fs <- fit_factor_series(rand, factors=factors)
test_that("class of fs object is factor_series", {
    expect_is(rand.fs, c("factor_series", "fa"))
})
maasfact1 <- psych::fa(na.omit(maas), 1,
                       method="ml",
                       rotate="oblimin")
maas1_xtab <- factor_xtab(maasfact1, names=c("Mindfulness"),
                          label="tab:hom1maas1fact",
                          caption="Factor Loadings,
One Factor Solution, MAAS, Sample One")
test_that("factor_xtab returns an xtable object",
          expect_is(maas1_xtab, c("data.frame", "xtable")))
