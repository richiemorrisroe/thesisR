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
data(tcq)
tcqitems <- tcq[,grep("^Pill|^Cre|^Inj|^Acu|^Ho|^Re[1-6]$", x=names(tcq))]
tcq_fact6 <- psych::fa(na.omit(tcqitems), nfactors=6, method="minres", rotate="promax")
tcq6_cor_xtab <- factor_cor(tcq_fact6)
test_that("factor_cor returns an xtable",
          expect_is(tcq6_cor_xtab, c("xtable", "data.frame")))
loading_list <- extract_loadings(tcq_fact6)
test_that("extract loadings returns a list",
          expect_is(extract_loadings(tcq_fact6), "list"))
fit4 <- fit_indices(tcq_fact6)
test_that("fit indices fails when given an item of incorrect class",
          expect_error(fit_indices(tcqitems)))
test_that("fit indices produces output when given correct input",
          expect_output(fit_indices(tcq_fact6), ".*"))
rand_fact_wold <- cv.svd.wold(na.omit(rand), k=10, maxrank=12)
rand.fact.gabriel <- cv.svd.gabriel(na.omit(rand), krow=2, kcol=2, maxrank=18)
svd_cv_rand <- svd_cv(rand_fact_wold)
test_that("svd_cv returns a data.frame",
          expect_is(svd_cv(rand.fact.gabriel), "data.frame"))
