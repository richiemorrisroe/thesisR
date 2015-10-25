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
