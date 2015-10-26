data(healthoptmind)
multi_folds <- 10
opt.part <- caret::createMultiFolds(na.omit(dat$optimism), k=multi_folds)
optfolds <- train_test_sets(opt.part, dat)
test_that("the correct number of folds are created",
          expect_equal(length(optfolds), multi_folds * 10))
maas <- dat[,grep("MAASQ", x=names(dat))]
test_splits <- 4
big_splits <- 10
split_maas <- split_sample(maas, split=test_splits)
maas_combinations <- create_combinations(split_maas)
split_maas10 <- split_sample(maas, split=big_splits)
## test_that("create combinations returns the right dim train and test sets:",{
##           expect_gt(lapply(split_maas["train"], dim),
##                     lapply(split_maas, function (x) "[[", "test", dim))
##       })
