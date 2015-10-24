multi_folds <- 10
opt.part <- caret::createMultiFolds(na.omit(dat$optimism), k=multi_folds)
optfolds <- train_test_sets(opt.part, dat)
test_that("the correct number of folds are created",
          expect_equal(length(optfolds), multi_folds * 10))
