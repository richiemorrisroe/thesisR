data(healthoptmind)
rand <- dat[,grep("RAND", x=names(dat))]
rand <- "RANDQ"
randset1 <- paste(rand, c(1, 2, 20, 22, 34, 36), sep="")
randset1 <- dat[,randset1]
randrecode1 <- recode_many(randset1,
                           vars=c("RANDQ1",
                           "RANDQ2",
                           "RANDQ20",
                           "RANDQ34",
                           "RANDQ36"),
                           Recodings= (
                               "1=100;2=75;3=50;4=25;5=0"))
test_that("recode_many returns a df of correct dimensions", {
          expect_is(randrecode1, "data.frame")
          expect_equal(dim(randrecode1), dim(randset1))
      })

sumorder <- c("generalhealth", "physfun", "rolelim",
              "rolelimem", "energyfat",
              "emwellbeing", "socialfunctioning",
              "pain", "mindfulness", "optimism")
totals <- dat[,66:75]
totals <- totals[,sumorder]
apatotals <- apa_demo_tables(totals)
test_that(desc = "apa_demo_tables returns correct data", {
          expect_is(apatotals, "data.frame")
          expect_equal(length(apatotals), 5)
      })
