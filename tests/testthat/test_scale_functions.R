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
randset2 <- paste(rand, c(3:12), sep="")
randset2 <- dat[,randset2]
randrecode2 <- recode_many(randset2,
                          vars=c("RANDQ3",
                          "RANDQ4",
                          "RANDQ5",
                          "RANDQ6",
                          "RANDQ7",
                          "RANDQ8",
                          "RANDQ9",
                          "RANDQ10",
                          "RANDQ11",
                          "RANDQ12"),
                          Recodings="1=0;2=50;3=100")
randset3 <- paste(rand, c(13:19), sep="")
randset3 <- dat[,randset3]
randrecode3 <- recode_many(randset3,
                          vars=c("RANDQ13",
                          "RANDQ14",
                          "RANDQ15",
                          "RANDQ16",
                          "RANDQ17",
                          "RANDQ18",
                          "RANDQ19"),
                          Recodings="1=0;2=100")
randset4 <- paste(rand, c(21,23,26,27,30), sep="")
randset4 <- dat[,randset4]
randrecode4 <- recode_many(randset4,
                          vars=c("RANDQ21",
                          "RANDQ23",
                          "RANDQ26",
                          "RANDQ27",
                          "RANDQ30"),
                          Recodings="1=100;2=80;3=60;4=40;5=20;6=0")
randset5 <- paste(rand, c(24,25,28,29,31), sep="")
randset5 <- dat[,randset5]
randrecode5 <- recode_many(randset5,
                          vars=c("RANDQ24",
                          "RANDQ25",
                          "RANDQ28",
                          "RANDQ29",
                          "RANDQ31"),
                          Recodings="1=0;2=20;3=40;4=60;5=80;6=100")
randset6 <- paste(rand, c(32,33,35), sep="")
randset6 <- dat[,randset6]
randrecode6 <- recode_many(randset6,
                          vars=c("RANDQ32",
                          "RANDQ33",
                          "RANDQ35"),
                           Recodings="1=0;2=25;3=50;4=75;5=100")
rand_scored <- cbind(randset1, randset2, randset3,
                     randset4, randset5, randset6)
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
