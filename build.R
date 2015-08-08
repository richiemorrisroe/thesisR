## create("thesisR") not needed no more

devtools::use_package("psych")
devtools::use_package(c("plyr"))
devtools::use_package(c("ggplot2"))
devtools::use_package(c("reshape2"))
devtools::use_package(c("GPArotation"))
devtools::document()
devtools::build()
devtools::install()
