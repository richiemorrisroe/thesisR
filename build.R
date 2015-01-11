## create("thesisR") not needed no more
devtools::use_package("psych")
devtools::use_package(c("ggplot2"))
devtools::use_package(c("reshape2"))
document()
build()
install()
