options(WordpressLogin = c(tylerrinker = "password"), 
	WordpressURL = "http://trinkerrstuff.wordpress.com/xmlrpc.php")
# install.packages("RWordPress", repos = "http://www.omegahat.org/R", type = "source")

library(pacman); p_load(knitr, RWordPress)
knit2wp("C:/Users/Tyler/Desktop/letters/REPORT/letters.Rmd", 
	title = "Exploration of Letter Make Up of English Words")

file.exists("C:/Users/Tyler/Desktop/letters/REPORT/letters.Rmd")
