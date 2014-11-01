if (!packageVersion("qdap") >= "2.2.0") {
    install.packages("qdap")	
}
library(qdap); library(qdapDictionaries); library(ggplot2); library(dplyr)
data(GradyAugmented)

GradyAugmentedGradyAugmented
?GradyAugmented

summary(nchar(GradyAugmented))

dist_tab(nchar(GradyAugmented))

ggplot(data.frame(nletters = nchar(GradyAugmented)), aes(x=nletters)) + 
    geom_histogram(binwidth=1, colour="grey70", fill="grey60") +
    theme_minimal() + 
    geom_vline(xintercept = mean(nchar(GradyAugmented)), size=1, 
        colour="blue", alpha=.7) + 
    xlab("Number of Letters")
	
position <- lapply(GradyAugmented, function(x){
    z <- unlist(sapply(letters, function(y){
        gregexpr(y, x)
    }))
    z <- z[z != -1] 
    setNames(z, gsub("\\d", "", names(z)))
})


position2 <- unlist(position)

freqdat <- dist_tab(names(position2))
freqdat[["Letter"]] <- factor(toupper(freqdat[["interval"]]), 
    levels=toupper((freqdat %>% arrange(freq))[[1]] %>% as.character))

ggplot(freqdat, aes(Letter, weight=percent)) + 
  geom_bar() + coord_flip() +
  scale_y_continuous(breaks=seq(0, 12, 2), label=function(x) paste0(x, "%"), 
      expand = c(0,0), limits = c(0,12)) +
  theme_minimal()
  
dat <- data.frame(letter=toupper(names(position2)), position=unname(position2))

dat2 <- table(dat)
dat3 <- t(round(apply(dat2, 1, function(x) x/sum(x)), digits=3) * 100)
qheat(apply(dat2, 1, function(x) x/length(position2)), high="blue", 
    low="yellow", by.column=NULL, values=TRUE, digits=3, plot=FALSE) +
	ylab("Letter") + xlab("Position") + 
	guides(fill=guide_legend(title="Proportion"))