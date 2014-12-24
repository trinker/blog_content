Map(function(x, y) {
    if (!x %in% list.files(.libPaths())){
        install.packages(x)   
    } else {
        if (packageVersion(x) < y) {
            install.packages(x)   
        } else {
            message(sprintf("Version of %s is suitable for demonstration", x))
        }
    }
}, c("qdapRegex", "qdapTools"), c("0.2.0", "1.1.0"))
 
lapply(c("qdapRegex", "qdapTools", "ggplot2", "qdap"), require, character.only=TRUE)



## Download .docx
url_dl("http://umlreading.weebly.com/uploads/2/5/2/5/25253346/whole_language_timeline-updated.docx")
 
## Read in .docx
txt <- read_docx("whole_language_timeline-updated.docx")
 
## Remove non ascii characters
txt <- rm_non_ascii(txt) 
 
## Split into body/references sections
parts <- split_vector(txt, split = "References", include = TRUE, regex=TRUE)
 
## View body
parts[[1]]

## View references
parts[[2]]

## Extract citations in order of appearance
rm_citation(unbag(parts[[1]]), extract=TRUE)[[1]]
 
## Extract citations by section 
rm_citation(parts[[1]], extract=TRUE)
 
## Frequency
(cites <- list2df(sort(table(rm_citation(unbag(parts[[1]]),
    extract=TRUE)), TRUE), "freq", "citation")[2:1])



## Distribution of citations (find locations)
cite_locs <- do.call(rbind, lapply(cites[[1]], function(x){
    m <- gregexpr(x, unbag(parts[[1]]), fixed=TRUE)
    data.frame(
        citation=x,
        start = m[[1]] -5,
        end =  m[[1]] + 5 + attributes(m[[1]])[["match.length"]]
    )
}))
 
## Plot the distribution
ggplot(cite_locs) +
    geom_segment(aes(x=start, xend=end, y=citation, yend=citation), size=3,
        color="yellow") +
    xlab("Duration") +
    scale_x_continuous(expand = c(0,0),
        limits = c(0, nchar(unbag(parts[[1]])) + 25)) +
    theme_black() +
    theme(panel.grid.major=element_line(color="grey20"))




