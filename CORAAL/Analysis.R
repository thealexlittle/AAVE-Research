# Library Imports
library(tidyverse)


this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Load In CORAAL Dataset
# Only Load in DC because that is the only part that is relevant
# Also pauses and turns are irrelevant

# I think I'm getting rate limited 
source('http://lingtools.uoregon.edu/coraal/explorer/R/CORAAL_web.R')

coraal <- coraal.webget.data("DC", no.pauses = TRUE)
md <- coraal.webget.meta("DC")

# Get Rid of Useless Metadata Info
md <- md %>% select(!c(2,3,5:10,17,22,23,41:49, 52))


# Create Regex Patterns for retrieving features
p.be <- "(^| )(?:I|[Ww]e|[Hh]e|[Tt]hey|[Ss]he|[Yy]ou|[Tt]hat|[Ii]t) be( |$)"
p.contractions <- "(?:am |[sedoi])n(?:'t|ot)"
# p.past.reg <- "([Ww]eren't|[Ww]asn't|[Cc]an't)"
p.reductions <- "musta|woulda|shoulda|mighta|gonna|hafta|tryna|sposta|finna|gotta|wanna|oughta|cause|til"
p.aint <- "ain't"

#Search CORAAL for utterances
be.utts <- coraal.search(p.be)
contraction.utts <- coraal.search(p.contractions)
red.utts <- coraal.search(p.reductions)
aint.utts <- coraal.search(p.aint)

#Helper function to count amount of times a speaker has uttered a given utterance 
query2col <- function(res){
  r <- lapply(md$CORAAL.Spkr, FUN = function(s){
    sum(res$Spkr == s)
  })
  r <- as.numeric(r) 
}

# Add Amount of Utterances of each column to metadata
md <- md %>% mutate(be = query2col(be.utts),
                    contra = query2col(contraction.utts),
                    red = query2col(red.utts),
                    aint = query2col(aint.utts)
                    )

# Create Extra Columns For Data Ratios 
# utt / word_count 
# utt / total utts

# Create Subcategories Based on Metadata
md.recent.int <- md %>% filter(Year.of.Interview > 2000)
md.old.int <- md %>% filter(Year.of.Interview <= 2000)

md.adults.int <- md %>% filter(Age > 20)
md.children.nt <- md %>% filter(Age <= 20)

md.young.birth <- md %>% filter(Year.of.Birth <= 1962)
md.old.birth <- md %>% filter(Year.of.Birth > 1962)

summary(md.recent.int)

ggplot(md.recent.int, mapping = aes(Year.of.Birth, be)) + geom_point()
ggplot(md.old.int, mapping = aes(Year.of.Birth, be)) + geom_point()

ggplot(md.old.int, mapping = aes(Age, be)) + geom_point()
ggplot(md.recent.int, mapping = aes(Age, be)) + geom_point()
