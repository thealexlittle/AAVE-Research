# Library Imports
library(tidyverse)


this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)


# Load In CORAAL Dataset
# Only Load in DC because that is the only part that is relevant
# Also pauses and turns are irrelevant

source('http://lingtools.uoregon.edu/coraal/explorer/R/CORAAL_web.R')
coraal <- coraal.webget.data("DC", no.pauses = TRUE, turns = FALSE)
metadat <- coraal.webget.meta("DC")



# Create Regex Patterns for retrieving features
p.be <- "(^| )(?:I|[Ww]e|[Hh]e|[Tt]hey|[Ss]he|[Yy]ou|[Tt]hat|[Ii]t) be( |$)"
p.contractions <- "(?:am |[sedoi])n(?:'t|ot)"
# p.past.reg <- "([Ww]eren't|[Ww]asn't|[Cc]an't)"
p.reductions <- "musta|woulda|shoulda|mighta|gonna|hafta|tryna|sposta|finna|gotta|wanna|oughta|cause|til"

be.utts <- coraal.search(p.be)
contraction.utts <- coraal.search(p.contractions)
red.utts <- coraal.search(p.reductions)

t <- transmute(metadat, sum(be.utts$Spkr == metadat$CORAAL.Spkr))

count_occ_for_spkr <- function(res){
  res.col <- lapply(metadat$CORAAL.Spkr, FUN = function(s){
    sum(res$Spkr == s)
  }) 
}


# Add Columns to Metadata



# Create Subcategories Based on Metadata
md.recent <- filter(md, Year.of.Interview > 2000)
