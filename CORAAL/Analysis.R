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
                    aint = query2col(aint.utts),
                    lines.Count = query2col(coraal)
                    )

# Create Extra Columns For Ratios 
# utt / word_count
# utt / total utts
md <- md %>% mutate(be.rat.lines = be / lines.Count,
                    contra.rat.lines = contra / lines.Count,
                    red.rat.lines = red / lines.Count,
                    aint.rat.lines = aint / lines.Count,
                    be.rat.words = be / CORAAL.Word.Count,
                    contra.rat.words = contra / CORAAL.Word.Count,
                    red.rat.words = red / CORAAL.Word.Count,
                    aint.rat.words = aint / CORAAL.Word.Count
)


# Create Subcategories Based on Metadata
md.recent.int <- md %>% filter(Year.of.Interview > 2000)
md.old.int <- md %>% filter(Year.of.Interview <= 2000)

md.adults.int <- md %>% filter(Age > 20)
md.children.int <- md %>% filter(Age <= 20)

md.young.birth <- md %>% filter(Year.of.Birth <= 1962)
md.old.birth <- md %>% filter(Year.of.Birth > 1962)

summary(md)

# Usage of Variable will be on the Y axis 
# Age will be the X axis

#Plot 1: Old interviews where age of interviewee varies 
#(How did people of Age X Speak in Interviews before 2000)
ggplot(md.old.int, mapping = aes(Age, be)) + geom_point()

#Plot 2: Recent interviews where age of interviewee varies
ggplot(md.recent.int, mapping = aes(Age, be)) + geom_point()

#Plot 3: Young interviewees (by DOB) are grouped by interview date

#Plot 4: Older interviewees (by DOV) grouped by interview 

ggplot(md.recent.int, aes(Age)) + geom_density()

# Plotting multiple values on a graph
g1 <- geom_point(aes(x = md.old.int$Age, y = md.old.int$be.rat.lines, color='Interviews before 2000', ))
g2 <- geom_point(aes(x= md.recent.int$Age, y = md.recent.int$be.rat.lines, color='Interviews after 2000'))
ggplot() + g1 + g2 + labs(title="Usage of Invariant 'be' given a subject's age",
                          x = "Age When Interviewed",
                          y = "Ratio of be to ")

md.old.int$InterviewTime = "before 2000"
md.recent.int$InterviewTime = "after 2000"
md.combo = bind_rows(md.old.int,md.recent.int)
ggplot(md.combo, aes(x = Age, y = be.rat.lines, color=InterviewTime)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Usage of Invariant 'be' given a subject's age",
       x = "Age When Interviewed",
       y = "Ratio of be to ")

theme_set(theme_bw())
  
  geom_point(aes(x = md.old.int$Age, y = md.old.int$be.rat.lines, color='Interviews before 2000', ))
g2 <- geom_point(aes(x= md.recent.int$Age, y = md.recent.int$be.rat.lines, color='Interviews after 2000'))
ggplot() + g1 + g2 + labs(title="Usage of Invariant 'be' given a subject's age",
                          x = "Age When Interviewed",
                          y = "Ratio of be to ")



# Plotting Birth Age and Usage
g3 <- geom_point(aes(x = md.recent.int$Year.of.Birth, y = md.recent.int$be.rat.lines, color="Recent Interviews"))
g4 <- geom_point(aes(x = md.old.int$Year.of.Birth, y = md.old.int$be.rat.lines, color="Old Interviews"))
ggplot() + g3 + g4 + labs(title="Rate of Invariant 'be' per line given a subject's age",
                          x = "Age of Subject",
                          y = "Amount of 'be' usage ")

g5 <- geom_point( aes(x = md$Year.of.Birth, y = md$be.rat.lines))
ggplot() + g5 + labs(title="Usage of Invariant 'be' given a subject's age",
                     x = "Birth Year of Subject",
                     y = "Ratio of 'be' usage by lines")
# Plot  

g6 <- geom_point( aes(x = md$Year.of.Birth, y = md$red.rat.lines))
ggplot() + g6 + labs(title="Usage of reduction given a subject's age",
                     x = "Birth Year of Subject",
                     y = "Ratio of 'be' usage by lines")

g7 <- geom_point( aes(x = md$Year.of.Birth, y = md$aint.rat.lines))
ggplot() + g7 + labs(title="Usage of ain't given a subject's age",
                     x = "Birth Year of Subject",
                     y = "Ratio of 'be' usage by lines")

g8 <- geom_point( aes(x = md.recent.int$Age, y = md.recent.int$be.rat.lines))
ggplot() + g8 + labs(title="Usage of be given a subject's age (recent)",
                     x = "Birth Year of Subject",
                     y = "Ratio of 'be' usage by lines")
