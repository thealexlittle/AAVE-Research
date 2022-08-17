# Library Imports
library(tidyverse)
library(lme4)

theme_set(theme_bw())


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
# be.utts <- coraal.search(p.be)
contraction.utts <- coraal.search(p.contractions)
red.utts <- coraal.search(p.reductions)
aint.utts <- coraal.search(p.aint)

# Read in be utts from handcoded csv
be.utts <- read.csv('b.csv')

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
                    aint.rat.words = aint / CORAAL.Word.Count,
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

ggplot(md.recent.int, aes(Age)) + geom_histogram()


#Plot use of be in interviewees of a certain age between 1960s and 2010s
md.old.int$InterviewTime = "before 2000"
md.recent.int$InterviewTime = "after 2000"
md.combo = bind_rows(md.old.int,md.recent.int)

# Plot Age when interviewed and usage of Be
ggplot(md.combo, aes(x = Age, y = be.rat.lines, color=InterviewTime)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Usage of Invariant 'be' given a subject's age",
       x = "Age When Interviewed",
       y = "Ratio of be to lines")
lm.age.be <- lm(Age ~ be.rat.lines, data = md.combo )
summary(lm.age.be)

ggsave('plots/UsageOf_Be_AgainstInterviewAge.png')

# Plotting Birth Age and Usage of Be
ggplot(md.combo, aes(x = Year.of.Birth, y = be.rat.lines, color=InterviewTime)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = "Usage of Be compared to Year of Birth",
        x = "Year of Birth",
        y = "Useage of Be")

ggsave('plots/UsageOf_Be_AgainstBirthYear.png')

lm.DOB.be <- lm(Age ~ be.rat.lines, data = md.combo )
summary(lm.DOB.be)

# Plot Year of birth against reduction usage
ggplot(md.combo, aes(x = Year.of.Birth, y = red.rat.lines, color=InterviewTime)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title="Usage of reduction given a subject's age", 
       x = "Birth Year of Subject", 
       y = "Ratio of reduction by lines")

ggsave('plots/UsageOf_Red_AgainstBirthYear.png')

ggplot(md.combo, aes(x = Age, y = red.rat.lines, color=InterviewTime)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title="Usage of reduction given a subject's age when interviewed", 
       x = "Age of Subject", 
       y = "Ratio of reduction by lines")

lm.DOB.red <- lm(Age ~ be.rat.lines, data = md.combo )
summary(lm.DOB.red)

ggsave('plots/UsageOf_Red_AgainstInterviewAge.png')

# Plot use of Year of birth against aint usage
ggplot(md.combo, aes(x = Year.of.Birth, y = aint.rat.lines, color=InterviewTime)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title="Usage of aint given a subject's age", 
       x = "Birth Year of Subject", 
       y = "Ratio of 'aint' usage by lines")

ggsave('plots/UsageOf_Aint_AgainstBirthYear.png')

ggplot(md.combo, aes(x = Age, y = aint.rat.lines, color=InterviewTime)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title="Usage of aint given a subject's age", 
       x = "Age of Subject when interviewed", 
       y = "Ratio of 'aint' usage by lines")

lm.DOB.aint<- lm(Age ~ be.rat.lines, data = md.combo )
summary(lm.DOB.aint)

ggsave('plots/UsageOf_Aint_AgainstInterviewAge.png')





