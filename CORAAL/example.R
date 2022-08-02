code:

library(tidyverse)

theme_set(theme_bw(18))

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
"#D55E00", "#CC79A7")

# basic definition of softmax (doesn't work if alpha is 0 because of
division by 0)
softmax = function(scores, alpha) {
   p = exp(alpha*scores) / sum(exp(alpha*scores))
   return(p)
}

# EXAMPLE 1: REFERENCE GAME WITH MANY UTTERANCES

# utterances: big, small, red, blue, big red, small red, small blue
# objects: o_big_red, o_small_red, o_small_blue
utterances = c("big", "small", "red", "blue", "big red", "small red",
"small blue")

# literal listener for o_big_red
o_big_red = c(1,0,.5,0,1,0,0)
o_small_red = c(0,.5,.5,0,0,1,0)
o_small_blue = c(0,.5,0,1,0,0,1)


# assume speaker wants to communicate big red object
test_scores = log(o_big_red)
test_scores
test_alpha = c(0.0001, 0.5, 1, 2, 5, 1000)

d = tibble(utterances =
as.character(),l_zero_big_red=as.numeric(),scores_big_red=as.numeric(),alpha=as.numeric(),softmaxed_big_red=as.numeric())

for (a in test_alpha) {
   new_d = tibble(utterances = utterances,l_zero_big_red=o_big_red) %>%
     mutate(scores_big_red = log(new_d$l_zero_big_red),
            alpha = a) %>%
     mutate(softmaxed_big_red = softmax(scores_big_red,a))
   d = d %>%
     bind_rows(new_d)
}

d = d %>%
   mutate(utterances = fct_relevel(utterances,"big","big red","red"),
          alpha = as.factor(round(alpha,3)))

ggplot(d, aes(x=utterances,y=softmaxed_big_red,color=alpha,group=alpha)) +
   geom_point(size=1) +
   geom_line(size=1) +
scale_color_manual(name="Alpha",values=hcl.colors(length(unique(d$alpha))))
+
   xlab("Utterance") +
   ylab("Probability") +
   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("graphs/probs_many_utterances.pdf",width=7.5,height=4.5)

# pre softmax
d_presoftmax = unique(d %>%
select(utterances,l_zero_big_red,scores_big_red)) %>%
   pivot_longer(l_zero_big_red:scores_big_red,names_to = "value_type") %>%
   mutate(value_type = fct_recode(value_type,"L0
probability"="l_zero_big_red", "L0 score"="scores_big_red")) %>%
   mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) #
replacenegative infinity values with NA to avoid ggplot trying to plot

infs = tibble(utterances=c("blue","small","small blue","small
red"),Label=c("-Inf","-Inf","-Inf","-Inf"))

ggplot(d_presoftmax, aes(x=utterances)) +
   geom_point(size=1,aes(y=value,color=value_type,group=value_type)) +
   geom_line(size=1,aes(y=value,color=value_type,group=value_type)) +
   ylim(-1,1.2) +
   xlab("Utterance") +
   ylab("Value") +
   scale_color_manual(name="Value type",values=cbPalette[1:2]) +
   geom_text(data=infs,aes(label=Label,y=-.8),color="#56B4E9") +
   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("graphs/pre_softmax_many_utterances.pdf",width=7.5,height=4.5)

# EXAMPLE 2: REFERENCE GAME WITH FEW UTTERANCES

# utterances: big, small, red, blue
# objects: o_big_red, o_small_red, o_small_blue
utterances = c("big", "small", "red", "blue")

# literal listener for o_big_red
o_big_red = c(1,0,.5,0)
o_small_red = c(0,.5,.5) # not used thus far
o_small_blue = c(0,.5,0,1) # not used thus far


# assume speaker wants to communicate big red object
test_scores = log(o_big_red)
test_scores
test_alpha = c(0.0001, 0.5, 1, 2, 5, 1000)

d = tibble(utterances =
as.character(),l_zero_big_red=as.numeric(),scores_big_red=as.numeric(),alpha=as.numeric(),softmaxed_big_red=as.numeric())

for (a in test_alpha) {
   new_d = tibble(utterances = utterances,l_zero_big_red=o_big_red) %>%
     mutate(scores_big_red = log(new_d$l_zero_big_red),
            alpha = a) %>%
     mutate(softmaxed_big_red = softmax(scores_big_red,a))
   d = d %>%
     bind_rows(new_d)
}

d = d %>%
   mutate(utterances = fct_relevel(utterances,"big","red"),
          alpha = as.factor(round(alpha,3)))

ggplot(d, aes(x=utterances,y=softmaxed_big_red,color=alpha,group=alpha)) +
   geom_point(size=1) +
   geom_line(size=1) +
scale_color_manual(name="Alpha",values=hcl.colors(length(unique(d$alpha))))
+
   xlab("Utterance") +
   ylab("Probability") +
   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("graphs/probs_few_utterances.pdf",width=7.5,height=4.5)

# pre softmax
d_presoftmax = unique(d %>%
select(utterances,l_zero_big_red,scores_big_red)) %>%
   pivot_longer(l_zero_big_red:scores_big_red,names_to = "value_type") %>%
   mutate(value_type = fct_recode(value_type,"L0
probability"="l_zero_big_red", "L0 score"="scores_big_red")) %>%
   mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) #
replacenegative infinity values with NA to avoid ggplot trying to plot

infs = tibble(utterances=c("blue","small"),Label=c("-Inf","-Inf"))

ggplot(d_presoftmax, aes(x=utterances)) +
   geom_point(size=1,aes(y=value,color=value_type,group=value_type)) +
   geom_line(size=1,aes(y=value,color=value_type,group=value_type)) +
   ylim(-1,1.2) +
   xlab("Utterance") +
   ylab("Value") +
   scale_color_manual(name="Value type",values=cbPalette[1:2]) +
   geom_text(data=infs,aes(label=Label,y=-.8),color="#56B4E9") +
   theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
ggsave("graphs/pre_softmax_few_utterances.pdf",width=7.5,height=4.5)