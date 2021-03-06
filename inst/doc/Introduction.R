## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(BayesTestStreak)

## ------------------------------------------------------------------------
trout_id <- find_id("Mike Trout")

## ------------------------------------------------------------------------
(y_PA <- streak_data(pbp2016, trout_id, 20:23))

## ------------------------------------------------------------------------
y_PA <- streak_data(pbp2016, trout_id, "H")

## ------------------------------------------------------------------------
y_AB <- streak_data(pbp2016, trout_id, "H", AB=TRUE)

## ------------------------------------------------------------------------
mavg_plot(y_AB, width=40)

## ------------------------------------------------------------------------
(s <- find.spacings(y_AB)$y)

## ------------------------------------------------------------------------
geometric.plot(s)

## ------------------------------------------------------------------------
permutation.test(y_AB)

## ------------------------------------------------------------------------
OUT <- bayes.factor.function(y_AB)
ggplot(data.frame(logK=OUT$log.K, logBF=OUT$log.BF), 
       aes(logK, logBF)) + geom_line()

