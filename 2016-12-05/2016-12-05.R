### kody ze spotkania

library(sjPlot)
library(VIM)
library(Amelia)
library(dplyr)

load('datasets/bkl_2011_2013.RData')

dane_2011 <- dane[[1]]

summary(dane_2011[,1:2])

## stosujemy funkcję z pakietu sjPlot
view_df(x = dane_2011,
        show.labels = FALSE,
        show.values = FALSE,
        show.na = TRUE)

podzbior <- select(dane_2011,
                   kobieta,wydzial1,
                   datacollection_status1,
                   p51,srednia)

summary(podzbior)

### z pakietu Amelia

missmap(podzbior)

## pakiet VIM

a <- aggr(podzbior,plot = F)
summary(a)
plot(a)

## pakiet VIM
df <- podzbior[,c('srednia',
                  'datacollection_status1')]
df <- as.data.frame(df)

histMiss(x = df)















