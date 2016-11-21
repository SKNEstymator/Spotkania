### 
library(vcd) 
library(ca)
library(factoextra)

load('../Analiza KdG/datasets/bkl_2011_2013.RData')

## wybieram podzbiór
do_tabeli <- subset(x = dane[[1]],
                    select = c(wydzial1,p13,w),
                    subset = wydzial1 != 6)

### agreguję dane
d <- aggregate(formula = w ~ wydzial1 + p13,
               data = do_tabeli,
               FUN = sum)

### sprawdzam tabelę 
wyn <- xtabs(w ~ wydzial1 + p13,
             data = d)

### należy zagregować 

rownames(wyn) <- c('WE','WGM','WIGE','WT','WZ')
colnames(wyn) <-  c('B.zle','źle','wyst','dobrze','b.dobrze')

analiza <- ca(wyn) 
plot(analiza)



