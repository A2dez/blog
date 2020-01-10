library(tidyverse)
library(ggmosaic)
library(viridis)
library(plotly)

gg <- ggplot(data = fly) +
  geom_mosaic(aes(x = product(DoYouRecline, RudeToRecline), fill=DoYouRecline), na.rm=TRUE) + 
  labs(x = "Is it rude recline? ", title='f(DoYouRecline | RudeToRecline) f(RudeToRecline)')
ggplotly(gg)

glimpse(fly)
#strip unstressed vowels off the end of stressPatterns
foot = as.factor(sub("[0]+$", "", words$stressPattern))
  
#make a dfrm                
data <- bind_cols(sp = words$stressPattern, 
             foot = foot)

data$stressFree <- str_extract(words$stressPattern, "[0]+$")

data$stressFree [is.na(data$stressFree )] <- 'F'

data$stressFree  <- fct_recode(data$stressFree, 
                               'F0' = '0', 'F00' = '00', 'F000' = '000') %>% 
  fct_relevel(c('F', 'F0', 'F00', 'F000'))

data

t <- table( data$stressFree, data$foot)
z <- ifelse (t> 0, log(t), t)

dim(t)



mt <- matrix(sample(1:20), ncol = 2)
rowsum(t, 4)

mosaicplot(t(t), 
     col = viridis(4, direction = -1, option = 5), 
     las = 2, bg = 'black')

foo <- read.table(text=t)
foo[order(foo$V1),]

par(bg
ggplot(data = data) +
  geom_mosaic(aes(x = product(stressFree, foot ), fill=stressFree)) +
  coord_flip()

+ 
  labs(x = "Is it rude recline? ", title='f(DoYouRecline | RudeToRecline) f(RudeToRecline)')



# NOT RUN {
require(stats)

x <- matrix(runif(100), ncol = 5)
group <- sample(1:8, 20, TRUE)
(xsum <- rowsum(x, group))
## Slower versions
tapply(x, list(group[row(x)], col(x)), sum)
t(sapply(split(as.data.frame(x), group), colSums))
aggregate(x, list(group), sum)[-1]
# }