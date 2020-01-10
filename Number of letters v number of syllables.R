#Letters v syllables now
# ---------------------------
data <- table(words$wordLength, words$noSyls)
f <- table(words$wordLength, words$noSyls) %>% prop.table

#making this Excel / Word readable 
f <- as.matrix(round(100*f, digits = 2))
f <- t(f) #transpose

write.table(f, 'Letters v Syllables.csv')

#Mosaic plot

par(bg = 'black') #the black background is amazebant 
par(col.lab="white", col.axis = 'white', font.axis=4,
    cex.main=1.6)
par(mar = c(6,2,6,0))

#create 8 colours, six viridis and 
# two extra hightly visible ones
v <- c(viridis(6, direction = -1, option = 5), 2, 3)

#base R mosaic for the lads
plot (data, col = v, 
      las = 1,  cex.axis= 2, main = '')

plot(1:5, yaxt = "n")
axis(3, col.axis = 2)

#title, 
title( sub = '#Letters (1...16) ',
       main = 'Number of Letters (x) v Number of Syllables (y)\nin 40,000 English words', 
       col.sub = 'white', col.main = 'white',
       cex.sub = 1.2,
       col.lab = 'white',
       font.main = 4, #bold and italic
       font.sub = 1, 
       font.lab = 4) #not actually visible cos I can't change the x-axis color from black

#create a legend
legend("left",  #on the left 
       title = '#Syllables',
       legend=c(1:8),
       lty = 0, #no lines
       bg = 'black', #black box
       box.lty = 0, #line types
       cex=1.2, 
       ncol = 1, #one column, the default
       text.font = 4, #bold and italic, very 2019
       text.col = 'white',
       fill = v)

# ===================
# Alternate version for LinkedIn

#Letters v syllables now
# ---------------------------
data <- table(words$wordLength, words$noSyls)
f <- table(words$wordLength, words$noSyls) %>% prop.table

#making this Excel / Word readable 
f <- as.matrix(round(100*f, digits = 2))
f <- t(f) #transpose

write.table(f, 'Letters v Syllables.csv')

#Mosaic plot

par(bg = 'black') #the black background is amazebant 
par(col.lab="white", col.axis = 'white', font.axis=4,
    cex.main=1)
par(mar = c(0,0,2,2))


#base R mosaic for the lads
plot (data, col = v, 
      las = 1,  cex.axis= 2, main = '')

#title, 
title( sub = '',
       main = 'Number of Letters (x) v Number of Syllables (y), in 40,000 English words', 
       col.sub = 'white', col.main = 'white',
       cex.sub = 1.2,
       col.lab = 'white',
       font.main = 4, #bold and italic
       font.sub = 1, 
       font.lab = 4) #not actually visible cos I can't change the x-axis color from black

#create a legend
legend("right",  #on the left 
       title = '#Syllables',
       legend=c(1:8),
       lty = 0, #no lines
       bg = 'black', #black box
       box.lty = 0, #line types
       cex=0.8, 
       ncol = 1, #one column, the default
       text.font = 4, #bold and italic, very 2019
       text.col = 'white',
       fill = v)

par(bg = 'white')
# plot(table(words$stressPattern))
# setdiff(unique(words$stressPattern), unique(b)  )
# 
# # https://r.789695.n4.nabble.com/Truncating-leading-zeros-in-strings-td2966968.html
# words$foot <- as.factor(sub("[0]+$", "", words$stressPattern))
# unique(words$foot)              
# head(words)
# b <- table(words$stressPattern, words$foot)
# b <- as.data.frame(table(words$stressPattern))
# b <- arrange(b, desc(-b$Freq)) %>% filter (Freq >10)
# 
# library(viridis)
# barplot(b$Freq, horiz = T, 
#         col = as.factor(words$foot))



library(vcd)
library(tidyverse)
library(viridis)

library(RColorBrewer)


#create matrix to plot bisyllable length likelihood

#need to create a table showing the sum of h + h[i]

#remove ED forms from the dataset
wordsEDless <- words[str_detect(words$word,'[^E]ED$') == F,]


g <- table(wordsEDless$wordLength, wordsEDless$noSyls)
h <- g[1:8,1]
# code nabbed from https://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop/13442634
iterations = length(h)
variables = length(h)

#define the matrix
biSyl <- matrix(ncol=variables, nrow=iterations)

#create matrix where every column is the sum of h + the i'th row
for(i in 1:iterations){
  biSyl[,i] <- h +h[i]
}

#names for hte lads
rownames(biSyl)= c(1:length(h))
colnames(biSyl)= c(1:length(h))

#create colour scheme
col <- c( viridis(5), viridis (5, direction = -1))
col <- col[c(1:5,:8)]
mosaicplot(biSyl,
           main = 'Possible lengths of bisyllabic compounds',
           xlab = 'Number of letters in first syllable',
           ylab = 'Number of letters in second syllable',
           cex.axis = 1.2, 
           border = 'black',
           col = viridis(4),
           las = 1, 
           off = c(5,20))

#compare the above against the actual likely values
biSylReal <- table(x = twoSyl$noCVs1, y = twoSyl$noCVs2)

par(bg = 'white')
mosaicplot(t(biSylReal),
           main = 'The actual length of bisyllabic words',
           ylab = 'Number of letters in first syllable',
           xlab = 'Number of letters in second syllable',
           col = col, 
           cex.axis = 1.2,
           las = 1,
           border = 'black',
           type = 'pearson', off = 20 )

#generate a matrix of the row number + column number
m <- outer(1:length(h),1:length(h),function(i,j) i + j)
mos <- cbind(gather(as.data.frame(biSyl)), letters = gather(as.data.frame(m))[,2])

dim(table(mos$key, mos$value))


# ^^^^^^^^^^^^^^^^]


iterations = 2*length(h)
variables = length(h)

#define the matrix of 18 x 9 NAs
output <- matrix(ncol=variables, nrow=iterations)

for(i in 1:variables){
  output[,i] <- c(rep(0,i), #disqualified before
                  h+h[i], 
                  rep(0,length(h)-i))
}

output

write.csv(output, file = "output.csv")

#do a bit of handballing in Excel to turn values into 0 
#for all duplicates (i.e. all values beyond the diagonal)
# i should really be doing this with an ifelser
output <- read.csv(file = 'output.csv')
#names for hte lads
rownames(output)= c(1:16)
colnames(output)= rep(NA, length(h))
par(bg = 'white')
par(mar = c(6,4,4,2))
cols <-c(1, brewer.pal(n = 11, name = "Spectral"), 2:4,7)

mosaicplot(t(output),
           cex.axis = 0.9,
           border = 'white',
           main = 'Possible lengths of bisyllabic compound words',
            ylab = 'Total number of letters',
           xlab = 'Number of letters in first syllable',
           off = c(10, 50),
           col=cols)

#create a legend
legend('bottom', #on the left 
       title = '#Number of letters',
       legend=c('NA',2:16),
       lty = 0, #no lines
       bg = 'white', #white box
       box.lty = 0, #line types
       cex=1,  #text size
       ncol = 4, #one column, the default
       text.font = 4, #bold and italic, very 2019
       text.col = 'black',
       inset=c(0,-0.25),
       fill = cols)
#diverginv palette RColorBrewer
      View( words[1:100, 2])

# Barplot using 
#create colour scheme
coolwarm_hcl <- colorspace::diverging_hcl(18,
                                          h = c(250, 10), 
                                          c = 100, l = c(37, 88),
                                          power = c(0.7, 1.7))
par( las = 1)
par(mar = c(2,2,2,2))
mosaic(t(output), 
       ylab = "", 
       xlab = "", main = "Possible lengths of bisyllabic compound words",



out <- as.data.frame(t(output))
par(bg = 'white')
mosaic(as.matrix(out), 
       col = rownames(output))

gp = gpar(fill = viridis(9))

t(h)
c(rep(0,9-8), h+h[2], rep(0,9-1))

#convert to a dfrm
output <- data.frame(output)

a<-c(1,2,3,NA,4,5,NA,NA)

mean(a, na.rm = T)

a[is.na(a)== 3]
mean_impute<-function(x){
  ifelse(is.na(x),mean(x,na.rm = T),x)
}

barplot(x$Freq)
cor(words$wordLength, words$noSyls)
abline()

names(words)
date()
?geom_mosaic
