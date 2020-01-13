
# AIM = TO PLOT ALL THE VOWEL PHONEMES AGAINST THE VOWEL LETTERS IN MONOSYLLABIC ENGLISH WORDS
#Very rough first draft:
#I'm not worrying for now about Y, W, R or GH or the rest of the crew
library(tidyverse)
library(RColorBrewer)

#import dataset
words <- read_csv('Carnegie Mellon_Enable2k Words only.csv')

#the 41,000 words under investigation
glimpse(words)

#extract just monosyllables #5,478 (5448 if we exclude 0s and 2s)
oneSyl <- filter(words, 
                 stressPattern == "'1" | #monosyllabic words
                   stressPattern ==  "'0" | #monosyllabic words with no stress: e.g. 'a', 'the'
                   stressPattern == "'2") %>% #monosyllabic words which have been
  #incorrectly marked as having secondary stress
  #keep the spelling and the phonological form only
  select(c('word', 'phonForm')) 

#remove 0/1/2 from the phonForm (That's the number marking the stress)
oneSyl$phonForm <-  str_replace(oneSyl$phonForm, '0|1|2', "")
glimpse(oneSyl)
# View(oneSyl)
#
# #extract all the vowel letters (ie any number of AEIOU)
oneSyl$vowels <- c(str_match(oneSyl$word, "[AEIOU]+") )
#I'm not worrying for now about Y, W, R or GH or the rest of the crew

#turn them into a table, and convert Frequencies to percentages
oneV <- as.data.frame(
  table(oneSyl$vowels )%>% prop.table * 100) 


#first letter of the spelling unit
oneV$firstLetter <- c(str_match(oneV$Var1, "[AEIOU]") ) %>% 
  factor 
  
oneV <- arrange(oneV, desc(Freq))


#barplot descending #exploratory
 
  barplot(oneV$Freq, 
  las = 1,  horiz = T, cex.names = 0.5, 
  main = 'Frequency of vowel spellings in English monosyllables', 
  xlab = 'Percentage', 
  names.arg = oneV$Var1, 
  col = oneV$firstLetter
  )

  
  
  
  
  
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # $NOW TO EXTRACT ALL THE PHONEMES
  
#all the phonemes
space <- str_split(oneSyl$phonForm, ' ') %>% unlist #separate phonemes by spaces (as in the data)
length(space) # 19947 phonemes
#HOW MANY phonemes
unique(space) #39

#Frequency of phonemes
phonemes <- as.data.frame (table(space) %>% prop.table *100)

phonemes <- arrange(phonemes, desc(Freq)) #arrange into ordere of frequency

#make factor variable
C <- 'C'
V <- 'V'
phoneme_VarC <- c(rep(C, 9), V, C, V, C, 
                       rep(V, 5), C, V,
                       C, rep(V, 3), rep(C, 5), 
                       V, C, C, V, C, C, V, V, C, 
                       C) 
length(phoneme_VarC)

phoneme_VarC <- as.factor(phoneme_VarC)

#make a dfrm
phonemes <- bind_cols(phoneme = phonemes$space, Freq = phonemes$Freq, VarC = phoneme_VarC)

glimpse(phonemes)
#Frequency of phonemes in English monosyllables
barplot(phonemes$Freq ,
        las = 1,  horiz = T, 
        cex.names = 0.5,  #legible, somewhat
        col = phonemes$VarC, #vowels in red, 
        names.arg = phonemes$phoneme,
        main = 'Frequency of phonemes in English monosyllables', 
        xlab = 'Percentage')


#lets work out how to search for all vowel phonemes
unique
# #extract all the Vowel phonemes
oneSyl$Vphon <- c(str_match(oneSyl$phonForm, "EY|AA|
          AE|AY|EH|AO|ER|IY|
          |IH|OW|ER|UW|AH|AW|OY|UH") )

glimpse(oneSyl)
col <- brewer.pal(n = 12, name = "Paired")
g2p <- table(oneSyl$Vphon, oneSyl$vowels)

#next job is to manually change the order of the vowel lettes


g2p <- g2p[c("ER", "AY","IH","IY", "EH","EY","AA",
              
               "AO",
               "OW",  "AH","UH", "UW","AW", "OY"),
           oneV$Var1[c(2, 5, 6, 7, 1, 3,8, 9, 4,13, 10:12, 14:34)]] #A, E, I, O, U, most common

dim(g2p)
par(bg = 'white')
#mosaic plot 
mosaicplot(g2p, 
           las = 1, 
           col = c(col, 1:22), 
           main = 'First draft of Vowel Correspondences')


# 'A',   'I', '' O    U    E    EA   EE   OO   OU   AI  
# [11] IE   OA   AU   EI   OI   UI   UE   UA   OE   AA  
# [21] AE   EAU  EU   UEE  AO   IA   OOE  UAI  UEA  UO  
# [31] IAO  IEU  OEU  UEUE
# oneV$Var1