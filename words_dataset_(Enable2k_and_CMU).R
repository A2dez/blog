
# -------------
#   Next job: Remove names from the dataset
# by merging the cleaned data with a large wordlist, Enable2k
# ------------

#import Carnegie Mellon Pronouncing Dictionary (from above)
cmu <- read_csv('Carnegie Mellon Cleaned.csv')
#import enable2k dataset
k2 <- read_csv(file = 'Enable2k.csv')
k2
#make upper case so they join
k2$aa <- toupper(k2$aa)
words <- inner_join( cmu, k2, by = c("word" = "aa"), copy = T) #https://dplyr.tidyverse.org/reference/join.html



# ---------------------------------------
# some Very specific Lemmatization

#Goal: to capture all past tense 'ED' forms which have no extra syllable
# and then create a new variable to plug into the algorithm 
#this will be useful for looing at 'orthographic syllables where there is no phonological equivalent

#This captures all words which:
# end in ED, but not EED (e.g. birdseed, freed) &
#which don't end in /ID/
# a Zero (unsressed syllable) or nothing is created in the variable column
variable <- ifelse(str_detect(words$word,'[^E]ED$') == T & 
                     str_detect(words$phonForm,'H0 D$') ==F, 
                   '0', '')
 sum(variable == '0') #3716
table(variable)

#and this is then pasted onto stress pattern, 
words$stressAlgo <- paste(words$stressPattern, variable) %>% 
  str_replace(" ", "") #with the ensuing whitespace being removed


# %%%%%%%%%
# Gaol: remove plural S
# ==========

# create a vector where every word has S added 
Sexpress <- paste(words$word, 'S') %>% 
  str_replace(" ", "") #and the unwanted extra space removed
Sexpress <- as.data.frame(Sexpress) #dfrm it

#anti_join the base DFRM with the new vector
#magically, all of the plural forms are diagonalized out, at a Cantor. 
words <- anti_join(words, Sexpress, by = c('word' = 'Sexpress'), copy = T)

#this reduces the data from 55k to 41k. 
#PROBLEM 
#this deletes 24 words such as 'bridges' and other words which end in S but end in /IH Z/ 
#need to add a condition to keep cases where a syllable is rightfully added
#we could simply


# DUPLICATES
#this removes all duplicate words EXCEPT those with different stress patterns
words <- words %>% distinct(word, stressPattern, .keep_all = TRUE)

# 109,526 REDUCED TO 105,144
glimpse(words) #down to 41,107!


# 
# __________________________
# Hyphenation time
# __________________________

# install.packages('sylly.en')
library(sylly.en) 
#this is a rare package used for hyphenating English words
# https://github.com/unDocUMeantIt/sylly/blob/master/R/sylly-internal.R

# #here is a sample for the uninitiated
# sampleText <- c("This", "is", "a", "rather", "stupid", "demonstration")
# sampleHyph<- hyphen_df(sampleText, hyph.pattern="en")
# 
# # > sampleHyph
# syll             word
# 1    1             This
# 2    1               is
# 3    1                a
# 4    2          rath-er
# 5    2          stu-pid
# 6    4 de-mon-stra-tion
# # 

#run the vector through Liang's hyphenator
words_hyph <- hyphen_df(words$word, hyph.pattern="en") # this is mighty (slow)
# returns a dfrm which needs to be added to the base dfrm
names(words_hyph) <- c('hyphSylls', 'hyphed' )
words <- cbind(words, words_hyph)


#Time to convert words to CV form
words$hyphCV <- gsub ('[QWRTYPLKJHGFDSZXCVBNM]', 'c' , words$hyphed) 
words$hyphCV <- gsub ('[AEIOU]', 'v' , words$hyphCV)

glimpse(words)
write_csv(words, 'Carnegie Mellon_Enable2k Words only.csv')
# A bunch of these words don't have two hyphens due to mismatches between the hyphenation algorithm
# and the CMU dictionary (compare their bi-syllabic 'actually' with the hyphenator's 'ac-tu-al-ly')


# ---------------------------------------------------------
#Subset into bisyllables etc. 
# ---------------------------------------------------------

#otherwise I would have to run it through the mighty (slow) hyphenator
glimpse(twoSyl) #13,881 words
twoSyl <- filter(words, hyphSylls == 2 & noSyls == 2) 
#this gets rid of mismatches between the original and the column that went through the hyphenator
# we need this for below (not the version saved to CSV)

#split the CVed hyphenated form into two parts 
letsPerSyl <- as.data.frame(str_split_fixed(twoSyl$hyphCV, '-', 2)) # 2 means two columns
names(letsPerSyl)  <- c('CVs1', 'CVs2') #rename

twoSyl <- cbind(twoSyl, letsPerSyl ) #reattach to bisyllables 

twoSyl$noCVs1 <- nchar(as.character(twoSyl$CVs1))
twoSyl$noCVs2 <- nchar(as.character(twoSyl$CVs2))
levels(as.factor(twoSyl$stressPattern))

twoSyl$stress1 <- gsub('[2]', '1', twoSyl$stressPattern) #reduce to just stressed & unstressed syllables
twoSyl$stress1  <- as.factor(twoSyl$stress1)

glimpse(twoSyl)

write.csv(twoSyl, file = 'Bisyllables of English_two_hyphens_only.csv') #this was for my own storage.

# ---------------------------------------------------------
#Subset into trisyllables now 
# ---------------------------------------------------------

glimpse(threeSyl) #9,492 words
threeSyl <- filter(words, hyphSylls == 3 & noSyls == 3) 
#this gets rid of mismatches between the original and the column that went through the hyphenator
# we need this for below (not the version saved to CSV)

#split the CVed hyphenated form into two parts 
letsPerSyl <- as.data.frame(str_split_fixed(threeSyl$hyphCV, '-', 3)) # 2 means two columns
names(letsPerSyl)  <- c('CVs1', 'CVs2', 'CVs3') #rename

threeSyl <- cbind(threeSyl, letsPerSyl ) #reattach to bisyllables 

threeSyl$noCVs1 <- nchar(as.character(threeSyl$CVs1))
threeSyl$noCVs2 <- nchar(as.character(threeSyl$CVs2))
threeSyl$noCVs3 <- nchar(as.character(threeSyl$CVs3))
levels(as.factor(threeSyl$stressPattern))

threeSyl$stress1 <- gsub('[2]', '1', threeSyl$stressPattern) #reduce to just stressed & unstressed syllables
threeSyl$stress1  <- as.factor(threeSyl$stress1)

glimpse(threeSyl)

write.csv(threeSyl, file = 'Trisyllables of English_three_hyphens_only.csv') #this was for my own storage.

# -------------------------------------------------

#Subset into quadrisyllables now 
# ---------------------------------------------------------

glimpse(fourSyl) #4,705 words
fourSyl <- filter(words, hyphSylls == 4 & noSyls == 4) 
#this gets rid of mismatches between the original and the column that went through the hyphenator
# we need this for below (not the version saved to CSV)

#split the CVed hyphenated form into two parts 
letsPerSyl <- as.data.frame(str_split_fixed(fourSyl$hyphCV, '-', 4)) # 2 means two columns
names(letsPerSyl)  <- c('CVs1', 'CVs2', 'CVs3', 'CVs4') #rename

fourSyl <- cbind(fourSyl, letsPerSyl ) #reattach to bisyllables 

fourSyl$noCVs1 <- nchar(as.character(fourSyl$CVs1))
fourSyl$noCVs2 <- nchar(as.character(fourSyl$CVs2))
fourSyl$noCVs4 <- nchar(as.character(fourSyl$CVs3))
fourSyl$noCVs3 <- nchar(as.character(fourSyl$CVs4))
levels(as.factor(fourSyl$stressPattern))

fourSyl$stress1 <- gsub('[2]', '1', fourSyl$stressPattern) #reduce to just stressed & unstressed syllables
fourSyl$stress1  <- as.factor(fourSyl$stress1)

glimpse(fourSyl)

write.csv(fourSyl, file = 'Quadrisyllables of English_four_hyphens_only.csv') #this was for my own storage.

# -------------------------------------------------
#Subset into quadrisyllables now 
# ---------------------------------------------------------
glimpse(fiveSyl) #1,848 words
fiveSyl <- filter(words, hyphSylls == 5 & noSyls == 5) 
#this gets rid of mismatches between the original and the column that went through the hyphenator
# we need this for below (not the version saved to CSV)

#split the CVed hyphenated form into two parts 
letsPerSyl <- as.data.frame(str_split_fixed(fiveSyl$hyphCV, '-', 5)) # 2 means two columns
names(letsPerSyl)  <- c('CVs1', 'CVs2', 'CVs3', 'CVs4', 'CVs5') #rename

fiveSyl <- cbind(fiveSyl, letsPerSyl ) #reattach to bisyllables 

fiveSyl$noCVs1 <- nchar(as.character(fiveSyl$CVs1))
fiveSyl$noCVs2 <- nchar(as.character(fiveSyl$CVs2))
fiveSyl$noCVs3 <- nchar(as.character(fiveSyl$CVs3))
fiveSyl$noCVs4 <- nchar(as.character(fiveSyl$CVs4))
fiveSyl$noCVs5 <- nchar(as.character(fiveSyl$CVs5))
levels(as.factor(fiveSyl$stressPattern))

fiveSyl$stress1 <- gsub('[2]', '1', fiveSyl$stressPattern) #reduce to just stressed & unstressed syllables
fiveSyl$stress1  <- as.factor(fiveSyl$stress1)

glimpse(fiveSyl)

write.csv(fiveSyl, file = 'Pentasyllables of English_five_hyphens_only.csv') #this was for my own storage.

# -------------------------------------------------
# Create names for testing

words <- read_csv( 'Carnegie Mellon_Enable2k Words only.csv')
glimpse(words)
names <- anti_join(cmu, words, by = c('word' = 'word'), copy = T)

# -------------------------------------------------
twoNames <- filter(names, noSyls == 2)


#run the vector through Liang's hyphenator
twoNames_hyph <- hyphen_df(twoNames$word, hyph.pattern="en") # this is mighty (slow)
# returns a dfrm which needs to be added to the base dfrm
names(twoNames_hyph) <- c('hyphSylls', 'hyphed' )
twoNames <- cbind(twoNames, twoNames_hyph)

#Time to convert words to CV form
twoNames$hyphCV <- gsub ('[QWRTYPLKJHGFDSZXCVBNM]', 'c' , twoNames$hyphed) 
twoNames$hyphCV <- gsub ('[AEIOU]', 'v' , twoNames$hyphCV)

#otherwise I would have to run it through the mighty (slow) hyphenator
glimpse(twoNames) #40,822  words
table(twoNames$stressPattern)
twoNames <- filter(twoNames, hyphSylls == 2 & noSyls == 2) 
#this gets rid of mismatches between the original and the column that went through the hyphenator
# we need this for below (not the version saved to CSV)


#split the CVed hyphenated form into two parts 
letsPerSyl <- as.data.frame(str_split_fixed(twoNames$hyphCV, '-', 2)) # 2 means two columns
names(letsPerSyl)  <- c('CVs1', 'CVs2') #rename
glimpse(letsPerSyl)
twoNames <- bind_cols(twoNames, letsPerSyl ) #reattach to bisyllables 

twoNames$noCVs1 <- nchar(as.character(twoNames$CVs1))
twoNames$noCVs2 <- nchar(as.character(twoNames$CVs2))
levels(as.factor(twoNames$stressPattern))

twoNames$stress1 <- gsub('[2]', '1', twoNames$stressPattern) #reduce to just stressed & unstressed syllables
twoNames$stress1  <- as.factor(twoNames$stress1)

glimpse(twoNames)

write.csv(twoNames, file = 'Bisyllabic names of English_two_hyphens_only.csv') #this was for my own storage.



# -------------------------------------------------

