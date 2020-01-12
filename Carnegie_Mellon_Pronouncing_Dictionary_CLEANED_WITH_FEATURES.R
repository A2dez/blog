# Goal: to import the Carnegie Mellon pronouncing dictionary 
# and to feature engineer it for further analysis


# Contents:
#   1. Import & inspect
# 2. Clean
# 3. Feature Engineer
# 4. Merge dataset with Enable 2k in order to remove names / rare words
# 4. subset into relevant groups. 


# The Dataset is the Carnegie Mellon University Pronouncing dictionary
# which can be found here:
# http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/cmudict-0.7b 

              #note that, in the original dataset, the first 126 rows include a copyright notice 
              #and pronunciations for punctuation (e.g. 'question mark')
              
              # 
              # read_delim("https://github.com/Alexir/CMUdict/blob/master/cmudict-0.7b", delim = "  ", skip = 126)
              # THIS IS A PYTHON SCRIPT FOR CONVERTING THE DATA TO A CSV
              # https://github.com/Alexir/CMUdict/blob/master/cmudict-0.7b
              #My method was much messier and involved copy and pasting it via a Text file, which you could also do. 


# Once it is in two columns, it looks like this: 
      # Observations: 133,785
      # Variables: 2
      # $ word     <chr> "A", "A(1)", "A'S", "A.", "A.'S", "A.S", "A42128", "AA", "…
      # $ phonForm <chr> "AH0", "EY1", "EY1 Z", "EY1", "EY1 Z", "EY1 Z", "EY1 F AO1…

#And the goal here will be to clean it and  engineer the following features: 

      # - length of words
      # - number of syllable
      # - the stress pattern
      # - a hyphenated formal
      # - the number of letters in each post-hyphenated syllable, etc. 


              # Observations: 124,300
              # Variables: 8
              # $ word          <chr> "A", "A", "AABERG", "AACHEN", "AACHENER", "AAH", "AAK…
              # $ phonForm      <chr> "AH0", "EY1", "AA1 B ER0 G", "AA1 K AH0 N", "AA1 K AH…
              # $ wordOriginal  <chr> "A", "A(1)", "AABERG", "AACHEN", "AACHENER", "AAH", "…
              # $ wordLength    <int> 1, 1, 6, 6, 8, 3, 5, 7, 7, 6, 6, 7, 8, 9, 5, 5, 6, 8,…
              # $ stressPattern <chr> "'0", "'1", "'10", "'10", "'100", "'1", "'10", "'212"…
              # $ syllables     <int> 1, 1, 2, 2, 3, 1, 2, 3, 2, 2, 2, 3, 2, 2, 1, 2, 2, 3,…
              # $ stress1       <fct> '0, '1, '10, '10, '100, '1, '10, '111, '10, '10, '11,…
              # $ CV            <fct> V, V, VVCVCC, VVCCVC, VVCCVCVC, VVC, VVCVC, VVCVCVC, …

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)
library(sylly.en) #for hyphenating English words

# import the dataset

cmu <- read_csv(file = 'CarnegieData with Stress Patterns copy.csv')
glimpse(cmu)
#just keep these two columns
cmu <- cmu[, c("word", "phonForm")]


#inspect the data
glimpse(cmu)

#view random 
cmu[3456,] #(Amen, as it happens)
#having a look at some middle of the dataset stuff.
cmu[213:245,] 
# 'Abkhazia' and the likes
# notice how the same words get repeated and there are lots of plurals etc.

#Which words contain digits? 
cmu$num <- as.character(str_extract_all(cmu$word, '[:digit:]'))

#which words contain punctuation marks?
cmu$punct <- as.character(str_extract_all(cmu$word, '[:punct:]'))

# Examine the dross
                
                #all instances of punctuation
                table(cmu$punct)
                # _                     -                     . 
                # 75                   865                    66 
                # '      c("_", "_", "_") c("_", "_", "(", ")") 
                # 7349                     1                     1 
                # c("_", "_")           c("_", "'")      c("_", "(", ")") 
                # 5                     6                    11 
                # c("-", "-", "-") c("-", "-", "(", ")")           c("-", "-") 
                # 5                     2                    44 
                # c("-", "'")      c("-", "(", ")")      c(".", ".", ".") 
                # 12                    42                     1 
                # c(".", ".") c(".", "'", "(", ")")           c(".", "'") 
                # 3                     1                    29 
                # c(".", "(", ")")           c("'", "'")      c("'", "(", ")") 
                # 11                    19                   650 
                # c("(", ")")          character(0) 
                # 8061                116519 

                
                #all instances of numerals
                table(cmu$num)
                # 0                          1                          2 
                # 1                       8157                        485 
                # 3                          4                          5 
                # 147                          3                          2 
                # 6                          8                c("0", "0") 
                # 1                          1                          1 
                # c("4", "2", "1", "2", "8")                c("8", "0") c("9", "2", "7", "6", "2") 
                # 1                          1                          1 
                # character(0) 
                # 124977 
                
                #a huge number of words with '1' or '2' and even '3', all for homophonees
                #i inspected the rest and am getting rid of them 


    
          ## in order to remove all non-letters from the column we could simply write:
      # cmu$word <-gsub("[^[:alpha:]]", "", cmu$word)
          ##but this leaves us with many unwanted entries and does not let us explore each group in turn 
          ##hence I have found the problem groups (from the tables above and I'm looking at each of them 

# rows where the 'word' column contains an apostrophe
apostrophes <- str_which(cmu$punct, "'")

#or full stop
fullStops <- str_which(cmu$punct, "\\.")

# anything with a 4, 5, 6, 7, 8 or 9 (e.g. CAT-4, M-80)
multiNum <- str_which(cmu$num, '[4-9]')

#removing underscores
underscore <- str_which(cmu$punct, '_')

#removing hyphens
hyphens <- str_which(cmu$punct, '-')

#removing leftovers 
leftovers <- str_which(cmu$punct, '_", "_"')

# remove misselling: zeroes in W00T for WOOT
misp <- str_which(cmu$num, "\"0\", \"0\"")


# cmu2[c(1, 2, 57445),] #A, A & I

# bringing all the skronk together (also removing overlaps)
byebye <- unique(c(apostrophes, 
                   fullStops, 
                   multiNum, 
                   underscore,
                   leftovers, 
                   hyphens, 
                   misp))
#and deleting it 
cmu <- cmu[-byebye,]
glimpse(cmu) #124,580 rows

# check for leftover numerical dross 
table(cmu$num) #just '1', '2' or '3'
# check for leftover punctuation dross 
table(cmu$punct) #all good except for bracketed words
#remove the unnecessary punctuation column
cmu <- select(cmu, -c(punct))
cmu <- select(cmu, -c(num))

# cmu[cmu$wordLength == cmu$noSyls,]
glimpse(cmu) #124,580

#store original but cleaned vector for later (e.g. studying homophones)
cmu$wordOriginal <- cmu$word

#And now to remove incidence of (1, 2, 3) which are all due to homophony
cmu$word <- gsub("[^[:alpha:]]", "", cmu$word) #i.e. remove all remaining non-letters 


#inspect data for distinct entries / outstanding overlaps
n_distinct(cmu$word) #116507(the rest are homophones, spellings with two or more pronunciations)
n_distinct(cmu$phonForm) #109784 (the rest are heterographs e,g. bare, bear)




# -----------------------------------
# Time to feature engineer now
# -----------------------------------

#create vector with the number of letters in each word
cmu$wordLength <- nchar(cmu$word)

        # note that in the original dataset cmu$V1[35348]  #had to be changed cos R didn't like the weird scramble that was there "D\xc9J\xc0"

# we can add a column with each word's stress pattern.
# This works because Column 2 (phonForm) contains both phonemes AND stress patterns
cmu$stressPattern <-  str_replace_all(cmu$phonForm, "[^0-9]", "") %>% #This retains only the numerals in the strings 
  factor #naturally we want this to be a factor

levels(as.factor(cmu$stressPattern)) # with a whopping 285 levels

#no syllables in each word
cmu$syllables <- nchar(as.character(cmu$stressPattern )) 

#get rid of initialims by keeping only rows where the number of letters is egreater than the number of syllables
# except for th words'A' and 'I'
# 

cmu <-  filter (cmu, as.numeric(cmu$wordLength) > as.numeric(cmu$syllables) | 
                 cmu$word == 'A' | cmu$word == 'I')

glimpse(cmu) #124,300 now

# #I'm sticking an apostrophe before each stress pattern
#because Excel can't deal with the leading zeroes and that you can read back in the data
cmu$stressPattern <- sapply(cmu$stressPattern , 
                     function(x) paste0("'", x))

#this is old code. I would now do this: str_replace(cmu$stressPattern, "^", "'")
#note that the choice of ' was unfortunate and requires me to ALWAYS use "" later in the code


#this removes secondary stress an reduced the problem to just 1s and 0s.
cmu$stress1 <- as.factor(gsub('[2]', '1', cmu$stressPattern))
levels(cmu$stress1) #135, of half the number as before
glimpse(cmu)
ncol(cmu) #7

       #Time to convert words to binary Vowels / Consonants
       cmu$CV <- gsub ('[QWRTYPLKJHGFDSZXCVBNM]', 'C' , cmu$word) #replaces all vowels with 'C'
       cmu$CV <- gsub ('[AEIOU]', 'V' , cmu$CV) #replaces all vowels with 'V'
       cmu$CV  <- as.factor(cmu$CV )
       

     
       
 write.csv(cmu, file = 'Carnegie Mellon Cleaned.csv')
       
       # Observations: 124,300
       # Variables: 8
       # $ row           <dbl> 1, 2, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 26, 27, 30,…
       # $ word          <chr> "A", "A", "AABERG", "AACHEN", "AACHENER", "AAH", "AAKER", "AALIYAH", "AALSETH…
       # $ wordLength    <dbl> 1, 1, 6, 6, 8, 3, 5, 7, 7, 6, 6, 7, 8, 9, 5, 5, 6, 8, 8, 5, 4, 5, 2, 5, 5, 6,…
       # $ stressPattern <chr> "'0", "'1", "'10", "'10", "'100", "'1", "'10", "'212", "'10", "'10", "'12", "…
       # $ noSyls        <dbl> 1, 1, 2, 2, 3, 1, 2, 3, 2, 2, 2, 3, 2, 2, 1, 2, 2, 3, 3, 2, 1, 2, 1, 3, 3, 3,…
       # $ phonForm      <chr> "AH0", "EY1", "AA1 B ER0 G", "AA1 K AH0 N", "AA1 K AH0 N ER0", "AA1", "AA1 K …
       # $ num           <chr> "character(0)", "1", "character(0)", "character(0)", "character(0)", "charact…
       # $ CV            <fct> V, V, VVCVCC, VVCCVC, VVCCVCVC, VVC, VVCVC, VVCVCVC, VVCCVCC, VVCVCC, VVCCVC,…
       #         # levels(cmu$CV) #3605 combos
#         # levels(as.factor(cmu$stressPattern)) #261





