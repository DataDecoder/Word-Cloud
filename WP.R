install.packages("NLP")
library(NLP)
library(psych)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library('tm')
# Set the working directory, file path and read the information
setwd("C:/Users/HP/Desktop/cereal project")
filepath = "C:/Users/HP/Desktop/cereal project/word.txt"
text_file<-readLines(filepath)
head(text_file)
tail(text_file)

# Step1 - Removing the spaced now and Cleaning the text 
Remove_Spaces <- paste(text_file, collapse = " ")
text_clean<-tolower(Remove_Spaces)
head(text_clean)

text_clean <- tolower(text_clean)
head(text_clean)

# Step 2 - Removing the punctuation using "\W" and digits usig  "\\d"
clean_text1=gsub(pattern = "\\W", replace = " ",text_clean) 
head(clean_text1)

clean_text2 = gsub(pattern = "\\d", replace = " ", clean_text1)
head(clean_text2)


# Step 3 - Removing the Stop words
stopwords()
clean_text3<-removeWords(clean_text2,stopwords())
head(clean_text3)

# Step 4 - Removing the single letter alphabets
Remove_alhabets = gsub(pattern = "\\b[A-z]\\b{1}", replace = " ",clean_text3)
head(Remove_alhabets)

# Step 5 - Removing white spaces
Remove_whitespace= stripWhitespace(Remove_alhabets)
head(Remove_whitespace)

# Step 6 - Split the words 
Text_split = strsplit(Remove_whitespace," ")
head(Text_split)


#Step 7 - Check the word frequency now
word_freq=table(Text_split)
head(word_freq)

cbind(names(word_freq), as.integer(word_freq))
head(word_freq)
View(word_freq)

write.csv(word_freq,"wordfrequency.csv")

# Step 8 - Generate the word cloud

df <- data.frame(names(word_freq), as.integer(word_freq))
figPath <- "C:/Users/HP/Desktop/dh.png"
wordcloud2(df, figPath = figPath, size = 1.5, color = "green",
           backgroundColor = "white")
letterCloud(df, word = "CEREALS", color='random-light' , 
             backgroundColor="black")
wordcloud2(df, size = 0.9, shape = 'star')

df <- data.frame(names(word_freq), as.integer(word_freq))
wordcloud2(df, size = 0.9, shape = 'triangle')


# Step 9 - Time to organize the words as per wordcloud. 
# The class of the data needs to be characters.
# we can achieve this my unlisting the list of words.

class(Text_split)
word_cloud1 = unlist(Text_split)
View(word_cloud1)

# Step 10 - Word Clouds
wordcloud(word_cloud1,min.freq = 1)
wordcloud(word_cloud1, min.freq = 3)
wordcloud(word_cloud1,min.freq = 1,random.order=FALSE)
wordcloud(word_cloud1,min.freq = 1,random.order=TRUE)
wordcloud(word_cloud1)
txt_col = brewer.pal(10, "Dark2")
wordcloud(word_cloud1, min.freq=3, random.order=F, scale=c(3,1),colors=txt_col)


# Step 11 - Add rainbow colors to the words. No. of colors in paranthesis
wordcloud2(word_freq, color = "random-light", backgroundColor = "Black")
write.csv(word_freq,"wordfrequency1.csv")
##################################SENTIMENT ANALYSIS##########################################

# Step 12 - Sentiment Analysis - A bag of positive and negative words
positive<-scan("positive.txt", what = "character", comment.char=";")
negative<-scan("negative.txt",what = "character", comment.char=";")
head(positive)
View(positive)
head(negative)


# Step 13 - Now we will try and match these positive/negative words in our text file
senti_analysis<-unlist(Text_split)
match(senti_analysis,positive)
match(senti_analysis,negative)


# Step 14 - We will now find out the p-score, n-score and senti_score
p_score<-sum(!is.na(match(senti_analysis,positive)))
p_score
n_score<-sum(!is.na(match(senti_analysis,negative)))
n_score
sentiment_score=p_score-n_score
sentiment_score


