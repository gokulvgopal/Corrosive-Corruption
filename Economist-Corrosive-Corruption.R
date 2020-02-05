# Data Visualization Project on the article - Corrosive Corruption by The Economist
# The details of the study is available at https://www.economist.com/graphic-detail/2011/12/02/corrosive-corruption
# Through this project, I aim to study the relation between Corruption Perception Index and Human Development Index
# Also, highlight the countries in best, average and worst ranking.

#importing library
library(ggplot2)
library(ggthemes)
library(dplyr)
df <- read.csv("Economist_Assignment_Data.csv")
head(df)
# Checking col names
# print(colnames(df))

# Data Visualization of CPI vs HDI using scatter plot
pl <- ggplot(df, aes(x=CPI, y=HDI)) + geom_point(aes(color=Region), size=4)
print(pl)

# adding a trend line
pl1 <- pl+geom_smooth(aes(group=1), method='lm', formula = y~log(x), se=FALSE,color='red')
pl2 <- pl1 + geom_text(aes(label=Country))
print(pl2)

# As we can see here, it is too cluttered. Using the summary(df) Let's find out the 1st quartile and 3rd quartile range
# to find worst and best cases
print(summary(df))
# worst cases come between 0.2860 and 0.5090 and the countries are
print( df %>% filter(HDI<=0.5090) %>% filter(n()>1) %>% summary())
# there are 44 countries
worst.country.df <- df %>% filter(HDI<=0.5090) %>% filter(n()>1) %>% arrange(HDI, desc(HDI)) %>% top_n(n=-15,HDI)
#print(worst.country.df)
#list of worst 15 countries
worst.country <- as.vector(worst.country.df$Country)

# from the summary we can see the medium HDI countries lie around 0.4125 with a median of 0.4295
# lets consider 15 countries between range 0.4100 to 0.43
avg.country.df <- df %>% filter(HDI<=0.44, HDI >= 0.40) %>% filter(n()>1) %>% arrange(HDI, desc(HDI)) %>% top_n(n=15,HDI)
#print(avg.country.df)
avg.country <- as.vector(avg.country.df$Country)

countryList <- rbind(worst.country.df, avg.country.df)

best.country.df <- df %>% filter(HDI >= 0.4597) %>% filter(n()>1) %>% arrange(HDI, desc(HDI)) %>% top_n(n=15,HDI)
print(best.country.df)
best.country <- as.vector(best.country.df$Country)

countryList <- rbind(countryList, best.country.df)
# countryNames has 45 countries from worst, average and best HDI range. Using this list to highlight only the important names in the plot
countryNames <- as.vector(countryList$Country)
#print(countryNames)
pl3 <- pl1 + geom_text(aes(label = Country), color = "gray20", data = subset(df, Country %in% countryNames),check_overlap = TRUE)
print(pl3)

# Adding title and scale to the plot
pl4 <- pl3 + scale_x_continuous(name='Corruption Perceptions Index, 2011 (1= Most Corrupt, 10 = Least Corrupt)', limits=c(1,10), breaks=1:10)
pl5 <- pl4 + scale_y_continuous(name='Human Development Index, 2011 (0 = Worst, 1 = Best)',limits=c(0.2,1))
pl6 <- pl5 + ggtitle("Corruption vs Human Development")
print(pl6)

# Adding the economist theme
pl7 <- pl6 + theme_economist_white()
print(pl7)
