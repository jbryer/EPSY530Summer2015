library(gdata)
library(likert)
library(reshape)

fall2013 <- read.xls('~/Dropbox/School/Teaching/EPSY530 2013 Fall/Data/MathAnxiety.xls')
summer2014 <- read.xls('~/Dropbox/School/Teaching/EPSY530 2014 Summer/Data/MathAnxiety.xls')
fall2014 <- read.xls('~/Dropbox/School/Teaching/EPSY530 2014 Fall/Data/MathAnxiety.xlsx')
summer2015 <- read.xls('~/Dropbox/School/Teaching/EPSY530 2015 Summer/Data/MathAnxiety.xlsx')

fall2013$Term <- 'Fall 2013'
summer2014$Term <- 'Summer 2014'
fall2014$Term <- 'Fall 2014'
summer2015$Term <- 'Summer 2015'

mass <- rbind(fall2013, summer2014, fall2014, summer2015)

#mass <- read.xls('Data/MathAnxiety.xlsx')

items <- c('I find math interesting.',
		   'I get uptight during math tests.',
		   'I think that I will use math in the future.',
		   'Mind goes blank and I am unable to think clearly when doing my math test.',
		   'Math relates to my life.',
		   'I worry about my ability to solve math problems.',
		   'I get a sinking feeling when I try to do math problems.',
		   'I find math challenging.',
		   'Mathematics makes me feel nervous.',
		   'I would like to take more math classes.',
		   'Mathematics makes me feel uneasy.',
		   'Math is one of my favorite subjects.',
		   'I enjoy learning with mathematics.',
		   'Mathematics makes me feel confused.')

for(i in 2:15) {
	mass[,i] <- factor(mass[,i], levels=1:5, labels=c('Strongly Disagree', 
													  'Disagree', 'Neutral', 'Agree', 'Strongly Agree'),
					   ordered=TRUE)
}

names(mass) <- c('Gender', items, 'Term')
str(mass)

l.summer15 <- likert(mass[mass$Term == 'Summer 2015',2:15])
plot(l.summer15)

l <- likert(mass[,2:15])
plot(l, wrap=30)

l.term <- likert(mass[,2:15], grouping = mass$Term)
plot(l.term)

l.gender <- likert(mass[,2:15], grouping =  mass$Gender)
plot(l.gender)


