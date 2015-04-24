require(gdata)

# Read in the data file
mass <- read.xls('Data/MathAnxiety.xlsx')

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

for(i in 2:ncol(mass)) {
	mass[,i] <- factor(mass[,i], levels=1:5, labels=c('Strongly Disagree', 
							'Disagree', 'Neutral', 'Agree', 'Strongly Agree'),
					   ordered=TRUE)
}

names(mass) <- c('Gender', items)
str(mass)

require(likert)

l <- likert(mass[,2:ncol(mass)])
plot(l, wrap=30)

lg <- likert(mass[,2:ncol(mass)], grouping=mass$Gender)
plot(lg)
