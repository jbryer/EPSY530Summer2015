install.packages(c('devtools','psych'))
devtools::install_github('jbryer/pisa')

library(pisa)
library(psych)

data(pisa.student)
data(pisa.school)
data(pisa.catalog.student)
data(pisa.catalog.school)
data(pisa.countries)

pisa.countries       # List the countries
pisa.catalog.student # List of student variables
pisa.catalog.school  # List of school variables

# School type (i.e. public or private) is in the school data file. This will
# merge the data tables so we have that variable for each student.
pisa.student <- merge(pisa.student, pisa.school[,c('CNT','SCHOOLID','SCHTYPE')], 
					  by=c('CNT', 'SCHOOLID'), all.x=TRUE)
# SCHTYPE is a three level factor, we'll convert it to a two level factor
table(pisa.student$SCHTYPE)
pisa.student$Public <- ifelse(pisa.student$SCHTYPE == 'Public', 'Public', 'Private')
# Make sure things look correct.
table(pisa.student$Public, useNA='ifany')
# Remove rows with missing school type
pisa.student <- pisa.student[!is.na(pisa.student$Public),] 

# Some other variables that might be of interest
#ST04Q01 # Sex
#JOYREAD # Enjoy/like reading

tab <- describeBy(pisa.student$PV1MATH, group=list(pisa.student$Public, pisa.student$CNT), 
		   mat=TRUE)[,c('group2','group1','n','mean','sd')]
tab
x <- xtable(tab, caption='Math Score by Country and Public/Private School Status')
print(x, include.rownames=FALSE, type='html')

ggplot(pisa.student, aes(x=PV1MATH, color=Public)) + geom_density() + facet_wrap(~ CNT)

pisa <- pisa.student[pisa.student$CNT == 'United States',]
nrow(pisa)
table(pisa$Public, useNA='ifany')

boxplot(PV1MATH ~ Public, data=pisa)
describeBy(pisa$PV1MATH, group=pisa$Public,  mat=TRUE)[,c('group1','n','mean','sd')]

(mean_private <- mean(pisa[pisa$Public == 'Private',]$PV1MATH))
(mean_public <- mean(pisa[pisa$Public == 'Public',]$PV1MATH))
(sd_private <- sd(pisa[pisa$Public == 'Private',]$PV1MATH))
(sd_public <- sd(pisa[pisa$Public == 'Public',]$PV1MATH))
(n_private <- nrow(pisa[pisa$Public == 'Private',]))
(n_public <- nrow(pisa[pisa$Public == 'Public',]))

(se <- sqrt( (sd_public^2 / n_public) + (sd_private^2 / n_private) ))
(df <- min( n_public - 1, n_private - 1))
(t <- ((mean_public - mean_private) - 0) / se)
pt(t, df)
(diff <- mean_private - mean_public)
(ci <- c(diff - qt(.975, df) * se, diff + qt(.975, df) * se))

t.test(PV1MATH ~ Public, data=pisa)
