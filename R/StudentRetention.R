library(retention)
data(students)
data(graduates)
head(students)
head(graduates)

students$Persisting = NA
students[which(students$Persist == 'Y'),'Persisting'] = TRUE
students[which(students$Persist == 'N'),'Persisting'] = FALSE


ret = retention(students, graduates,
				studentIdColumn='StudentId',
				degreeColumn='Degree',
				persistColumn='Persisting',
				warehouseDateColumn='CreatedDate', 
				gradColumn='GraduationDate',
				grouping='Level')

