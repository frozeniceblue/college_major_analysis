## Load the data
library("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

devtools::install_github("jhudsl/matahari")
library(matahari)
head(college)

dance_start(value = FALSE, contents = FALSE)

## Clean the data
sum(is.na(college$major_category))
sum(is.na(college$median))
college$major_category=as.factor(college$major_category)

## Show the distribution and relationship of median of major_category
model=lm(median~major_category, college)
summary(model)
boxplot(median/10000~major_category, college, las=2)

## Show the heatmap and the significance of major_category
library(reshape2)
library(ggplot2)

p_value_matrix=matrix(, nrow=16, ncol=16)
for (i in 1:16){
    major_category_reorder=relevel(college$major_category, sort(as.character(unique(college$major_category)))[i])
    fit_factor=lm(median~major_category_reorder, college)
    p_value_raw=summary(fit_factor)$coef[,4]
    p_value_sort=as.matrix(c(p_value_raw[1:i][-1],p_value_raw[1],p_value_raw[i:16][-1]))
    p_value_matrix[,i]=p_value_sort
}

p_value=data.frame(p_value_matrix)
names(p_value)=sort(as.character(unique(college$major_category)))
p_value$ID=sort(as.character(unique(college$major_category)))

p_value_final=melt(p_value)

g=ggplot(data=p_value_final, aes(x=variable, y=ID, fill=value<0.05))
g=g+geom_tile()
g=g+theme(axis.text.x=element_text(angle=90,hjust=1))+xlab("ID") + ylab("ID")
g=g+ggtitle("Show the relationship between category majors")
g

## Show the relationship between significant factor and others
major_category_final=relevel(college$major_category, sort(as.character(unique(college$major_category)))[4])
fit_significant=lm(median~major_category_final, college)
summary(fit_significant)

## dave the file
dance_save("C:/Users/frozenl/Desktop/coursera/project_0/college_major_analysis.rds")
