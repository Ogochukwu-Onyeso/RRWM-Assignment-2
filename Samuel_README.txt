Program to examine the relationship between place of birth, immigrant status, and total income in Canada
Samuel Nemeroff
THIS README CONTAINS THE RESULTS OF THE TABLE AND REGRESSION AT THE END

Step 0.5: Loading dependencies (Optional, done to condense some code and use easier functions such as %>% pipe operator and whatnot)
a)	Make sure to download the tidyverse R package by going to Tools, Install Packages, and type in Tidyverse, and hit install. 

b)	Once Tidyverse has finished installing, ensure to add tidyverse, and specifically dplyr and ggplot2 to the library for your code. 


Part 1: Data cleaning
a)	The first step I did to make this significantly easier on myself and my computer is to isolate the variables in excel to bring over to RStudio. For this, I pulled PPSORT, HDGREE, AGEIMM, IMMSTAT, POB, PR, Sex, and TOTINC into a new sheet.

b)	First and foremost, as we are looking at Canadians income, we must clean the dataset by removing the invalid responses to the TOTINC column (88,888,888 and 99,999,999).

c)	Recode place of birth into a binary variable – either the individual was born in Canada, or they were born outside of Canada. I recoded the name as POB_Binary.

d)	Recode immigration status into a binary variable – either the individual is an immigrant or not. I recoded the name as ImmigrationStat.

e)	I now elect to recode the income from a continuous variable into discrete categories – this is optional, as ultimately, we just end up looking at top earners versus non top earners, but I do this for simpler visualization of things like tables. I elected to recode it into intervals of $20,000, with no lower limit under $20k (as some respondents indicated negative income) and no upper limit above $140k

f)	I also create the additional binary variable of “top earner”, where if a respondent earned any income above $80k qualifies them as a top earner. This is an arbitrary number for this exercise, and for further testing could be set to another value. 0=not top earner, 1=top earner.

g)	I also elected to simplify the HDGREE variable – taking it down to a few less categories. Here, 1=no degree, 2=a degree less than bachelors, 3=bachelors or above, 4=masters, 5=phd, 6=medicine, and 99=not available.  I recoded the name as DegreeEarned.


Part 2:  Visualizations
a)	The first visualization to do is a simple summary table of immigrants versus non-immigrants making above $80,000. Print this table.

b)	Time for the logistic regression. This will be a binary logistic regression, as the dependent variable (top earner) is simply a yes/no variable. Here, we will include the variables of POB_Binary, ImmigrationStat, Sex, DegreeEarned, PR, and AGEIMM. Run this regression, and request a summary of the logit model. 



TABLE RESULTS:
# A tibble: 4 × 3
  POB_Binary ImmigrationStat  count
       <dbl>           <dbl>  <int>
1          0               0 539052
2          0               1     19
3          1               0   2754
4          1               1 194449


REGRESSION RESULTS:
Call:
glm(formula = topearner ~ POB_Binary + ImmigrationStat + SEX + 
    DegreeEarned + PR + AGEIMM, family = "binomial", data = census_clean)

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -2.3303845  0.0453738 -51.360   <2e-16 ***
POB_Binary      -0.0141918  0.0518599  -0.274    0.784    
ImmigrationStat -1.4471692  0.0652471 -22.180   <2e-16 ***
SEX              0.8226602  0.0069096 119.060   <2e-16 ***
DegreeEarned     0.0084512  0.0002841  29.749   <2e-16 ***
PR               0.0146957  0.0002531  58.063   <2e-16 ***
AGEIMM          -0.0119612  0.0004339 -27.567   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 616520  on 736273  degrees of freedom
Residual deviance: 594803  on 736267  degrees of freedom
AIC: 594817

Number of Fisher Scoring iterations: 5
