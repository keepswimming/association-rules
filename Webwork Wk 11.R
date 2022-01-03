Read the data into R.
Change the Lot.Shape variable into a binary variable that tells whether the Lot.Shape is regular ("Reg") 
or irregular (IR1, IR2, or IR3).
library(readr)
library(arules)

ames = read_csv("AmesSimple.csv")
#str(ames)
#head(ames)
summary(ames)

#Change the Lot.Shape variable into a binary variable that tells whether the Lot.Shape is regular ("Reg") or irregular (IR1, IR2, or IR3).
#There are many possibilities; here is one:
  summary(factor(ames$Lot.Shape))
#Because there are no NA's, we can use the ifelse function to create the new version of the variable:
####This is another way to do the above######
  library(dplyr)  
  ames <- ames %>%
  mutate(Lot.Shape = ifelse(Lot.Shape == "Reg", "Reg", "Ir"))
  
#Problem 2
  ```
The continuous variables in the data set will need to be converted to discrete factors. 
A convenient way to do this is with the discretize function in the arules package. Install the arules package 
(if you have not already done so), and load the package. We'll turn Bedroom.AbvGr into a binary variable by 
splitting it into 2 categories:
  
ames$Bedroom.AbvGr = discretize(ames$Bedroom.AbvGr, breaks=2, ordered=T, method="interval")

Note that the argument breaks=2 (not 1 or 3) gives us 2 categories. The argument method="interval" divides the variable into intervals of equal length. For example, using
discretize(x, breaks=3, ordered=T, method="interval") where x ranges from 0 to 9 would create the categories 
[0,3), [3,6), and [6,9].

#2a. View the summary of Bedroom.AbvGr. How many houses are in the first category of this variable?
summary(factor(ames$Bedroom.AbvGr))

#2b.Discretize the number of bathrooms, Full.Bath, into 2 ordered categories with equal interval lengths.
ames$Full.Bath = discretize(ames$Full.Bath, breaks=2, ordered=T, method="interval")

#problem 3
#Discretize Gr.Liv.Area into 3 ordered categories with equal interval length. ******Store the results in a temporary 
#variable--don't overwrite the contents of ames$Gr.Liv.Area, and don't add it as a new column in the data frame. 

#a-How many houses are in the category corresponding to the largest houses?
#class living area into 3 distinct groups
Gr.Liv.Area = discretize(ames$Gr.Liv.Area, breaks = 3, ordered = TRUE, method = "interval")

#b-there are 5 homes with the largest sq. ft.
#summary(ames$Gr.Liv.Area.disc) 
[334,2.1e+03)  [2.1e+03,3.87e+03) [3.87e+03,5.64e+03] #answer is 5 houses in the category of largest house
2610                 315                   5 


#c-calculate the "support" of the largest group - way too low.
support = P(A and B0)
5 / nrow(ames) #0.001706485

#d-Why is the support of the largest category so low? (Hint: It may help to view a histogram of Gr.Liv.Area.)
The distribution of Gr.Liv.Area is right-skewed.

#Do you see any potential problems with discretizing Gr.Liv.Area in this way?
Yes, the largest category has such low support that it is unlikely to be included in any association rules (and it will never be 
included if we use a support threshold higher than the value in part b).

#problem 4
In some cases, discretizing using equal interval lengths doesn't make sense. An alternative is to discretize 
the variable using fixed category boundaries. For example, you might want to use fixed category boundaries to 
discretize people's ages into their 20's, 30's, 40's, etc. This way the resulting association rules would be 
easier to interpret than if you used arbitrarily chosen boundaries.
When a variable is skewed (either left- or right-skewed), using equal interval lengths often results in one or 
more categories with very low support, which means those categories are unlikely to appear in any association 
rules. Instead, you could use fixed category boundaries at
the median (for 2 categories)
the first and third quartiles (for 3 categories)
the 33rd and 67th percentiles (for 3 categories)
the first quartile, median, and third quartile (for 4 categories)

Let's discretize Gr.Liv.Area into 3 ordered categories with fixed boundaries determined by the first and third 
quartiles of the data. Using

summary(ames$Gr.Liv.Area)#we find that the min, max, and first and third quartiles are as follows

Use the following code to discretize Gr.Liv.Area into 3 categories with fixed boundaries:
ames$Gr.Liv.Area = discretize(ames$Gr.Liv.Area, method = "fixed", 
                                breaks=c(334, 1126, 1743, 5642), 
                                ordered=T)
#a. How many houses fall into the category corresponding to the largest values of Gr.Liv.Area?
summary(ames$Gr.Liv.Area)# answer 733
[334,1.13e+03) [1.13e+03,1.74e+03) [1.74e+03,5.64e+03] 
731                1466                 733

#b. Make a histogram of SalePrice. To discretize this variable, is it more appropriate to use fixed boundaries or equal interval lengths?
hist(ames$SalePrice) #its R-skewed, so Fixed boundaries

c. Discretize SalePrice into 3 categories, using the method you chose in part b. If fixed boundaries are more 
appropriate, use the first and third quartiles as the boundaries.

ames$SalePrice = discretize(ames$SalePrice, method = "fixed", 
                            breaks=c(12789, 129500, 213500, 755000), 
                            ordered=T)

#problem 5
The discretize function tells R to treat the resulting variable as an ordered factor. Use the following code to tell R to 
treat all of the other variables as discrete factors, too:
#convert all variables as discrete factors

  ames <- ames %>%
  mutate(across(is.character, factor)) %>%
  mutate(across(is.numeric, factor)) #this will give a warning

###a new way to write this code is 
ames <- ames %>%
  mutate(across(where(is.character), factor))%>%
  mutate(across(where(is.numeric), factor))%>%
  mutate(across(where(is.double), factor))

The ames data frame should now contain 12 columns, all of which are ord or fctr variable types. Next, we need to convert the 
data frame into a transactions format, so that we can use the Apriori algorithm. Do this with the following code:
  
  ames_trans = as(ames, "transactions")

#View the summary of ames_trans.
summary(ames_trans)
#output should look like this
transactions as itemMatrix in sparse format with
2930 rows (elements/itemsets/transactions) and
28 columns (items) and a density of 0.4285471 
> 

#View the summary of ames_trans.
a. How many houses are represented in the data set?#2930 rows

  b. How many attributes ("items" or variables) are in the data? #28 columns 
  
  d. In this data set, all but 2 of the houses have the same number of attributes (i.e., the same size of itemset). 
How many attributes do they have? #12

Problem 6   
#a. Mine the data for association rules with at least .05 support and at least .5 confidence. Consider rules of length up 
#to 12. How many such rules are there?
  
## Redundant rules
rules = apriori(ames_trans, 
                parameter = list(support = .05, confidence = 0.5, maxlen = 12))
summary(rules)

#Inspect the 3 rules with the highest lift. Questions b-d deal with the 3rd rule in this list.
inspect(head(rules, n = 5, by = "lift"))

Hint:
(There are 2 ways to answer this question. Try to find them both!)  
  Method 1: Look at the "count" column of the output from inspect.
Method 2: The "support" column tells what proportion of the data has all the attributes of this rule. Multiply this by the 
number of rows in the data.
count = 180
support = 0.06143345 
rows = 2930

2930*0.06143345 = 

#6c. What proportion of these houses were townhouses?
180/216 #this is the confidence

#6d
#How much more likely is a house to be a townhouse if it falls into this set, 
#compared to a house chosen at random from the whole data set?
#***This is the lift 7.310379 

#Problem 7
In some cases, we may be primarily interested in a particular antecedent or consequent. For example, a real estate agent might be interested in finding characteristics that are associated with having a high sale price (SalePrice between 213500 and 755000).
We could think of SalePrice as a response variable, in which case we could address this question using a classification method of supervised learning. However, we can also address it using the unsupervised learning technique of association rule mining.
a. Use the following code to mine the data for association rules with "SalePrice=[213500,755000]" on the right-hand side, and any variables on the left-hand side.

rules2 = apriori(ames_trans, parameter = list(support = .05, confidence = 0.5, maxlen = 12), 
                 appearance = list(rhs = c("SalePrice=[2.14e+05,7.55e+05]"), default = "lhs") )
a.How many such rules are there?
  summary(rules2) #1926

b. Filter out any redundant rules from this set. Call the set of non-redundant rules rules3. How many rules are in this set?
#take the NON-redundant rules from rule set 2, and put them into rule set 3 
non_redundant = !is.redundant(rules2)
rules3 = rules2[non_redundant]
summary(rules3) #672

OR
non_redundant = (interestMeasure(rules2,
                                 measure = "improvement",
                                 quality_measure = "confidence") > 0)
rules3 = rules2[non_redundant]
summary(rules3)
  
# Problem 8
We can also take subsets of rules after mining them. Focusing on rules3 (the set of non-redundant rules with the consequent 
being a high sales price), let's look at rules with 1- and 2-family homes in the antecedent: 
  
 rules4 = subset( rules3, subset = lhs %in% c("Bldg.Type.simple=1Fam", "Bldg.Type.simple=2Fam") )
summary(rules4)  

We can also take subsets of rules after mining them. Focusing on rules3 (the set of non-redundant rules with the consequent being a high sales price), let's look at rules with 1- and 2-family homes in the antecedent:
  rules4 = subset( rules3, subset = lhs %in% c("Bldg.Type.simple=1Fam", "Bldg.Type.simple=2Fam") )
a. Among this set of rules, what characteristics are associated with the greatest lift in probability of a high sales price?
  
  inspect(head(rules4, n = 1, by = "lift"))
  
    Lot.Area between 
11560 and 215245
square feet
Total.Bsmt.SF between 
990 and 6110
square feet
Gr.Liv.Area between 
1743 and 5642
square feet
One-family home
Year.Built between 1973 and 2010
Garage area between 
480 and 1488
square feet

Does not have
a pool

#We can also subset according to the values of lift and confidence:
  high_lift = subset(rules3, subset = lift > 3.5 & confidence > .95)
How many rules does high_lift contain?
summary(high_lift) #19

#Problm 9
Extracting just the antecedent or just the consequent of rules can be helpful for filtering rules. For example, we can 
filter our results to rules with a single antecedent using the following code:

mylhs = lhs(rules3)
singleAnt = which( size(mylhs) == 1 )
inspect( rules3[singleAnt] )

#Problem 10
#The transactions format is a way to represent a sparse matrix (i.e., a matrix with lots of zeros). The idea is that out 
#of hundreds of items sold in a store, each customer may buy only a few items, so we can save space and time by performing 
#the Apriori algorithm on a version of the data that doesn't list dozens of zeroes for all of the items a customer doesn't 
#buy.
#However, in some cases it can be helpful to convert a transactions or itemMatrix object back to a matrix:
  
mylhs_mat = as(mylhs, Class = "matrix") #error cannot find "mylhs"


#Try making a histogram of the number of antecedents in which each characteristic appears.

num_occurences = colSums(mylhs_mat)
gf_histogram(~num_occurences)
#And for fun, here's how to make a bar chart of the same information:
  antecedents = data.frame(occurences = num_occurences, variable = colnames(mylhs_mat))
antecedents <- antecedents %>% 
  arrange(desc(occurences))
antecedents <- antecedents %>%
  mutate(variable = factor(variable, levels = antecedents$variable))
antecedents %>% 
  gf_col(occurences  ~ variable) %>% 
  gf_theme(axis.text.x=element_text(angle=65, hjust=1))

  