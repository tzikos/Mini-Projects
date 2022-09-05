########## ΤΕΛΙΚΗ ΕΡΓΑΣΙΑ ##########


# I will use the FirstYearGPA dataset 
dataset = read.csv("FirstYearGPA.csv")
dataset = dataset[-1]
# First, we will take a look in the structure of the dataset to get a grasp of what we have 
str(dataset)

# We will make some changes in order to make our dataset more "meaningful"

dataset$Male = as.factor(dataset$Male)
dataset$FirstGen = as.factor(dataset$FirstGen)
dataset$White = as.factor(dataset$White)
dataset$CollegeBound = as.factor(dataset$CollegeBound)
attach(dataset)
str(dataset)

# We are now able to move on to Descriptive Statistics on our dataset 

summary(dataset) # Taking a general look
dataset[2,1] = 4.0
attach(dataset)

##### Descriptive Statistics for the categorical variables 

#### Male (factor) 

### Frequency tables 
sex_frequency <- table(Male)
sex_frequency <- as.data.frame(sex_frequency)
colnames(sex_frequency)<-c("Sex","Frequency")
rownames(sex_frequency)<-c("Female","Male")
sex_frequency

sex_rel_frequency <- prop.table(table(Male))
sex_rel_frequency <- as.data.frame(sex_rel_frequency)
colnames(sex_rel_frequency)<-c("Sex","Relative Frequency")
tab0<-cbind(sex_frequency,sex_rel_frequency[,2])
colnames(tab0)<-c("Sex","Frequency","Relative Frequency")
tab0 # We can already see that there are slightly more girls in the sample 

### Barplot

sex_frequency_bar <-barplot(table(Male),main="Sex Distribution",
                                 xlab="Sex", ylab="Frequency",
                                 ylim=c(0,130),
                                 horiz=F ,cex.names=0.8,
                                 col=rainbow(length(sex_frequency)))

### Pie chart

sex_frequency_pie <- pie(table(Male), main="Sex", 
                              col=rainbow(length(sex_frequency)) )

#### FirstGen (factor)

### Frequency tables 
firstgen_frequency <- table(FirstGen)
firstgen_frequency <- as.data.frame(firstgen_frequency)
colnames(firstgen_frequency)<-c("First Kid","Frequency")
rownames(firstgen_frequency) <- c("No","Yes")
firstgen_frequency

firstgen_rel_frequency <- prop.table(table(FirstGen))
firstgen_rel_frequency <- as.data.frame(firstgen_rel_frequency)
colnames(firstgen_rel_frequency)<-c("First Kid of the family","Relative Frequency")
tab1<-cbind(firstgen_frequency,firstgen_rel_frequency[,2])
colnames(tab1)<-c("First Kid of the family","Frequency","Relative Frequency")
tab1 # We can clearly see that there are much more kids that are not the first kids of their families
# to go to College.

### Barplot

firstgen_frequency_bar <-barplot(table(FirstGen),main="First Kids Distribution",
                              xlab="Generation", ylab="Frequency",
                              horiz=F ,cex.names=0.8, ylim = c(0,200),
                              col=rainbow(nrow(firstgen_frequency)))

### Pie chart

firstgen_frequency_pie <- pie(table(FirstGen), main="Kids that are the first of their families or not", 
                           col=rainbow(nrow(firstgen_frequency)) )

#### White (factor)

### Frequency tables 
race_frequency <- table(White)
race_frequency <- as.data.frame(race_frequency)
colnames(race_frequency)<-c("Race","Frequency")
rownames(race_frequency) <- c("Minority","White")
race_frequency

race_rel_frequency <- prop.table(table(White))
race_rel_frequency <- as.data.frame(race_rel_frequency)
colnames(race_rel_frequency)<-c("Race of Students","Relative Frequency")
tab2<-cbind(race_frequency,race_rel_frequency[,2])
colnames(tab2)<-c("Race of Students","Frequency","Relative Frequency")
tab2 # We can clearly see that there are much more white students than minority students

### Barplot

race_frequency_bar <-barplot(table(White),main="Race Distribution",
                                 xlab="Race", ylab="Frequency",
                                 horiz=F ,cex.names=0.8, ylim = c(0,200),
                                 col=rainbow(nrow(sex_frequency)))

### Pie chart

race_frequency_pie <- pie(table(White), main="Whites vs Minorities", 
                              col=rainbow(nrow(race_frequency)) )


#### CollegeBound (factor)

### Frequency tables 
cbound_frequency <- table(CollegeBound)
cbound_frequency <- as.data.frame(cbound_frequency)
colnames(cbound_frequency)<-c("College Bound","Frequency")
rownames(cbound_frequency) <- c("No","Yes")
cbound_frequency

cbound_rel_frequency <- prop.table(table(CollegeBound))
cbound_rel_frequency <- as.data.frame(cbound_rel_frequency)
colnames(cbound_rel_frequency)<-c("College Bound Students","Relative Frequency")
tab3<-cbind(cbound_frequency,cbound_rel_frequency[,2])
colnames(tab3)<-c("College Bound Students","Frequency","Relative Frequency")
tab3 # We can clearly see that more than 9/10 students were in a high school where >50% of students aimed for college

### Barplot

cbound_frequency_bar <-barplot(table(CollegeBound),main="College Bound Students Distribution",
                             xlab="Students", ylab="Frequency",
                             horiz=F ,cex.names=0.8, ylim = c(0,200),
                             col=rainbow(nrow(cbound_frequency)))

### Pie chart

cbound_frequency_pie <- pie(table(CollegeBound), main="College Bound Students vs Not", 
                          col=rainbow(nrow(cbound_frequency)) )



### Crosstabulation Matrix for Race and H.S. boundness to College

crosstab <-table(White,CollegeBound)
colnames(crosstab)<-c("No","Yes")
rownames(crosstab)<-c("No","Yes")
crosstab # We cannot observe a significant difference by means of race and high school boundness to college so we make a chi-squared test to be sure

### Chi-Squared Test
# install.packages("gginference")
chi_test1 = chisq.test(White,CollegeBound)
chi_test1 # The p-value is really high (0.965) so we cannot reject the null hypothesis that the race and the high school boundness is not correlated.
library(gginference)
ggchisqtest(chi_test1) # Visualising the result


### Crosstabulation Matrix for either the kid being the first in the family or not and its H.S. boundness to College

crosstab1 <-table(FirstGen,CollegeBound)
colnames(crosstab1)<-c("No","Yes")
rownames(crosstab1)<-c("No","Yes")
crosstab1 # We cannot observe a significant difference by means of order of child and high school boundness to college so we make a chi-squared test to be sure

### Chi-Squared Test
# install.packages("gginference")
chi_test2 = chisq.test(FirstGen,CollegeBound)
chi_test2 # The p-value is higher than 0.05 (0.2156) so we cannot reject the null hypothesis that the order of child and the high school boundness is not correlated.
library(gginference)
ggchisqtest(chi_test2) # Visualising the result

# There is no point in analysing whether there is or not a correlation between the student's order in family and his race or sex so we move on.

##### Descriptive Statistics for the numerical variables 

# install.packages("DescriptiveStats.OBeu")
# library(DescriptiveStats.OBeu)
# ds.analysis(dataset)

# install.packages("psych")
library(psych)
describe(dataset, quant = c(0.25,0.75), IQR = T)[-c(5,8,9,10),-c(1,2)]

# With this line of code we have every information that we want about the numerical variables in a well-organized matrix

##### GPA (numeric)

### Histogram
gpa_histogram <- hist(GPA , breaks ="scott",
                                main = "Histogram of Students' GPA",
                                xlab ="GPA",col = "green",freq = TRUE)

### Boxplot
boxplot(x = GPA, main = "Boxplot of Students' GPA", col = "red")

# We already see that GPA seems to follow normal distribution

##### HSGPA (numeric)

### Histogram
hsgpa_histogram <- hist(HSGPA , breaks ="scott",
                      main = "Histogram of Students' High School GPA",
                      xlab ="High School GPA",col = "green",freq = TRUE)

### Boxplot
boxplot(x = HSGPA, main = "Boxplot of Students' High School GPA", col = "red")

##### SATV (numeric)

### Histogram
satv_histogram <- hist(SATV , breaks ="scott",
                        main = "Histogram of Students' scores in Verbal tests",
                        xlab ="Scores in SATV",col = "green",freq = TRUE)

### Boxplot
boxplot(x = SATV, main = "Boxplot of Students' scores in Verbal tests", col = "red")

##### SATM (numeric)

### Histogram
satm_histogram <- hist(SATM , breaks ="scott",
                       main = "Histogram of Students' scores in Maths tests",
                       xlab ="Scores in SATM",col = "green",freq = TRUE)

### Boxplot
boxplot(x = SATM, main = "Boxplot of Students' scores in Maths tests", col = "red")


##### HU (numeric)

### Histogram
hu_histogram <- hist(HU , breaks ="scott",
                       main = "Histogram of Students' participation hours in humanitarian courses",
                       xlab ="Participation Hours",col = "green",freq = TRUE)

### Boxplot
boxplot(x = HU, main = "Boxplot of Students' participation hours in humanitarian courses", col = "red")

##### SS (numeric)

### Histogram
ss_histogram <- hist(SS , breaks ="scott",
                     main = "Histogram of Students' participation hours in social science courses",
                     xlab ="Participation Hours",col = "green",freq = TRUE)

### Boxplot
boxplot(x = SS, main = "Boxplot of Students' participation hours in social science courses", col = "red")

##### Descriptive Statistics for Factor and Numeric Variables

### GPA by Race

boxplot(formula = GPA~White, data = dataset, main = "Students' GPA by race",
        ylab = "Students' GPA", xlab = "Race of Student", col = rainbow(length(levels(White))))

# We can see that White Students' GPA's are higher in general than minority students

### GPA by Sex

boxplot(formula = GPA~Male, data = dataset, main = "Students' GPA by Sex",
        ylab = "Students' GPA", xlab = "Sex of Student", col = rainbow(length(levels(Male))))

# We can see that Male and Female Students' GPA's are not so different in general

### GPA by FirstGen

boxplot(formula = GPA~FirstGen, data = dataset, main = "Students' GPA by Student's order in family",
        ylab = "Students' GPA", xlab = "Order of Student", col = rainbow(length(levels(FirstGen))))

# We can see that not first in family's order Students' GPA's are higher in general than first in family's order students

### GPA by CollegeBound

boxplot(formula = GPA~CollegeBound, data = dataset, main = "Students' GPA by High School's College Boundness",
        ylab = "Students' GPA", xlab = "College boundness of H.S.", col = rainbow(length(levels(CollegeBound))))

# We can see a slight difference in GPA's with students from non-college-bound high schools being first.

##### We will now move on to check if some of our numeric variables obey the normal distribution

# install.packages("nortest")
library(nortest)

### GPA

lillie.test(GPA) # Even if our p-value is relatively small (0.06421) it is still higher than 0.05 so we cannot be confident to reject the null hypothesis.
# Therefore, we will make another test (the Shapiro-Wilk)

shapiro.test(GPA) # Now, the p-value is less than 0.05 (0.005792) so we confidently reject the null hypothesis that the GPA's obey a normal distribution.

### HSGPA

lillie.test(HSGPA) # The p-value is smaller than 0.05 (0.002229) so we reject the null hypothesis that the Students' High School GPA's obey a normal distribution.

### SATV

lillie.test(SATV) # Again, we reject the null hypothesis that the scores of students in a verbal sat test follow a normal distribution. (p-value = 0.0002052)

### SATM

lillie.test(SATM) # Like before, we reject the null hypothesis (p-value = 0.02064)

### HU

lillie.test(HU) # We reject the null hypothesis (p-value = 0.0004468)

### SS 

lillie.test(SS) # We reject the null hypothesis (p-value = 0.000001271)

# So, none of our numeric variables obeys a normal distribution 
# Therefore, we move on to non-parametrical tests

### Non-Parametrical Tests

#install.packages("randtests")
library(randtests)

### GPA
runs.test(GPA) # p-value is 0.6838 > 0.05 so we confidently accept the null hypothesis that the sample is random.

### HSGPA
runs.test(HSGPA) # p-value is 0.5402 > 0.05 so we confidently accept the null hypothesis that the sample is random.

### SATV 
runs.test(SATV) # p-value is 0.5972 > 0.05 so we confidently accept the null hypothesis that the sample is random.

### SATM 
runs.test(SATM) # p-value is 0.679 > 0.05 so we confidently accept the null hypothesis that the sample is random.

### HU
runs.test(HU) # p-value is 0.8063 > 0.05 so we confidently accept the null hypothesis that the sample is random.

### SS
runs.test(SS) # p-value is 0.1204 > 0.05 so we confidently accept the null hypothesis that the sample is random.

# We now move on to non-parametrical Hypothesis testing


##### Non-Parametrical Hypothesis Testing 

### GPA ~ Sex

wilcox.test(x = dataset[Male == "0","GPA"],
            y = dataset[Male == "1","GPA"]) # We can accept the null hypothesis that the GPA of Males is the same as Females

### GPA ~ Race

wilcox.test(formula = GPA ~ White, data = dataset) # We can confidently reject the null hypothesis that the GPA of White students is the same as the Minority students.
# By taking into account a boxplot that we saw earlier, we can summarize that White students have higher GPA's in general.

### GPA ~ FirstGen

wilcox.test(formula = GPA ~ FirstGen, data = dataset) 
# We can reject the null hypothesis that students' GPA's who are or are not first in their families, are the same.
# By taking into account a boxplot that we saw earlier, we can summarize that students who are not the first kids in their families,
# tend to have higher GPA's than students who are the first in their families.

### GPA ~ CollegeBound

wilcox.test(formula = GPA ~ CollegeBound, data = dataset) 
# We cannot reject the null hypothesis that there is no significant difference between the GPA's of students who went to college bound 
# high schools and students that did not.

### Now, we move on to examine the correlation of all of our numerical variables

cor(dataset[,-c(5,8,9,10)]) # With this simple line of code we are able to see that there is not even one strong correlation between our numerical variables (the highest is ~0.52)

# install.packages("corrplot")
library(corrplot)
corrplot(cor(dataset[,-c(5,8,9,10)])) # Visualising the results.

# We concluded that there is no important correlation between any of the numerical variables, so we will skip the part of plotting each pair of numerical variables.

##### We are finally ready to proceed with the part of machine learning in order to predict the students' first year GPA based on the other features.

# First, I will make a new categorical variable "grade" replacing the numerical GPA in order to make predictions 
# for a class with 2 levels. Ideally, I would make the grade variable consisting of 3 levels, but since we only 
# learned how to make predictions for categorical variables of 2 levels I will follow that. Therefore, I will 
# only make 2 levels called "High Pass" and "Pass" to keep it in my reach.

grade1 = as.factor(ifelse(GPA>=3.3,"High Pass","Pass")) 

# Making the new dataset 
dataset2 = data.frame(dataset[-c(1)],grade1)         
attach(dataset2)

# Making our training and test sets
s_size <- floor(0.8 * nrow(dataset2))
set.seed(1)
train_index <- sample(1:nrow(dataset2), size = s_size)
train <- dataset2[train_index,]
test <- dataset2[-train_index,]

# Making our first classifier, the tree.
library(tree)
tree.grade <- tree(grade1 ~ ., train) 

# Viewing our first results
summary(tree.grade)

# Plotting the new tree
par(mfrow=c(1,2))
plot(tree.grade)
text(tree.grade,pretty=0)

# Making predictions 
tree.pred1 <- predict(tree.grade, test, type="class")

# Making the Confusion Matrix
grade.test = grade1[-train_index]
ConMat<-table(tree.pred1,grade.test)
ConMat

# Evaluating the model
library(caret) 
confusionMatrix(ConMat) # Accuracy : 0.6136


# Evaluating which is the best model based on dev
set.seed(1)
cv.grade <- cv.tree(tree.grade, FUN=prune.misclass)
names(cv.grade)
cv.grade

prune.grade <- prune.misclass(tree.grade, best=15)

# Plotting the pruned tree
plot(prune.grade)
text(prune.grade,pretty=0)

# Making new predictions
tree.pred2 <- predict(prune.grade, test, type="class")

# Making the Confusion Matrix
ConMat2<-table(tree.pred2,grade.test)
ConMat2

# Evaluating the model

confusionMatrix(ConMat2) # Accuracy : 0.6364

######## SVM

### Linear

# To begin with, I will make a linear SVM model with linear kernel and cost 10.
library(e1071)
svm.model_1<-svm(grade1 ~ .,data=train,kernel="linear",cost=10)
summary(svm.model_1)

# Now, I will use the SVM model to make predictions for the test set.
svm.pred_1<-predict(svm.model_1, test) 

# Making the Confusion Matrix.
xtab<-table(predict=svm.pred_1,truth=test$grade1)
xtab # We can see there are some misclassifications.

# Evaluating the SVM model.
library(caret) 
confusionMatrix(xtab) # Accuracy : 0.7273

# I will now use the tune function in order to optimize my model by trying different cost values.
set.seed(123) # Because we want to get the same results everytime.
tune.out=tune(svm , grade1~., data=train, kernel ="linear",
              ranges =list(cost=c(0.01,0.1, 1, 10, 100, 1000)))

summary(tune.out) # Lowest error is for cost of 1.

# Making the "best" linear model.
bestmodel <- svm(grade1 ~ .,data=train,kernel="linear",cost=1)
summary(bestmodel)

# I will use the best linear model to make predictions for the test set.
svm.pred_2<-predict(bestmodel,test)

# Making the Confusion Matrix
xtab2<-table(predict=svm.pred_2,truth=test$grade1)

# Evaluating the SVM model
confusionMatrix(xtab2) # Accuracy : 0.7273

# Since the linear kernel SVM model with cost = 10 has the same accuracy (0.7273) with the other 
# linear kernel SVM model with cost = 1, I will keep as the best, the one with the lower cost, 
# which is the "bestmodel".

### Radial

# I will use the tune function again in order to find the best parameters (cost and gamma) for the 
# SVM model with radial kernel.
set.seed(123)
tune.out1=tune(svm , grade1~., data=train, kernel ="radial",
               ranges =list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.01,0.1,1,10)))
summary(tune.out1) # Best parameters : cost = 100, gamma = 0.01

# Making the best SVM model with radial kernel
bestmodel1 <- svm(grade1 ~ .,data=train,kernel="radial",cost=1,gamma=0.1)

# Using the model for predictions
svm.prediction<-predict(bestmodel1,test)

# Making the Confusion Matrix
xtab3<-table(predict=svm.prediction,truth=test$grade1)

# Evaluating the model
confusionMatrix(xtab3) # Accuracy : 0.7273

### Sigmoid

# I will use again the tune function in order to find the best parameters for the SVM model with sigmoid kernel.
set.seed(123)
tune.out2=tune(svm , grade1~., data=train, kernel ="sigmoid",
               ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000), gamma=c(0.01,0.1,1,10)))
summary(tune.out2) # Best parameters : cost = 100, gamma = 0.01

bestmodel2 <- svm(grade1 ~ .,data=train,kernel="sigmoid",cost=100,gamma=0.01)
bestmodel2

# Using the model to make predictions on the test set
svm.prediction1<-predict(bestmodel2,test)

# Making the Confusion Matrix
xtab4<-table(predict=svm.prediction1,truth=test$grade1)

# Evaluating the model
confusionMatrix(xtab4) # Accuracy : 0.7273 which is the same as the other 2 SVM's

# From our results we can see that we have equal accuracy in the optimized SVM models with linear, radial and sigmoid
# kernels and less accuracy (0.6136) with our tree model. So, both SVM's will have about the same results, but I will 
# choose the one with linear kernel as a preference. 

# Lastly, further than the course's content, I made some small research on my own and I found out this way of making 
# predictions for a categorical variable consisting of more than 2 levels.

library(nnet)
test1 = multinom(grade~.,data = dataset1)
summary(test1)
predict1 = predict(test1, grade)

# Making the Confusion Matrix
table123<-table(predict=predict1,truth=grade)

# Evaluating the model
confusionMatrix(table123) # Accuracy : 0.7854
