#############################
#    Statistical Learning   #
#       Final project       #
# Mathias Cardarello Fierro #
#############################

#############################
##  1.Setting environment
#############################

## We use the function p_load of 'pacman' to check if a package is installed, 
# if not it attempts to install it and then loads it

if (!require("pacman")) install.packages("pacman")

pacman::p_load(readr, dplyr, readxl, ggpubr, ggplot2, Hmisc, plyr, tigerstats, lsr, gridExtra,
               MASS, tree, ISLR, forcats, randomForest, caret, RCurl, party, rpart, rpart.plot,
               DAAG, mlbench, pROC, DescTools, cluster, factoextra, sjPlot)


devtools::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)

devtools::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)



#############################
##  2.Data exploration
#############################

## Load the pre processed dataset from GitHub

x <- getURL("https://raw.githubusercontent.com/mathicard/SL-DSE/main/dataset_final.csv")
df <- read.csv(text = x)[,-c(1,13,14)]

## Casting int and char to Factor

df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.factor)
df[sapply(df, is.integer)] <- lapply(df[sapply(df, is.integer)], 
                                       as.factor)

## 2.1 Descriptive statistics ##

# Dependent variable: Level_READ

summary(df$Level_MAT)


# Categories distribution

group_by(df, Level_MAT) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    mean = mean(theta_MAT, na.rm = TRUE),
    median = median(theta_MAT),
    min = min(theta_MAT),
    max = max(theta_MAT),
    sd = sd(theta_MAT, na.rm = TRUE)
  )

# We have five groups of students according to their Maths score,
# but the extreme ones (1 and 5) have a very small share (6.3% and 8.6%).
# To avoid this unbalanced distribution among categories, 
# level 1 is merged with level 2, while level 5 is merged with level 4.


df_original <- df 

df$Level_MAT <- fct_collapse(df$Level_MAT, '1' = c("1", "2"), '2' = "3", '3' = c("4", "5"))

group_by(df, Level_MAT) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    mean = mean(theta_MAT, na.rm = TRUE),
    median = median(theta_MAT),
    min = min(theta_MAT),
    max = max(theta_MAT),
    sd = sd(theta_MAT, na.rm = TRUE)
  )


# Now we have a factor with 3 levels, each of them with a share between 31% and 35%


# Box plot
ggplot(df, aes(Level_MAT, theta_MAT)) +
  geom_boxplot(aes(fill = Level_MAT), show.legend = FALSE) +
  labs(x="Score level", y="Score") +
  theme_minimal()


# Histogram
ggplot(df, aes(x=Level_MAT)) +
  labs(x="Score level", y="") +
  scale_fill_manual(values = "steelblue3") +
  geom_bar(fill="steelblue3") +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),3) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.5), 
            size = 3.3) +
  theme_minimal()


## Summary by Gender ##

df %>% group_by(Gender) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    "mean score" = mean(theta_MAT))  

# the sample is balanced between genders
# males has, on average, a better score level in Maths than females.


# Histogram by Gender

ggplot(df, aes(x=Level_MAT, fill=Gender)) +
  labs(x="Score level", y="", fill="Gender") +
  scale_fill_manual(values = c("tan1", "#4E84C4")) +
  geom_bar(position="dodge") +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),3) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(1), 
            size = 3.3) +
  theme_minimal()


# Box plot

stat_box_data <- function(y, upper_limit = max(df$theta_MAT) * 1.5) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

ggplot(df, aes(Gender, theta_MAT)) +
  geom_boxplot(aes(fill = Gender), show.legend = FALSE) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9) + 
  labs(x="Gender", y="Score") +
  scale_fill_manual(values = c("tan1", "#4E84C4")) +
  stat_compare_means(method = "anova", label.y = 8.8) +        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE) +     # Pairwise comparison against all
  theme_minimal()


# Chi-squared test

associationTest(~Gender+Level_MAT, data=df) #Variables are independent at 95%



## Summary by Region ##

df %>% group_by(Region) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    "mean score" = mean(theta_MAT))  

# Students from the capital city (MVD) have, on average, a better score.


# Box plot

ggplot(df, aes(Region, theta_MAT)) +
  geom_boxplot(aes(fill = Region), show.legend = FALSE) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.6) + 
  labs(x="Region", y="Score") +
  stat_compare_means(method = "anova", label.y = 8.5) +        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE) +     # Pairwise comparison against all
  theme_minimal()


# Chi-squared test

associationTest(~Region+Level_MAT, data=df) 

#Variables are dependent at 5% for both test statistics.


## Summary by Age ##

df %>% group_by(Age) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    "mean score" = mean(theta_MAT))  


#Box plot
ggplot(df, aes(as.factor(Age), theta_MAT)) +
  geom_boxplot(aes(fill = Age), show.legend = FALSE) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9) + 
  labs(x="Age", y="Score") +
  stat_compare_means(method = "anova", label.y = 8.5) +        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE) +     # Pairwise comparison against all
  theme_minimal()

# Also with Age we merge some groups to have a more balanced sample

# Creating a factor corresponding to age
# with three equally spaced levels

df$Age <- as.factor(df$Age)
df$Age <- fct_collapse(df$Age, '<=11' = c("10", "11"), '12' = "12", '>=13' = c("13", "14", "15"))

#Box plot
ggplot(df, aes(Age, theta_MAT)) +
  geom_boxplot(aes(fill = Age), show.legend = FALSE) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 1.6) + 
  labs(x="Age", y="Score") +
  stat_compare_means(method = "anova", label.y = 8) +        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE) +     # Pairwise comparison against all
  theme_minimal()

#The p-value is low (p < 0.001), 
#so it appears that the score is not independent of the Age.


# Chi-squared test

associationTest(~Age+Level_MAT, data=df) 

#Variables are dependent at 1% for both test statistics.



## Summary by Centre_type ##

df %>% group_by(Centre_type) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    "mean score" = mean(theta_MAT))  


#Box plot

ggplot(df, aes(Centre_type, theta_MAT)) +
  geom_boxplot(aes(fill = Centre_type), varwidth = TRUE, show.legend = FALSE) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9) + 
  labs(x="Centre type", y="Score") +
  stat_compare_means(method = "anova", label.y = 8.5) +        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE) +     # Pairwise comparison against all
  theme_minimal()


# Chi-squared test

associationTest(~Centre_type+Level_MAT, data=df) 


## Summary by Context ##

df %>% group_by(Context) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    "mean score" = mean(theta_MAT))  


# Box plot

ggplot(df, aes(Context, theta_MAT)) +
  geom_boxplot(aes(fill = Context), show.legend = FALSE) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9) + 
  labs(x="Context", y="Score") +
  stat_compare_means(method = "anova", label.y = 8) +        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE) +     # Pairwise comparison against all
  theme_minimal()


# Chi-squared test

associationTest(~Context+Level_MAT, data=df) 

#Variables are not independent at 99% for both tests

# It gave a warning because many of the expected values will be very small 
# and therefore the approximations of p may not be right. 
# Therefore it was required to simulate p-value to get a more robust approximation

chisq.test(x = df$Context, y = df$Level_MAT, simulate.p.value = TRUE)

# Variables are NOT independent at 99%


# Chi-squared test between Context and Centre_type
associationTest(~Context+Centre_type, data=df) 

# Expected frequencies too small (simulate p-value)
chisq.test(x = df$Context, y = df$Centre_type, simulate.p.value = TRUE)

# Variables are NOT independent at 99%


# Mosaic plot

ggplot(data = df) +
  geom_mosaic(aes(x = product(Context, Centre_type), fill=Context)) +
labs(title='Context by centre type', x='Centre type') +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()


## Summary by Strat ##

df %>% group_by(strat) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    "mean score" = mean(theta_MAT))  


# Box plot

ggplot(df, aes(strat, theta_MAT)) +
  geom_boxplot(aes(fill = strat), varwidth = TRUE, show.legend = FALSE) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9) + 
  labs(x="Strata", y="Score") +
  stat_compare_means(method = "anova", label.y = 8.5) +        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE) +     # Pairwise comparison against all
  theme_minimal()


# Chi-squared test

associationTest(~strat+Level_MAT, data=df) 

#Variables are not independent at 5%


## Summary by Reading score ##

df %>% group_by(Level_MAT) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    "mean Reading score" = mean(theta_READ))  


# Box plot

ggplot(df, aes(Level_MAT, theta_MAT)) +
  geom_boxplot(aes(fill = Level_MAT), varwidth = TRUE, show.legend = FALSE) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9) + 
  labs(x="Math level", y="Reading score") +
  stat_compare_means(method = "anova", label.y = 8.5) +        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE) +     # Pairwise comparison against all
  theme_minimal()

# Chi-squared test between factor variables

associationTest(~Level_MAT+Level_READ, data=df) 

#Variables are not independent at 1%


# Pearson correlation test for continuos variables

cor.test(df$theta_MAT, df$theta_READ, method = "pearson")

# true correlation is not equal to 0 at 99%
# are significantly correlated with a correlation coefficient of 0.63.



# Scatter plot

# color by Gender
scatterPlot <- ggplot(df,aes(theta_MAT, theta_READ, color=Gender)) + 
  geom_point() + 
  scale_color_manual(values = c("tan1", "#4E84C4")) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x="Score in Maths", y = "Score in Reading") +
  theme(legend.position=c(0,1), legend.justification=c(0,1))

# Marginal density plot of x (top panel)
xdensity <- ggplot(df, aes(theta_MAT, fill=Gender)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("tan1", "#4E84C4")) +
  labs(x="Score in Maths") +
  theme(legend.position = "none")

# Marginal density plot of y (right panel)
ydensity <- ggplot(df, aes(theta_READ, fill=Gender)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("tan1", "#4E84C4")) + 
  labs(x="Score in Reading") +
  theme(legend.position = "none")

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))



## Summary by Attendance in 2020 ##

#Reorder levels of Attendance
df$Attendance <- factor(df$Attendance, levels = c("regularly", 
                                                  "little (every week is a few days away)", 
                                                  "very little (approx. once every 15 days)"))


df %>% group_by(Attendance) %>% 
  dplyr::summarise(
    count = n(),
    share = n()/4722*100,
    "mean Reading score" = mean(theta_MAT)) 


# Box plot

ggplot(df, aes(Attendance, theta_MAT)) +
  geom_boxplot(aes(fill = Attendance), varwidth = TRUE, show.legend = FALSE) +
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9) + 
  labs(x="Attendance", y="Score") +
  stat_compare_means(method = "anova", label.y = 8.3) +        # Add global ANOVA p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.", hide.ns = TRUE) +     # Pairwise comparison against all
  scale_fill_brewer(palette="Blues") +
  theme_minimal()


# Chi-squared test

associationTest(~Attendance+Level_MAT, data=df) 

# Variables are not independent at 1%

## Multiple Chi-sq tests for each factor variable with Level_MAT ##

CHIS <- lapply(df[,-c(2,3,4)], function(x) chisq.test(df[,3], x))
sum_chi <- as.data.frame(do.call(rbind, CHIS))

sum_chi[1:8,1:3]

not_sig <- filter(sum_chi[,c(1,3)], p.value >=0.05)
str(not_sig)

top <- as.data.frame(lapply(not_sig, unlist))
top[order(-top[, 2]), ]

# there are only 16 out of 193 variables with NO significant independence with Level_MAT at 5%,
# among them there is Gender as we showed before.


#############################
##  3.SUPERVISED ANALYSIS
#############################


### 3.1 LDA ###

## Data preprocessing

data <- df[,-c(2,5)] #keep Level_MAT and theta_READ

# Normalise numeric variables (theta_READ)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

data$theta_READ <- normalize(data$theta_READ)
summary(data$theta_READ) #ok


## Detect multicollinearity in the dataset (CramerV approach)

mult <- PairApply(df, CramerV, symmetric = FALSE)

(vars <- which(mult <= 0.01, arr.ind = T))
dim(vars)

(vars_name <- unique(rownames(vars))) #19 variables multicollinear


#(vars_index <- order(unique(vars[,1])))
(vars_index <- unique(vars[,1]))


## Multiple Chi-sq tests for those variables wrt Level_MAT

CHIS_m <- lapply(df[,c(vars_index)], function(x) chisq.test(df[,3], x))
sum_chi_m <- as.data.frame(do.call(rbind, CHIS_m))

(sig <- filter(sum_chi_m[,c(1,3)], p.value <=0.01))
dim(sig)

top2 <- as.data.frame(lapply(sig, unlist))
top2[order(top2[, 2]), ]

# There are 14 significantly associated variables among this subset
# We keep the ones with the highest Chi-squared statistic (>= 70) -> most related to Level_MAT

# We keep: "E6Co6_6", "Age", "E6Co7_4", "Attendance", "E6Co6_5", "E6Co6_4"

# We drop: 
# "Gender", "E6CF25a_2_3", "E6CF25a_2_4", "E6Co4", "E6Co7_2",
# "E6C44m_2", "E6F12",  "E6F20", "ES7_2", "Region", "E6F11", "E6C47c_11", "E6F6a" 


data.frame(colnames(data))

data_m <- data[,-c(6, 49, 50, 63, 72, 
                   124, 18, 26, 177, 4, 17, 90, 10)] 

# Check
mult2 <- PairApply(data_m, CramerV, symmetric = FALSE)
(vars2 <- which(mult2 <= 0.01, arr.ind = T)) #No multicollinear regressors



## Train-test split

set.seed(14)
ind <- sample(2, nrow(data_m),
              replace = TRUE,
              prob = c(0.7, 0.3))
training <- data_m[ind==1,]
testing <- data_m[ind==2,]


## Analysis

# Model 1
set.seed(14)
(linear <- lda(Level_MAT~., training))
linear$prior

# Most relevant features
# LD1
sort(abs(linear$scaling[,1]), decreasing = TRUE)[1:5]

# theta_READ +     E6F45 -   E6Co599 -     Centre_typeTiempo Extendido +     E6F44 -
sort(linear$scaling[,1], decreasing = TRUE)[1:5]


# LD2
sort(linear$scaling[,2], decreasing = TRUE)[1:5]

sort(abs(linear$scaling[,2]), decreasing = TRUE)[1:5]

# E6Co599 +    E6F44 +   ContextRural Quintil 2 +  E6F3299 +   E6F3399 +


#Based on the training dataset: 
#34.9% belongs to level 1 groups, 34.1% to level 2 and 31% belongs to level 3.

#The first discriminant function is a linear combination of the variables.
#Percentage separations achieved by the first discriminant function is 83.9% 
#and second is 16.1% 

#We can use the singular values to compute the amount of the between-group 
#variance that is explained by each linear discriminant. 
#In our example we see that the first linear discriminant explains 
#more than 83% of the between-group variance in the dataset.


#Plot

lda.data <- cbind(training, predict(linear)$x)

ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Level_MAT)) + 
  labs(color = 'Maths level') +
  theme_minimal()


# Make predictions
predictions <- linear %>% predict(testing)

# Model accuracy
mean(predictions$class==testing$Level_MAT)  #56.2% of accuracy

#Confusion matrix
p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$Level_MAT)
tab

sum(diag(tab))/sum(tab)

p2 <- predict(linear, testing)$class
tab1 <- table(Predicted = p2, Actual = testing$Level_MAT)
tab1

sum(diag(tab1))/sum(tab1) #check accuracy






### 3.2 Decision Trees ###


## Data preprocessing
## train-test split

set.seed(14)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind == 1, ]
test <- data[ind == 2, ]


## Analysis

set.seed(14)
tree <- rpart(Level_MAT ~., data = train, method = "class")
rpart.plot(tree)
summary(tree)

printcp(tree)
plotcp(tree)

p <- predict(tree, train, type = 'class')

confusionMatrix(p, train$Level_MAT, positive='y') #Accuracy of 58%



#The‘CP’ stands for Complexity Parameter of the tree. 
#We want the cp value of the smallest tree that has smallest cross validation error. 
#In regression, this means that the overall R-squared must increase by cp at each step.
#In other words, it refers to trade-off between the size of a tree and the error rate 
#to help prevent overfitting. 
#Thus large trees with a low error rate are penalized in favor of smaller trees.

# ROC Curve

p1 <- predict(tree, test, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test$Level_MAT, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')


# Tuning size of nodes

# Selecting the optimal subtree according to the 1-SE rule:
# xerror_min + 1*xstd

set.seed(14)
tree2 <- rpart(Level_MAT ~., data = train, cp = 0, method="class")
plotcp(tree2)  
abline(v = 3, lty = "dashed") #size 

printcp(tree2)

0.64501 + 1*0.013131 #0.658141 -> cp = 0.01299304 (2 plits)



#At size=7 the Cp is min

tree2$cptable

set.seed(14)
tree7 <- rpart(Level_MAT ~., data = train, cp = 0.00324826, method="class")
plotcp(tree7)  
rpart.plot(tree7)

summary(tree7)

p7 <- predict(tree7, train, type = 'class')
confusionMatrix(p7, train$Level_MAT, positive='y') #Accuracy of 61% (higher)





### Multi split Decision Tree (with only Factor variables)  >> MEMORY PROBLEM

#install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)

## train-test split

set.seed(14)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- df[ind == 1, -c(2,4)]
test <- df[ind == 2, -c(2,4)]

## Analysis

tree.chaid = chaid(Level_MAT ~., data = train)
plot(tree.chaid)

pmodel1 <- predict(tree.chaid)
confusionMatrix(pmodel1, test$Level_MAT) #Accuracy xx%




### 3.3 Random Forest ###

## train-test split

set.seed(14)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind == 1, ]
test <- data[ind == 2, ]

rf.tree = randomForest(Level_MAT~., train, importance=TRUE)
rf.tree

# error rate: 41.9%
# ntrees: 500
# mpty: 13



# Most important variables
varImpPlot(rf.tree)
#out.importance <- round(importance(rf.tree), 2)
#print(out.importance )


# Predictions
set.seed(14)
pred_rf <- predict(rf.tree, test[,-2])
table(pred_rf, test[,2])

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table(pred_rf, test[,2]))  #Accuracy 60%



## Model with 1000 trees
set.seed(14)
rf.tree2 = randomForest(Level_MAT~ ., train, importance=TRUE, ntree=1000)
rf.tree2

# error rate: 40.75%
# ntrees: 1000
# mpty: 13


# Plot the error vs number of trees
plot(rf.tree2)


# Predictions
pred_rf2 <- predict(rf.tree2, test[,-2])
table(pred_rf2, test[,2])

#Accuracy
accuracy(table(pred_rf2, test[,2]))  #Accuracy 60.33%



## Model with 500 trees and mtry 20

set.seed(14)
rf.tree3 = randomForest(Level_MAT~., train, ntree=500, mtry=20, importance=TRUE)
rf.tree3

# error rate: 41.17%
# ntrees: 500
# mpty: 20

# Predictions
pred_rf3 <- predict(rf.tree3, test[,-2])
table(pred_rf3, test[,2])

#Accuracy
accuracy(table(pred_rf3, test[,2]))  #Accuracy 61.5%



## Feature selection (BORUTA) ## >> Takes about 2 hours !!!

install.packages('Boruta')
library(Boruta)
library(mlbench)

## Boruta

set.seed(14)
boruta <- Boruta(Level_MAT~ ., train ,doTrace  = 2, maxRuns = 500)
print(boruta)

#Boruta performed 499 iterations in 1.68544 hours.
#50 attributes confirmed important: Age, Attendance, Centre_cat, Centre_type, Context and
#45 more;
#142 attributes confirmed unimportant: BE1_1, BE1_2, BE1_3, BE1_4, BE1_5 and 137 more;
#3 tentative attributes left: E6C40m_5, E6Co9_1, ES8_6;


#Model
getNonRejectedFormula(boruta)

#Level_MAT ~ Centre_cat + theta_READ + Region + Age + Centre_type + 
#  Context + E6F2 + E6F16 + E6F9 + E6F11 + E6F3 + E6F4 + E6F5a + 
#  E6F22 + E6F22a_5 + E6F22a_3 + E6F25a + E6Co3 + E6Co6_6 + 
#  E6Co7_4 + E6Co8_1 + E6Co9_1 + E6Co9_2 + E6Co9_3 + E6Co9_4 + 
#  E6Co9_6 + E6C47c_1 + E6C47c_13 + E6F34_1 + E6F34_2 + E6F34_5 + 
#  E6F34_6 + E6C42a + E6C40m_2 + E6C40m_4 + E6C40m_5 + E6C46a_2 + 
#  E6C46a_3 + E6C46m_4 + E6C48_5 + Attendance + ES1_6 + ES3_3 + 
#  ES2_1 + ES2_3 + ES2_4 + ES4_1 + ES4_2 + ES4_3 + ES4_4 + ES5_3 + 
#  ES5_5 + ES8_6


## Model with size 500
set.seed(14)
rf.boruta = randomForest(Level_MAT~ Centre_cat + theta_READ + Region + Age + Centre_type + 
                           Context + E6F16 + E6F9 + E6F11 + E6F3 + E6F4 + E6F5a + 
                           E6F22 + E6F22a_5 + E6F22a_3 + E6F25a + E6Co3 + E6Co6_6 + 
                           E6Co7_4 + E6Co8_1 + E6Co9_1 + E6Co9_2 + E6Co9_3 + E6Co9_4 + 
                           E6Co9_6 + E6C47c_1 + E6C47c_13 + E6F34_1 + E6F34_2 + E6F34_5 + 
                           E6F34_6 + E6C42a + E6C40m_2 + E6C40m_4 + E6C40m_5 + E6C46a_2 + 
                           E6C46a_3 + E6C46m_4 + E6C48_5 + Attendance + ES1_6 + ES3_3 + 
                           ES2_1 + ES2_3 + ES2_4 + ES4_1 + ES4_2 + ES4_3 + ES4_4 + ES5_3 + 
                           ES5_5 + ES8_6, train, importance=TRUE)
rf.boruta

# error rate: 41.14%
# ntrees: 500
# mpty: 7

pred_boruta <- predict(rf.boruta, test[,-2])
table(pred_boruta, test[,2])

confusionMatrix(pred_boruta, test$Level_MAT)

#Accuracy
accuracy(table(pred_boruta, test[,2]))  #Accuracy 60.4%


## Model with size 1000
set.seed(14)
rf.boruta2 = randomForest(Level_MAT~ Centre_cat + theta_READ + Region + Age + Centre_type + 
                           Context + E6F16 + E6F9 + E6F11 + E6F3 + E6F4 + E6F5a + 
                           E6F22 + E6F22a_5 + E6F22a_3 + E6F25a + E6Co3 + E6Co6_6 + 
                           E6Co7_4 + E6Co8_1 + E6Co9_1 + E6Co9_2 + E6Co9_3 + E6Co9_4 + 
                           E6Co9_6 + E6C47c_1 + E6C47c_13 + E6F34_1 + E6F34_2 + E6F34_5 + 
                           E6F34_6 + E6C42a + E6C40m_2 + E6C40m_4 + E6C40m_5 + E6C46a_2 + 
                           E6C46a_3 + E6C46m_4 + E6C48_5 + Attendance + ES1_6 + ES3_3 + 
                           ES2_1 + ES2_3 + ES2_4 + ES4_1 + ES4_2 + ES4_3 + ES4_4 + ES5_3 + 
                           ES5_5 + ES8_6, train, importance=TRUE, ntree=1000)

rf.boruta2

# error rate: 40.81%
# ntrees: 1000
# mpty: 7

pred_boruta2 <- predict(rf.boruta2, test[,-2])
table(pred_boruta2, test[,2])

confusionMatrix(pred_boruta2, test$Level_MAT)

#Accuracy
accuracy(table(pred_rf4, test[,2]))  #Accuracy 60.75%

# Most important variables
varImpPlot(rf.boruta2)




#############################
##  4.UNSUPERVISED ANALYSIS
#############################

### 1. PCA ###

# Package for performing dimension reduction
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
library(h2o)

h2o.no_progress()  # turn off progress bars for brevity
h2o.init()  # connect to H2O instance (Java required)

## Load and scale dataset ##

data <- df[,-c(2,5)] #keep Level_MAT and theta_READ

# Convert data to h2o object
data.h2o <- as.h2o(data)

# Run PCA
set.seed(14)
my_pca <- h2o.prcomp(
  training_frame = data.h2o,
  pca_method = "GLRM",             #For categorical variables
  use_all_factor_levels = TRUE,
  k = ncol(data.h2o), 
  transform = "STANDARDIZE", 
  impute_missing = TRUE,
  max_runtime_secs = 1000
)

my_pca@model$importance[,1:10]

# The first component explains 75.11% of the variance, PC2 5.2% and PC3 3.1%.


## Features influence on PC1

# Order by PC1

eigenvectors <- as.data.frame(my_pca@model$eigenvectors)
eigenvectors <- mutate(feature = row.names(eigenvectors), .data = eigenvectors)

eigen_pca1 <- eigenvectors[order(-eigenvectors$pc1),] 

eigen_pca1[1:30,] %>% 
  ggplot(aes(pc1, reorder(feature, pc1))) +
  labs(y='Top 30 features') +
  geom_point() +
  theme_minimal()

eigen_pca1[1:6,0:2]


# Feature loadings illustrating the influence that each variable 
# has on the first principal component.

## Features influence on PC2

# Order by PC2

eigen_pca2 <- eigenvectors[order(-eigenvectors$pc2),] 

eigen_pca2[1:30,] %>% 
  ggplot(aes(pc2, reorder(feature, pc2))) +
  labs(y='Top 30 features') +
  geom_point() +
  theme_minimal()

eigen_pca2[1:10,0:2]


## Features influence comparison between PC1 and PC2

library(plyr)

data.label <- ifelse(my_pca@model$eigenvectors[,1]>=0.048 |
                     my_pca@model$eigenvectors[,2]>=0.1077,
                     row.names(my_pca@model$eigenvectors),"")

set.seed(1)
my_pca@model$eigenvectors %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, pc2, label = data.label)) +
  geom_point(color = "steelblue", size = 2)+
  geom_label_repel(aes(label = data.label),
                  size          = 2,
                   box.padding   = 0.4,
                   point.padding = 0.5,
                   force         = 100,
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "y",
                   max.overlaps = 100) +
  theme_minimal()


## Compute eigenvalues
eigen <- my_pca@model$importance["Standard deviation", ] %>% as.vector() %>% .^2

## Sum of all eigenvalues equals number of variables (194)
sum(eigen)

## Number of PC: There are 3 common approaches in helping to make this decision ##

## 1. Find PCs where the sum of eigenvalues is greater than or equal to 1
which(eigen >= 1)

#10 PC


## 2.Proportion of variance explained criterion

# Extract and plot PVE and CVE

data.frame(
  PC  = my_pca@model$importance %>% seq_along(),
  PVE = my_pca@model$importance %>% .[2,] %>% unlist(),
  CVE = my_pca@model$importance %>% .[3,] %>% unlist()
) %>%
  tidyr::gather(metric, variance_explained, -PC) %>%
  ggplot(aes(PC, variance_explained)) +
  geom_point() +
  facet_wrap(~ metric, ncol = 1, scales = "free") +
  theme_minimal()


# We need just one PC to explain 75% of total variability

## 3. Scree plot criterion

data.frame(
  PC  = my_pca@model$importance %>% seq_along,
  PVE = my_pca@model$importance %>% .[2,] %>% unlist()
) %>%
  ggplot(aes(PC, PVE, group = 1, label = PC)) +
  geom_point() +
  geom_line() +
  xlim(0, 30) +
  geom_text(nudge_y = -.002) +
  geom_vline(xintercept = 2, color="blue", 
             linetype="dashed", size=0.5) +
  theme_minimal()

# A scree plot shows the eigenvalues or PVE for each individual PC. 
# Most scree plots look broadly similar in shape, starting high on the left, 
# falling rather quickly, and then flattening out at some point. 
# This is because the first component usually explains much of the variability, 
# the next few components explain a moderate amount, 
# and the latter components only explain a small fraction of the overall variability. 
# The scree plot criterion looks for the “elbow” in the curve 
# and selects all components just before the line flattens out, 
# which looks like 2 in our example.




### 2. K-means ###

## Preprocessing ##

## One-hot encoding categorical variables

# One-hot encode --> retain only the features (no theta_READ, no Level_MAT)
full_rank  <- caret::dummyVars(theta_READ ~ ., data[,-2], 
                               fullRank = TRUE)
data_1hot <- predict(full_rank, data)

# Scale data
data_1hot_scaled <- scale(data_1hot)

# New dimensions (649 features)
dim(data_1hot_scaled)


## Analysis ##

## Number of clusters (k)

set.seed(14)
fviz_nbclust(
  data_1hot_scaled, 
  kmeans, 
  method = "wss", 
  k.max = 15, 
  verbose = FALSE
)

# The results show the “elbow” appears to happen when k = 5

## Compute Gower distance for original data
set.seed(14)
gower_dst = daisy(data_1hot_scaled, metric = "gower")

## Fit the 5-means model with Gower distance
set.seed(14)
res_pam <- pam(x = gower_dst, k = 5, diss = TRUE)

summary(res_pam)
plot(res_pam)
res_pam$silinfo$clus.avg.widths


## Fit the 3-means model with Gower distance
set.seed(14)
res_pam3 <- pam(x = gower_dst, k = 3, diss = TRUE)
summary(res_pam3)
plot(res_pam3)

res_pam3$silinfo$clus.avg.widths


## Comparison to level_MAT ##

data_cl3 <- cbind(data, res_pam3$clustering)
colnames(data_cl3)[195] <- "cluster"


sjPlot::tab_xtab(data_cl3$cluster, data_cl3$Level_MAT, show.row.prc = TRUE)

## Chi-squared test

data_cl3$cluster <- as.factor(data_cl3$cluster)
associationTest(~cluster+Level_MAT, data=data_cl3)

# Variables are NOT independent at 99%


## Mosaic plot of Level_MAT by cluster

ggplot(data = data_cl3) +
  geom_mosaic(aes(x = product(Level_MAT, cluster), fill=Level_MAT)) +
  labs(title='Maths level by cluster', x='Cluster') +
  scale_fill_brewer(palette="Paired") +
  theme_minimal()

