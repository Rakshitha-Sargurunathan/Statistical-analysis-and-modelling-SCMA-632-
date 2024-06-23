setwd('E:\\VCU\\SCMA\\DATA')
install.packages('AER') #AER package contains functions and datasets for applied econometrics with R.
library(AER)

data("Affairs")
head(Affairs)
unique(Affairs$affairs)
table(Affairs$affairs)

## from Table 22.4 in Greene (2003)
fm.tobit <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating ,
                  data = Affairs)
fm.tobit2 <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
                   right = 4, data = Affairs)

summary(fm.tobit)
summary(fm.tobit2)

# Fit a Tobit Model to real data
df = read.csv('NSSO68.csv', header=TRUE)
unique(df$state_1)
dput(names(df))

#Subsetting and Preprocessing the Data
df_Bhr = df[df$state_1== 'Bhr',]
vars <- c("Sector", "hhdsz", "Religion", "Social_Group", "MPCE_URP", "Sex", "Age", "Marital_Status", "Education", "chicken_q", "chicken_v")

df_Bhr_p = df_Bhr[vars]
names(df_Bhr_p)

df_Bhr_p$price = df_Bhr_p$chicken_v / df_Bhr_p$chicken_q
names(df_Bhr_p)

summary(df_Bhr_p)

head(table(df_Bhr_p$chicken_q))

dim(df_Bhr_p)

# Fitting a Multiple Linear regression Model with chicken_q as the dependent variable

fit = lm(chicken_q ~ hhdsz+ Religion+ MPCE_URP+ Sex+ Age+ Marital_Status+ Education +price , data=df_Bhr_p)
summary(fit) 

# Fitting a Tobit Model to the data
install.packages('GGally')
install.packages('VGAM')
install.packages('ggplot2')
library(GGally)
library(VGAM)
library(ggplot2)

exp(-1.104e+00)
sd(df_Bhr_p$chicken_q)
var(require(ggplot2)
    require(GGally)
    require(VGAM)
    ggpairs(df_Bhr_p[, c("chicken_q", "MPCE_URP", "price")])
    
    m <- vglm(chicken_q ~ hhdsz+ Religion+ MPCE_URP+ Sex+ Age+ Marital_Status+ Education +price, tobit(Lower = 0), data = df_Bhr_p)
    summary(m)
    
    exp(-1.032e+00)
    sd(df_Bhr_p$chicken_q)
    df_Bhr_p$price[is.na(df_Bhr_p$price)] <- 0
    http://127.0.0.1:19305/graphics/ae84d2ce-1d21-4c33-b25d-4421bf7e41ee.png
    m <- vglm(chicken_q ~ hhdsz+ Religion+ MPCE_URP+ Sex+ Age+ Marital_Status+ Education +price, tobit(Lower = 0), data = df_Bhr_p)
    summary(m)
    