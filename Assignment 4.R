#Part 1
ecomm.df = read.csv("https://goo.gl/hzRyFd")
#[1]
dim(ecomm.df)
#[2]
table(ecomm.df$country)
#[3]
#Canada
#[4]
na.omit(ecomm.df$intentWasPlanningToBuy)
table(ecomm.df$intentWasPlanningToBuy,ecomm.df$profile)
#[5]
#Yes 96/16+174+96
96/(16+174+96)
#[6]
#+ Yes 14/9+35+14
14/(9+35+14)
#[7]
max(table(ecomm.df$region))
#TX 94
#[8]
mysummary.df=data.frame(matrix(NA, nrow=2, ncol=2))
colnames(mysummary.df)=c("Mean", "SD")
rownames(mysummary.df)=c("Parent", "Teacher")
mysummary.df["Parent", "Mean"]=mean(ecomm.df$behavNumVisits[ecomm.df$profile=="Parent"])
mysummary.df["Parent", "SD"]=sd(ecomm.df$behavNumVisits[ecomm.df$profile=="Parent"])
mysummary.df["Teacher", "Mean"]=mean(ecomm.df$behavNumVisits[ecomm.df$profile=="Teacher"])
mysummary.df["Teacher", "SD"]=sd(ecomm.df$behavNumVisits[ecomm.df$profile=="Teacher"])
mysummary.df
#Mean Parent:1.878866 Teacher:1.269058; SD Parent:2.654421 Teacher:0.8105623
#[9]
mysummary.df=data.frame(matrix(NA, nrow=2, ncol=2))
colnames(mysummary.df)=c("Mean", "SD")
rownames(mysummary.df)=c("Parent", "Teacher")
mysummary.df["Parent", "Mean"]=round(mean(ecomm.df$behavNumVisits[ecomm.df$profile=="Parent"]),2)
mysummary.df["Parent", "SD"]=round(sd(ecomm.df$behavNumVisits[ecomm.df$profile=="Parent"]),2)
mysummary.df["Teacher", "Mean"]=round(mean(ecomm.df$behavNumVisits[ecomm.df$profile=="Teacher"]),2)
mysummary.df["Teacher", "SD"]=round(sd(ecomm.df$behavNumVisits[ecomm.df$profile=="Teacher"]),2)
mysummary.df
#Mean Parent:1.88 Teacher:1.27; SD Parent:2.65 Teacher:0.81

#Part 2
library(datasets)
data("mtcars")
?mtcars
#[1]
hist(mtcars$hp,main="Horse Power*Frequeny",xlab="Horse Power(hp)")
plot(table(mtcars$hp),main="Horse Power*Frequeny",xlab="Horse Power(hp)",ylab="Frequency")
#plot cause we can know exactly how many cars in the dataset is correspond to different hp.
#[2]
plot(mtcars$hp,mtcars$mpg,main="mpg*hp",xlab="Horse Power(hp)",ylab="Miles Per Gallon(mpg)")
#[3]
cor(mtcars$mpg,mtcars$hp)
#[4]
cor.test(mtcars$mpg,mtcars$hp)
#Yes, because the p value is smaller than 0.05.
#[5]
cor.test(mtcars$mpg,mtcars$wt)
#weight, cause the correlation of weight is closer to -1 than hp.
#[6]
table(mtcars$cyl)
by(data=mtcars$mpg,INDICES=mtcars$cyl,FUN=mean)
mysummary.df=data.frame(matrix(NA, nrow=3, ncol=1))
colnames(mysummary.df)=("Mean of mpg")
rownames(mysummary.df)=c("4","6","8")
mysummary.df["4","Mean of mpg"]=mean(mtcars$mpg[mtcars$cyl==4])
mysummary.df["6","Mean of mpg"]=mean(mtcars$mpg[mtcars$cyl==6])
mysummary.df["8","Mean of mpg"]=mean(mtcars$mpg[mtcars$cyl==8])
mysummary.df
#The lower the cylinders, the higher the mean of mpg
#[7]
by(data=mtcars$mpg,INDICES=mtcars[,c(2,9)],FUN=mean)
mysummary.df=data.frame(matrix(NA, nrow=6, ncol=1))
colnames(mysummary.df)=("Mean of mpg")
rownames(mysummary.df)=c("4 with am=0","6 with am=0","8 with am=0","4 with am=1","6 with am=1","8 with am=1")
mysummary.df["4 with am=0","Mean of mpg"]=mean(mtcars$mpg[mtcars$cyl==4&mtcars$am==0])
mysummary.df["6 with am=0","Mean of mpg"]=mean(mtcars$mpg[mtcars$cyl==6&mtcars$am==0])
mysummary.df["8 with am=0","Mean of mpg"]=mean(mtcars$mpg[mtcars$cyl==8&mtcars$am==0])
mysummary.df["4 with am=1","Mean of mpg"]=mean(mtcars$mpg[mtcars$cyl==4&mtcars$am==1])
mysummary.df["6 with am=1","Mean of mpg"]=mean(mtcars$mpg[mtcars$cyl==6&mtcars$am==1])
mysummary.df["8 with am=1","Mean of mpg"]=mean(mtcars$mpg[mtcars$cyl==8&mtcars$am==1])
mysummary.df
#There is no big difference between auto or manual when the cylinder is 6 or 8, but a big difference when cylinder is 4.
#[8]
boxplot(mpg~cyl,data=mtcars,main="Mileage by number of Cylinders ",xlab="Cylinder",ylab="mpg")
#When cylinder equals 4, the range is the largest; when it is 6, the range is the smallest
#[9]
pairs(mtcars[,c(1,4,6,7)],pch=16,cex=.8)
#[10]
#Compared to hp, qsec is less correlated to mpg and wt.

