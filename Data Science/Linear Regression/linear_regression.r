####################################################################
# Learning Data Science - Linear Regression                        #
# Fun Series October                                               #
# Author: Zexi chen(Unity ID: zchen22)                             #
####################################################################

rm(list=ls(all=T))

# read the file
df = read.csv("Advertising.csv",head = T, sep = ",")

# calculate the rows of the file
nrow(df)

# copy the first column of the data file
plot(df$TV,df$Sales,pch=19,cex=0.50,main="The points for TV and Sales\nwith Ruler Estimate")
# draw a line 
abline(3, 0.05,col=rgb(0.2,0.8,0.2,0.5),lwd=3)

# calculate the linear model
linear_model <- lm(df$Sales~df$TV)
# print the result for the linear model
print(linear_model)

# plot the actual linear regression line
plot(df$TV,df$Sales,pch=19,cex=0.50,main="The points for TV and Sales\nand the Line from lm()")
abline(3, 0.05,col=rgb(0.2,0.8,0.2,0.5),lwd=3)
intercept = coef(linear_model)[1]
slope = coef(linear_model)[2]
#plot the line from lm()
abline(intercept,slope,col=rgb(0,0,1,0.5),lwd=3)

rawScore <- function(range, slope1, intercept1, slope2, intercept2) {
  sum <- 0
  # Note: written as loop instead of 
  # vector operations for accessibility
  for (x in range) {
    y1 <- slope1 * x + intercept1
    y2 <- slope2 * x + intercept2
    vdist <- abs(y1 - y2)
    sum <- sum + vdist
  }
  ans <- sum/length(range)
  # it will set the attributes of the variable to null. If it is a matrix,
  # it will be converted to a vector. Other uses refer to ?attributes
  attributes(ans) <- NULL
  return(ans)
}

# run the function
rawScore(1:50, 1, 10, 1, 5)