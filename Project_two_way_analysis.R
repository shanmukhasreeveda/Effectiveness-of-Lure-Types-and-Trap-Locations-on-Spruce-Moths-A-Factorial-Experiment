
#---------------------
# Read csv file into R
#---------------------
traps_data <- read.csv("/Users/sreevedatippavajhala/Desktop/STAT 271 Datasets/Spruce_Moth_Traps.csv")
head(traps_data)
str(traps_data) 
attach(traps_data)

#---------------------------------------------------------
# Create individual value plots of all factor-level groups
#---------------------------------------------------------
#install.packages("lattice")
library(lattice)

# plot to visulise the individual data
xyplot(Moths ~ factor(Location):factor(Lure))

#----------------------------------
# Scatter plot with multiple groups
#----------------------------------

#install.packages("ggplot2")
library(ggplot2)

ggplot(traps_data, aes(x=factor(Lure), y=Moths, shape=factor(Lure), color=factor(Lure))) +
  geom_point() +
  facet_wrap(~Location) +
  labs( title = "Individual Value Plot of Mouths Found",
        x = "Type of Lure in trap"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) # center plot title

#------------------------------
# Box plot with multiple groups
#------------------------------

#install.packages("ggpubr")
library(ggpubr)

ggboxplot(traps_data, x="Lure", y="Moths", color="Lure") +
  facet_wrap(~Location) +
  labs( title = "Box Plot of Number of Moths found",
        x = "Type of lure in trap"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#----------------------------------------
# Compute descriptive statistics by group
#----------------------------------------
mean(Moths[Location=="Top"])
mean(Moths[Location=="Middle"])
mean(Moths[Location=="Lower"])
mean(Moths[Location=="Ground"])
mean(Moths[Lure=="Scent"])
mean(Moths[Lure=="Sugar"])
mean(Moths[Lure=="Chemical"])
mean(Moths)

mean(Moths[(Location=="Top") & (Lure=="Scent")])
mean(Moths[(Location=="Top") & (Lure=="Sugar")])
mean(Moths[(Location=="Top") & (Lure=="Chemical")])
mean(Moths[(Location=="Middle") & (Lure=="Scent")])
mean(Moths[(Location=="Middle") & (Lure=="Sugar")])
mean(Moths[(Location=="Middle") & (Lure=="Chemical")])
mean(Moths[(Location=="Lower") & (Lure=="Scent")])
mean(Moths[(Location=="Lower") & (Lure=="Sugar")])
mean(Moths[(Location=="Lower") & (Lure=="Chemical")])
mean(Moths[(Location=="Ground") & (Lure=="Scent")])
mean(Moths[(Location=="Ground") & (Lure=="Sugar")])
mean(Moths[(Location=="Ground") & (Lure=="Chemical")])

#--------------------------------------
# Two-way ANOVA with interaction effect
#--------------------------------------

anova2 <- aov(Moths ~ Location * Lure, data = traps_data )
summary(anova2)

#Location: p-value = 2.09* 10 ^-5 < 0.05, we  reject H0, there is significant evidence to conclude at least one of u_top, u_middle, u_lower, u_ground differe from others.
#Lure: p-value = 0.416 > 0.05, we fail to reject H0, there is in significant evidence to conclude u_Scent, u_Sugar, u_Chemical differe.
#Location * Lure: p-value = 0.932 > 0.05, we fail to reject H0, there is no interaction effect between location and Lure.

#------------------
# Mean effect plots
#------------------

# first download and install �gplots� package, then activate it with a library command 

#install.packages("gplots")
library(gplots)
plotmeans(Moths ~ Location, xlab=" Location of trap in tree", ylab="Number of Spruce Moth found", main="Mean Effect Plot for Location")
plotmeans(Moths ~ Lure, xlab="Type of lure in trap", ylab="Number of Spruce Moth found", main="Mean Effect Plot for Type of lure in trap")

#---------------------------------------------------------------------------------
# The Levene's test for equality of variances is in the additional "car" package
#---------------------------------------------------------------------------------

# leveneTest is present in car package

#install.packages("car") 
library(car)

leveneTest(anova2)

#p-value = 0.7875 > 0.05, which indicates the variances are equal.

#--------------------------------
# Compute some summary statistics
#--------------------------------

#  using "tapply" function
tapply(Moths, list(Location,Lure), sd)

#To find the maximum and minimum standard deviations
max(tapply(Moths, list(Location,Lure), sd))
min(tapply(Moths, list(Location,Lure), sd))

sd.max <- max(tapply(Moths, list(Location,Lure), sd))
sd.min <- min(tapply(Moths, list(Location,Lure), sd))
sd.max/sd.min 
# informal check: max(s)/min(s) = 3.139467 > 2, it is not appropriate to assume equal variances.

#-----------------------------------------------------------------------------
# The residuals versus fits plot is used to check the homogeneity of variances
#-----------------------------------------------------------------------------
plot(anova2, 1) 
# 1 indicates showing the first plot

#-----------------------------------
#Produce a histogram of the residuals
#-----------------------------------
res <- anova2$residuals
hist(res, main="Histogram of residuals", xlab="Residuals", prob = TRUE)

mu <- mean(res)  # Calculate mean of residuals
sigma <- sd(res) # Calculate standard deviation of residuals
curve(dnorm(x, mean = mu, sd = sigma), col = "blue", lwd = 2, add = TRUE) # Plot normal curve

#The histogram is fairly symmetric and mound shaped, 
#so the normal population assumption for the random error terms is reasonable.


#--------------------------------
# Normality plot of the residuals
#--------------------------------
plot(anova2, 2, col = "blue") # 2 indicates showing the second plot

#The residuals follow a fairly straight line in the normal probability plot,
#so the normal population assumption for the random error terms is reasonable.


#-------------------------
# Two-way interaction plot
#-------------------------
interaction.plot(x.factor = Location, trace.factor = Lure, 
                 response = Moths, fun = mean, 
                 type = "b", legend = TRUE,
                 pch=c(1,19), col = c(2, 4), 
                 xlab = "Location of trap in tree", ylab="Number of Spruce Moth found", main="Interaction plot")

interaction.plot(x.factor = Lure, trace.factor = Location, 
                 response = Moths, fun = mean, 
                 type = "b", legend = TRUE,
                 pch=c(1,19), col = c(2, 4), 
                 xlab = "Tupe of Lure", ylab="Number of Spruce Moth found", main="Interaction plot")

#Two lines cross, which indicates a possible interaction between Lure and Location.



#------------------------------------
# Tukey multiple pairwise-comparisons
#------------------------------------
TukeyHSD(anova2)

#The Location effect is statistically significant  except for Top-Ground, Middle-Lower  combinations
#The Lure effect is statistically in significant.



