# set working directory

setwd("~/Labwork/Apex")

# import data

data <- read.csv(file = "Compare.data.csv", header = TRUE, sep = ",")

# t-tests

# pH

t.test(data$Apex, data$Orion.Star)

pH.t <- t.test(data$Apex, data$Orion.Star)

# computed value
qt(0.975, 10.274)

# density graph for pH
names(pH.t)
pH.t$statistic
ts = replicate(1000,t.test(rnorm(10), rnorm(10))$statistic)
range(ts)
pts = seq(-4.3, 4.3, length=100)
plot(pts,dt(pts,df=10), col='red')
lines(density(ts))

# Temperature

t.test(data$Apex.1, data$Traceable)

temp.t <- t.test(data$Apex.1, data$Traceable)

# computed value
qt(0.975, 9.113)

# density graph for temperature
names(temp.t)
pH.t$statistic
ts.1 = replicate(1000,t.test(rnorm(10), rnorm(10))$statistic)
range(ts.1)
pts = seq(-4.03, 4.03, length=100)
plot(pts,dt(pts,df=10), col='blue')
lines(density(ts.1))


# Par

t.test(data$Apex.2, data$Apogee)

par.t <- t.test(data$Apex.2, data$Apogee)

# computed value
qt(0.975, 14.325)

# graph density plots
names(par.t)
pH.t$statistic
ts.2 = replicate(1000,t.test(rnorm(10), rnorm(10))$statistic)
range(ts.2)
pts = seq(-3.5, 3.5, length=100)
plot(pts,dt(pts,df=10), col='orange')
lines(density(ts.2))

# graph barcharts

library(ggplot2)

# pH 

p1 <- ggplot(data, aes(x = data$Group, y=data$pH)) + geom_boxplot(fill="grey80", colour = "blue") + scale_x_discrete() + xlab("Probe") + ylab("pH") + ggtitle("pH")

# Temperature

p2 <- ggplot(data, aes(x= data$Group.1, y=data$Temperature))+ scale_x_discrete() + geom_boxplot(fill="grey80", colour = "blue")  + xlab("Probe") + ylab("Temperature °C") + ggtitle("Temperature")

# PAR

p3 <- ggplot(data, aes(x= data$Group.2, y=data$Par)) + scale_x_discrete() + geom_boxplot(fill="grey80", colour = "blue") + xlab("Probe") + ylab("Par") + ggtitle("Par")


# Combine Graphs

library(Rmisc)

multiplot(p1, p2, p3, cols = 2)

# make histograms

# pH

h1 <- ggplot(data = data, aes(data$pH)) + geom_histogram(binwidth = 3.75,fill=I("Blue")) + xlab("pH") + ggtitle("Histogram for pH") + ylab("Count") 

# Temperature

h2 <- ggplot(data = data, aes(data$Temperature)) + geom_histogram(binwidth = 3.75, fill=I("Blue")) + xlab("Temperature (°C) ") + ggtitle("Histogram for Temperature") + ylab("Count")


# Par

h3 <- ggplot(data = data, aes(data$Par)) + geom_histogram(binwidth = 3.75, fill=I("Blue")) + xlab("Par") + ggtitle("Histogram for Par") + ylab("Count")

# multiplot
 
multiplot(h1, h2, h3, cols = 2)
