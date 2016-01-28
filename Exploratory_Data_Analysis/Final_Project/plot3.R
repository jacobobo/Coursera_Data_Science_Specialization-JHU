library(ggplot2)
library(plyr)

## Read in the two RDS files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Subset the Baltimore data from NEI
x <- subset(NEI,fips == "24510", select = Emissions:year)

## Get sums of Emissions for each subset by year and source
plotdata <- ddply(x, c("year","type"), numcolwise(sum))

## Open PNG graphics device
png(file="plot3.png")

qplot(year, Emissions, data = plotdata, color = type, geom = "line", 
      xlab = "Year", ylab = "Emissions (Tons)", 
      main = "PM25 Emissions in Baltimore city, MD from 1999-2008 by Source")

## Close PNG graphics device
dev.off()