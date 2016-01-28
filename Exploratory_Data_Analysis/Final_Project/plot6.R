library(plyr)

## Read in the two RDS files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Find which rows in SCC contain the word "Vehicles"
cars <- grep("Vehicles", SCC$EI.Sector, ignore.case = TRUE)

## Subset the SCC codes
SCCs <- SCC$SCC[cars]

## Subset the Baltimore data from NEI with the correct SCC codes
BC <- subset(NEI, fips == "24510" & SCC %in% SCCs, select = Emissions:year)

## Subset the LA County data from NEI with the correct SCC codes
LAC <- subset(NEI, fips == "06037" & SCC %in% SCCs, select = Emissions:year)

## Get sums of Emissions for each subset by year
plotBC <- ddply(BC, "year", numcolwise(sum))
plotLAC <- ddply(LAC, "year", numcolwise(sum))

## Open PNG graphics device
png(file="plot6.png")

## Set for 2-column multiplot with an outer margin at the top
par(oma=c(0,0,2,0),mfrow = c(1,2))

plot(plotBC, type = "l", col = "red", xlab = "Year", ylab = "Emissions (Tons)", 
     main = "Baltimore City")
plot(plotLAC, type = "l", col = "blue", xlab = "Year", 
     ylab = "Emissions (Tons)", main = "Los Angeles County")

## Add main title to outer margin
title(main = "Total PM2.5 Emissions From Motor Vehicle Sources, 1999-2008", 
      outer = TRUE)

## Close PNG graphics device
dev.off()