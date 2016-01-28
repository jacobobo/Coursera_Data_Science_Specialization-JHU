library(plyr)

## Read in the two RDS files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Find which rows in SCC contain the word "Coal"
coal <- grep("Coal", SCC$EI.Sector, ignore.case = TRUE)

## Subset the SCC codes
SCCs <- SCC$SCC[coal]

## Subset the Baltimore data from NEI with the correct SCC codes
x <- subset(NEI, SCC %in% SCCs, select = Emissions:year)

## Get sums of Emissions for each subset by year
plotdata <- ddply(x, "year", numcolwise(sum))

## Reduce scale for better plotting
plotdata$Emissions <- plotdata$Emissions/1000

## Open PNG graphics device
png(file="plot4.png")

plot(plotdata, type = "l", xlab = "Year", 
     ylab = "Emissions (Thousands of Tons)", 
     main = "Total PM2.5 Emissions From Coal Combustion-Related Sources \n
     United States from 1999-2008")

## Close PNG graphics device
dev.off()