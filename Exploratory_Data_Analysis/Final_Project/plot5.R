library(plyr)

## Read in the two RDS files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Find which rows in SCC contain the word "Vehicles"
cars <- grep("Vehicles", SCC$EI.Sector, ignore.case = TRUE)

## Subset the SCC codes
SCCs <- SCC$SCC[cars]

## Subset the Baltimore data from NEI with the correct SCC codes
x <- subset(NEI, fips == "24510" & SCC %in% SCCs, select = Emissions:year)

## Get sums of Emissions for the subset by year
plotdata <- ddply(x, "year", numcolwise(sum))

## Open PNG graphics device
png(file="plot5.png")

plot(plotdata, type = "l", xlab = "Year", ylab = "Emissions (Tons)", 
     main = "Total PM2.5 Emissions From Motor Vehicle Sources \n
     Baltimore City, MD from 1999-2008")

## Close PNG graphics device
dev.off()