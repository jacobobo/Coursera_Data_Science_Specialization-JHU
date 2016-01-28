## Read in the two RDS files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Pull out unique years of the data
x <- unique(NEI$year)

## Subset NEI for the Emissions from each year
x9 <- subset(NEI, year == 1999, select = Emissions)
x2 <- subset(NEI, year == 2002, select = Emissions)
x5 <- subset(NEI, year == 2005, select = Emissions)
x8 <- subset(NEI, year == 2008, select = Emissions)

## Find the total emissions for each year
y <- c(sum(x9), sum(x2), sum(x5), sum(x8))

## Reduce scale for better plotting
y <- y/1000000

## Open PNG graphics device
png(file="plot1.png")

plot(x,y,type = "l", main = "Total PM2.5 Emissions in the US from 1999-2008", 
     xlab = "Year", ylab = "Total Emissions (Millions of Tons)")

## Close PNG graphics device
dev.off()