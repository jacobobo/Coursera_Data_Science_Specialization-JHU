## Read in data file, with a header, separators as semi-colons, and replacing
## all the '?' with NA
mydata <- read.table("household_power_consumption.txt", header = TRUE, 
                     sep = ";", na.strings = "?")

## Change the format of the date column to a recognized date
mydata$Date <- as.Date(mydata$Date,"%d/%m/%Y")

## Create a working subset of the two needed days of data
working <- subset(mydata, 
                  mydata$Date == "2007-02-01" | mydata$Date == "2007-02-02")

## Paste the Date and Time together
tmp <- paste(working$Date, working$Time)

## Make the time column have the full POSIXlt class so it's more useable
working$Time <-strptime(tmp, "%Y-%m-%d %H:%M:%S")

## Open PNG graphics device
png(file="plot3.png")

## Create line plot with first set of data and add labels
with(working, plot(Time,Sub_metering_1,type = "l", xlab = "", 
                   ylab = "Energy sub metering"))

## Add in next set of data in red
with(working, points(Time,Sub_metering_2, type = "l", col="red"))

## Add in last set of data in blue
with(working, points(Time,Sub_metering_3, type = "l", col="blue"))

## Add legend in the top right with data column names and matching colors
legend("topright",lwd = 2,col = c("black", "red", "blue"), 
       legend = names(working[7:9]))

## Close PNG graphics device
dev.off()