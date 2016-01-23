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
png(file="plot2.png")

## Create line plot of Global active power over days and label
with(working, plot(Time, Global_active_power, type = "l", xlab = "",
                   ylab = "Global Active Power (kilowatts)"))

## Close PNG graphics device
dev.off()