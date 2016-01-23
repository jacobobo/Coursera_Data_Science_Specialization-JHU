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
png(file="plot4.png")

## Set Multi-up plot display to 2 x 2
par(mfrow = c(2,2))

## Plot 1
with(working, plot(Time, Global_active_power, type = "l", xlab = "",
                   ylab = "Global Active Power"))

## Plot 2
with(working, plot(Time, Voltage, type = "l", xlab = "datetime",
                   ylab = "Voltage"))

## Plot 3
with(working, plot(Time,Sub_metering_1,type = "l", xlab = "", 
                   ylab = "Energy sub metering"))
with(working, points(Time,Sub_metering_2, type = "l", col="red"))
with(working, points(Time,Sub_metering_3, type = "l", col="blue"))
legend("topright",lwd = 2,col = c("black", "red", "blue"), 
       legend = names(working[7:9]),bty="n")

## Plot 4
with(working,plot(Time,Global_reactive_power,type = "l", xlab="datetime"))

## Close PNG graphics device
dev.off()