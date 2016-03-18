require(ggplot)
require(gridExtra)

## The data file was downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
## and unzipped into the workin directory into repdata-data-StormData.csv

if(!exists("stormData")){
    stormData <- read.csv("repdata-data-StormData.csv",header=TRUE, stringsAsFactors = FALSE)
}

## Let us look at stormData

str(stormData)

## Since this is too much data loaded into the memory, let us keep only the variables we need for the present analysis 
## and get rid of the rest

stormData <- stormData[,c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

## We are only interested in the YEAR part of BGN_DATE. So let us extract that and get rid of BGN_DATE

stormData$YEAR <- as.numeric(format(as.Date(stormData$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
stormData$BGN_DATE <- NULL

## Now we will make a histogram showing the data distribution between 1950- 2011

hist(stormData$YEAR, breaks = 60, xlab = "YEAR", main = "Storm Events recorded between 1950-2011")

## We notice that the number of events being recorded increased significantly mid-1990s onwards.

# QUESTION-1 which types of events are most harmful with respect to population health?

## We can answer this by totaling all FATALITIES & INUJURIES for each type of storm event between 1950 and 2011 and
## check which event type resulted in most FATALITIES & INUJURIES during this period.
## How this result can be biased if a certain event type happened to be recorded more than ## another event type 
## (esp during the period 1950-1995 when not all event types seem to have been recorded)

## TOP 10 FATALITIES CAUSING EVENT TYPES:

totalFatalities <- aggregate(stormData$FATALITIES, by = list(stormData$EVTYPE), sum)
names(totalFatalities) <- c("Event.Type", "Fatalities")
totalFatalities <- head(totalFatalities[order(totalFatalities$Fatalities, decreasing =TRUE), ], 10)
within(totalFatalities, Event.Type <- factor(x = Event.Type, levels = totalFatalities$Event.Type))
totalFatalities

## TOP 10 INJURIES CAUSING EVENT TYPES:
totalInjuries <- aggregate(stormData$INJURIES, by = list(stormData$EVTYPE), "sum")
names(totalInjuries) <- c("Event.Type", "Injuries")
totalInjuries <- head(totalInjuries[order(totalInjuries$Injuries, decreasing =TRUE), ], 10)
within(totalInjuries, Event.Type <- factor(x = Event.Type, levels = totalInjuries$Event.Type))
totalInjuries

## Here are the plots that show the results. 
## As we can see, TORNADO is by far the top most cause of Fatalities and Injuries
fPlot <- ggplot(totalFatalities, aes(x = as.factor(Event.Type),y =Fatalities)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Total Fatalities") + xlab("Event Type") +
    ggtitle('Total FATALITIES due to major storm events\nin the US between 1950-2011')

iPlot <- ggplot(totalInjuries, aes(x = as.factor(Event.Type),y =Injuries)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Total Injuries") + xlab("Event Type") +
    ggtitle('Total INJURIES due to major storm events\n in the US between 1950-2011')

grid.arrange(fPlot, iPlot, ncol = 2)
                                                                                                                                              
# QUESTION-2 which types of events have the greatest economic consequences?

## We can answer this by totaling all property and crop damages for each type of storm event between 1950 and 2011 and
## check which event type resulted in most FATALITIES & INUJURIES during this period.
## Again, this result can be biased if a certain event type happened to be recorded more than ## another event type 
## (esp during the period 1950-1995 when not all event types seem to have been recorded)

# First, we will convert the property damage and crop damage data (in both PROPDMGEXP and CROPDMGEXP columns) as per
# units described in the code book (Storm Events) -> Hundred (H), Thousand (K), Million (M) and Billion (B):

## POPERTY DAMAGE FIELD CONVERSION

stormData$PROPDMGEXP <- as.character(stormData$PROPDMGEXP)
stormData$PROPDMGEXP[toupper(stormData$PROPDMGEXP) == 'H'] <- "2"
stormData$PROPDMGEXP[toupper(stormData$PROPDMGEXP) == 'K'] <- "3"
stormData$PROPDMGEXP[toupper(stormData$PROPDMGEXP) == 'M'] <- "6"
stormData$PROPDMGEXP[toupper(stormData$PROPDMGEXP) == 'B'] <- "9"
stormData$PROPDMGEXP <- as.numeric(stormData$PROPDMGEXP)
stormData$PROPDMGEXP[is.na(stormData$PROPDMGEXP)] <- 0

## A new field PROPDAMAGECOST contains the result of multiplying PROPDMG with 10^n^ (where n is got from PROPDMGEXP)
stormData$PROPDAMAGECOST <- stormData$PROPDMG * 10^stormData$PROPDMGEXP


## CROP DAMAGE FIELD CONVERSION
stormData$CROPDMGEXP <- as.character(stormData$CROPDMGEXP)
stormData$CROPDMGEXP[toupper(stormData$CROPDMGEXP) == 'H'] <- "2"
stormData$CROPDMGEXP[toupper(stormData$CROPDMGEXP) == 'K'] <- "3"
stormData$CROPDMGEXP[toupper(stormData$CROPDMGEXP) == 'M'] <- "6"
stormData$CROPDMGEXP[toupper(stormData$CROPDMGEXP) == 'B'] <- "9"
stormData$CROPDMGEXP <- as.numeric(stormData$CROPDMGEXP)
stormData$CROPDMGEXP[is.na(stormData$CROPDMGEXP)] <- 0

## A new field TOTALPROPDMGCOST contains the result of multiplying CROPDMG with 10^n^ (where n is got from CROPDMGEXP)
stormData$CROPDAMAGECOST <- stormData$CROPDMG * 10^stormData$CROPDMGEXP


## TOP 10 EVENTS CAUSING PROPERTY DAMAGE:
totalPropDamage <- aggregate(stormData$PROPDAMAGECOST, by = list(stormData$EVTYPE), sum)
names(totalPropDamage) <- c("Event.Type", "Total.Prop.Damage")
totalPropDamage <- head(totalPropDamage[order(totalPropDamage$Total.Prop.Damage, decreasing =TRUE), ], 10)
within(totalPropDamage, Event.Type <- factor(x = Event.Type, levels = totalPropDamage$Event.Type))
totalPropDamage

## TOP 10 EVENTS CAUSING PROPERTY DAMAGE:
totalCropDamage <- aggregate(stormData$CROPDAMAGECOST, by = list(stormData$EVTYPE), sum)
names(totalCropDamage) <- c("Event.Type", "Total.Crop.Damage")
totalCropDamage <- head(totalCropDamage[order(totalCropDamage$Total.Crop.Damage, decreasing =TRUE), ], 10)
within(totalCropDamage, Event.Type <- factor(x = Event.Type, levels = totalCropDamage$Event.Type))
totalCropDamage

## Here are the plots that illustrate these results. 
## As we can see, TORNADO is by far the top most cause of Fatalities and Injuries
pPlot <- ggplot(totalPropDamage, aes(x = as.factor(Event.Type),y =Total.Prop.Damage)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Total Property Damage Cost") + xlab("Event Type") +
    ggtitle('Total PROPERTY DAMAGE COST due to\n major storm events in the US between 1950-2011')

cPlot <- ggplot(totalCropDamage, aes(x = as.factor(Event.Type),y =Total.Crop.Damage)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Total Crop Damage Cost") + xlab("Event Type") +
    ggtitle('Total CROP DAMAGE COST due to\n major storm events in the US between 1950-2011')

grid.arrange(pPlot, cPlot, ncol = 2)
