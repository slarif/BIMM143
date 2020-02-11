#' ---
#' title: "Class 5: Data Visualization and Graphs in R"
#' author: "Sarra Larif"
#' date: "January 21st, 2020"
#' ---

plot(1:10, col="red", typ="o")
#Need to send script to R consol for it to actually execute code 
#Can copy/paste or do control+enter (cursor must be on line you want to execute)
#Can also press "Run" button to run line or "Source" to run everything

#Need to import/read input data file first
baby <- read.table("bimm143_05_rstats/weight_chart.txt")
#Have to "argue" with it to make headers in header space rather than as 2 data points
baby <- read.table("bimm143_05_rstats/weight_chart.txt", header = TRUE)

#A basic plor of age vs. weight
plot(baby$Age, baby$Weight, col="purple", type = "o", pch = 15, cex = 1.5, lwd = 2, 
     ylim = c(2,10), xlab = "Age (months)", ylab = "Weight (kg)", main = "Suitable Title" )
#$ or [] accesses that column of the table
#MAKE SURE TO PUT ALL ARGUMENTS IN QUOTATION MARKS 

#An example of 'pch' plot character and 'cex' size
plot(1:5, cex = 1:5, pch = 1:5)

#Bar plot with mouse genome features
# mouse <- read.table("bimm143_05_rstats/bimm143_05_rstats/feature_counts.txt")
#Doesn't work because "line three did not have 3 elements" because line 3 has a tab, not space
mouse <- read.table("bimm143_05_rstats/feature_counts.txt", header = TRUE, sep = "\t")
barplot(mouse$Count, horiz = TRUE, col = "lightblue", 
        names.arg = mouse$Feature, las = 1)

par(mar=c(5,11,2,1))
barplot(mouse$Count, horiz = TRUE, col = "lightblue", 
        names.arg = mouse$Feature, las = 1)

par(mar=c(5,4,2,2))
plot(1:10)

#Histogram 
x <- c(rnorm(10000),rnorm(10000)+4)
hist(x, breaks=50)

#Color vectors
gender <- read.table("bimm143_05_rstats/male_female_counts.txt", header = TRUE, sep="\t")
nrow(gender)
barplot(gender$Count, col = rainbow(10))

#Prof's way
mf <- read.delim("bimm143_05_rstats/male_female_counts.txt")
barplot(mf$Count, names.arg = mf$Sample, col=rainbow(10))
nrow(mf)

#same plot, different colors
barplot(mf$Count, names.arg = mf$Sample, col=c("red", "blue"), las = 2)

#more stuff
genes <- read.delim("bimm143_05_rstats/up_down_expression.txt")
table(genes$State)
