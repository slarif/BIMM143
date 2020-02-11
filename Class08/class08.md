Class 08
================
Sarra Larif
1/30/2020

``` r
## This function will generate random substrings from the larger reference sequence 
GenerateSimulatedReads <- function(refSequence, readLength, numberOfReadsToSimulate) {
  
  startPositions <- sample(x = c(1:(nchar(refSequence) - readLength)), size = numberOfReadsToSimulate, replace = T)
  endPositions   <- startPositions + readLength - 1
  sequenceVector    <- substring(as.character(refSequence), startPositions, endPositions)
  sequenceDataFrame     <- data.frame(startPositions, endPositions, sequenceVector, stringsAsFactors = FALSE)
  return(sequenceDataFrame)
}

# sample(x = c(10), size = 5, replace = T)
# substring(as.character("ATCGCGCATATACCGATCGT"), c(1,3,5), c(3,9,12))
# sequenceVector = substring(as.character("ATCGCGCATATACCGATCGT"), c(1,3,5), c(3,9,12))
# sequenceDataFrame     <- data.frame(c(1,3,5), c(3,9,12), sequenceVector, stringsAsFactors = FALSE)

## Methylate CpG dinucleoties in simulated reads with a given probability 
ConvertCG <- function(simulatedReads, methylationProbability) {

   flip <- rbinom(nrow(simulatedReads[grep("CG",simulatedReads$sequenceVector),]),1,1-methylationProbability)
   simulatedReads[,"flip"] <- 0
   simulatedReads[grep("CG",simulatedReads$sequenceVector),"flip"] <- flip
   simulatedReads[,"methylatedSeq"] <- simulatedReads$sequenceVector
   simulatedReads[simulatedReads$flip==1,]$methylatedSeq <- gsub("CG","TG",simulatedReads[simulatedReads$flip==1,]$sequenceVector)
   return(simulatedReads) # after bisulfite conversion of some CpGs to TpGs 
}

# simulatedReads[grep("CG",simulatedReads$sequenceVector),]
# sum(rbinom(30,1,0.4))
# gsub("CG","TG","CCCGAGAG")


## Re-align simulated reads back to the reference genome and calculate methylation ratios for CpGs 
AlignToReference <- function(meSimReads=methylatedSimulatedReads, refSequence, readLength, numberOfMismatches=1) {

   allKmers <- substring(refSequence, 1:(nchar(refSequence) - readLength + 1), readLength:nchar(refSequence))
   refCGpositions <- grep("CG",substring(refSequence,1:(nchar(refSequence)-1),2:nchar(refSequence)))
   coverageVector=replicate( 2, numeric(nchar(refSequence)))
   for(i in 1:nrow(methylatedSimulatedReads)) {
     if (length(which(adist(allKmers, methylatedSimulatedReads$methylatedSeq[i])==0,))==1){
        for (j in meSimReads$startPositions[i]:meSimReads$endPositions[i]){
           coverageVector[j,1]=coverageVector[j,1]+1
        }
     }
     else if (length(which(adist(allKmers, methylatedSimulatedReads$methylatedSeq[i])<=numberOfMismatches,))==1){
        for (j in meSimReads$startPositions[i]:meSimReads$endPositions[i]){
           coverageVector[j,2]=coverageVector[j,2]+1
        }
     }
    }
    CpGs=coverageVector[refCGpositions,1]
    TpGs=coverageVector[refCGpositions,2]
    methylationSummary=cbind(refCGpositions, CpGs, TpGs, methylatedFraction=CpGs/(CpGs+TpGs))
    print (methylationSummary)
}

# refSequence = "CGGGATGAAGGCCCCCGA"
# allKmers = substring(refSequence, 1:(nchar(refSequence) - 5 + 1), 5:nchar(refSequence))
# refCGpositions = grep("CG",substring(refSequence,1:(nchar(refSequence)-1),2:nchar(refSequence)))
# which(adist(allKmers, "ATGAA")==0)
# which(adist(allKmers, "CCCTG")==1)


refSeq="CGGGATGAAGGCCCCCGAGCTCCCCGAGAGCAGCGTCAGGGCACGGATGC"
readLen=8 # number of base pairs for simulated reads
noOfSimReads=500 # number of reads to simulate 
methylationProbability=0.8 # on average 1 minus this fraction of reads will have C to T conversion

simulatedReads  <- GenerateSimulatedReads(refSequence=refSeq, readLength=readLen, numberOfReadsToSimulate=noOfSimReads)
methylatedSimulatedReads <- ConvertCG(simulatedReads, methylationProbability)
methylationSummary  <- AlignToReference(methylatedSimulatedReads, refSeq, readLen)
```

    ##      refCGpositions CpGs TpGs methylatedFraction
    ## [1,]              1    9    1          0.9000000
    ## [2,]             16   85   13          0.8673469
    ## [3,]             25   74    9          0.8915663
    ## [4,]             34   84   23          0.7850467
    ## [5,]             44   68   12          0.8500000

refSeq=“CGGGATGAAGGCCCCCGAGCTCCCCGAGAGCAGCGTCAGGGCACGGATGC” readLen=8 \#
number of base pairs for simulated reads noOfSimReads=500 \# number of
reads to simulate methylationProbability=0.6 \# on average 1 minus this
fraction of reads will have C to T conversion

Takes sequence and chops it into sequences to extract 8 base pairs
simulatedReads \<- GenerateSimulatedReads(refSequence=refSeq,
readLength=readLen, numberOfReadsToSimulate=noOfSimReads)

Looks for methylation and then goes over each read to decide whether or
not to change C to T methylatedSimulatedReads \<-
ConvertCG(simulatedReads, methylationProbability)

Align to reference genome and quantify flipped Cs methylationSummary \<-
AlignToReference(methylatedSimulatedReads, refSeq, readLen)

`sample(x = c(10), size = 5, replace = T)` generates random sequence so
results will be different every time Replace = FALSE means that you
can’t have the same number twice “`{r} sample(x = c(10), size = 5,
replace = T) sample(x = c(43), size = 5, replace = T) sample(x = c(10),
size = 5, replace = FALSE) sample(x = c(10), size = 15, replace =
FALSE)`”

`substring(as.character("ATCGCGCATATACCGATCGT"), c(1,3,5), c(3,9,12))`
Takes random indeces and gives 8? base pairs

`grep` finds all bases defined ex. “CG” finds all CGs and tells you how
many there are (not where) `rbinom` gives binomial distribution, an
experiment that has 2 possible outcomes: success of failure where you
can have an unequal probability

``` r
rbinom(30, 1, 0.4)
```

    ##  [1] 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1 0 0 0 0 1 1 0 1 0 0 0 1 1 0

``` r
sum(rbinom(30,1,0.4))
```

    ## [1] 11

tries experiment 30 times, success = 1, 0.4 probability of success is
0.4

`gsub` finds first argument within third argument and replaces first
with second aargument

``` r
gsub("CG", "TG", "CCCGAGAG") #bisulfite conversion
```

    ## [1] "CCTGAGAG"

refSequence = “CGGGATGAAGGCCCCCGA” allKmers = substring(refSequence,
1:(nchar(refSequence) - 5 + 1), 5:nchar(refSequence)) refCGpositions =
grep(“CG”,substring(refSequence,1:(nchar(refSequence)-1),2:nchar(refSequence)))
which(adist(allKmers, “ATGAA”)==0) which(adist(allKmers, “CCCTG”)==1)
