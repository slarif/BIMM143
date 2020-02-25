Untitled
================

\#\#Lecture Notes When curve dips down rapidly there is new technology

\#\#Hands-on Session \#\#\#OMIM Search “asthma Verlaan” and click first
result and scroll down until you find “Verlaan et. al” then click the
SNPs and click where it says it overlaps with `4 transcripts` Ensemble
oage gives info on SNP and the genes in its vicinity (genomic context)
and tells you populations in various regions

\#\#\#Galaxy FASTQC runs all quality scores in box plots Tophat reads
over the introns in data for RNA-seq

\#\#\#Analyze RNA-seq by genotype results

Here we read the results of RNA-Seq for about \~230 samples (people)
with different genotypes for the rs8067378 Asthma associated SNP.

``` r
expr <- read.table("rs8067378_ENSG00000172057.6.txt")
head(expr)
```

    ##    sample geno      exp
    ## 1 HG00367  A/G 28.96038
    ## 2 NA20768  A/G 20.24449
    ## 3 HG00361  A/A 31.32628
    ## 4 HG00135  A/A 34.11169
    ## 5 NA18870  G/G 18.25141
    ## 6 NA11993  A/A 32.89721

``` r
#Calculate average expression value of people with A/A by extracting the A/A rows
inds <- expr$geno=="A/A"
expr[inds,]
```

    ##      sample geno      exp
    ## 3   HG00361  A/A 31.32628
    ## 4   HG00135  A/A 34.11169
    ## 6   NA11993  A/A 32.89721
    ## 8   NA18498  A/A 47.64556
    ## 13  NA20585  A/A 30.71355
    ## 15  HG00235  A/A 25.44983
    ## 16  NA20798  A/A 34.24915
    ## 18  NA19116  A/A 35.15014
    ## 24  NA19247  A/A 24.54684
    ## 27  NA19207  A/A 49.39612
    ## 30  HG00335  A/A 28.20755
    ## 37  NA20759  A/A 28.56199
    ## 48  HG00105  A/A 51.51787
    ## 53  NA11894  A/A 38.10956
    ## 55  HG00132  A/A 31.13741
    ## 62  HG00377  A/A 39.12999
    ## 67  NA19172  A/A 32.44173
    ## 76  NA20544  A/A 34.03260
    ## 82  NA18868  A/A 36.27151
    ## 96  HG00174  A/A 26.10355
    ## 97  HG00324  A/A 19.48106
    ## 99  NA20520  A/A 38.77623
    ## 102 HG00111  A/A 40.82922
    ## 112 NA20810  A/A 46.50527
    ## 121 HG00236  A/A 33.07320
    ## 122 NA19146  A/A 25.47283
    ## 127 NA11881  A/A 29.50655
    ## 133 NA19138  A/A 27.48438
    ## 154 NA19222  A/A 35.69719
    ## 155 NA06989  A/A 32.42236
    ## 162 NA12749  A/A 28.91526
    ## 167 HG00371  A/A 19.14544
    ## 169 NA12004  A/A 22.85572
    ## 174 HG00102  A/A 31.17067
    ## 177 NA20521  A/A 27.87464
    ## 179 NA20509  A/A 27.91580
    ## 180 HG00329  A/A 16.86780
    ## 182 HG00359  A/A 23.66127
    ## 187 NA11892  A/A 28.03403
    ## 188 NA20804  A/A 36.51922
    ## 198 HG00306  A/A 27.43637
    ## 202 NA07037  A/A 35.63983
    ## 205 NA19129  A/A 38.85161
    ## 214 HG00323  A/A 22.44576
    ## 215 NA18916  A/A 37.06379
    ## 217 HG00100  A/A 35.67637
    ## 226 NA20790  A/A 50.16704
    ## 227 NA20512  A/A 37.94544
    ## 228 HG00268  A/A 29.15536
    ## 229 HG00380  A/A 28.85309
    ## 230 NA12761  A/A 38.57101
    ## 235 HG00096  A/A 30.89365
    ## 240 NA07048  A/A 39.31537
    ## 242 HG00376  A/A 31.43743
    ## 243 NA19092  A/A 35.26739
    ## 245 HG00158  A/A 22.37043
    ## 246 HG00269  A/A 28.46943
    ## 248 HG00258  A/A 30.15636
    ## 257 NA12347  A/A 35.88457
    ## 263 NA11995  A/A 32.59723
    ## 264 NA19209  A/A 36.02549
    ## 265 NA20540  A/A 23.86454
    ## 266 NA12890  A/A 28.38114
    ## 269 HG00138  A/A 25.14243
    ## 270 NA19200  A/A 51.30170
    ## 275 NA12383  A/A 28.14811
    ## 278 NA06984  A/A 29.18390
    ## 290 NA07357  A/A 27.09760
    ## 298 NA11930  A/A 33.89656
    ## 304 NA19131  A/A 33.48253
    ## 305 NA18499  A/A 15.43178
    ## 306 HG00117  A/A 29.45277
    ## 322 HG00160  A/A 26.80283
    ## 332 NA12340  A/A 43.51943
    ## 337 HG00146  A/A 45.80808
    ## 341 HG00362  A/A 26.55972
    ## 343 NA18917  A/A 24.87330
    ## 346 NA20527  A/A 29.99549
    ## 348 NA20805  A/A 26.68589
    ## 354 NA12156  A/A 39.37193
    ## 359 HG00157  A/A 38.39523
    ## 360 HG00262  A/A 41.23635
    ## 368 HG00101  A/A 27.13936
    ## 370 NA20760  A/A 36.55643
    ## 371 HG00176  A/A 28.34688
    ## 374 NA20514  A/A 15.42908
    ## 382 NA20785  A/A 47.50579
    ## 384 HG00253  A/A 30.15754
    ## 386 HG00339  A/A 34.88439
    ## 389 NA18861  A/A 29.29955
    ## 390 NA20539  A/A 32.87767
    ## 400 HG00145  A/A 43.43665
    ## 401 NA19225  A/A 26.56050
    ## 405 HG00379  A/A 21.87746
    ## 408 NA18907  A/A 33.42582
    ## 409 NA19204  A/A 25.38406
    ## 412 NA20770  A/A 18.20442
    ## 420 HG00104  A/A 21.62336
    ## 424 NA20786  A/A 35.80093
    ## 426 NA20756  A/A 32.26844
    ## 433 NA12762  A/A 34.40756
    ## 442 HG00159  A/A 23.99631
    ## 443 NA20811  A/A 11.39643
    ## 448 HG01791  A/A 35.24632
    ## 451 HG00182  A/A 23.38376
    ## 456 NA12750  A/A 34.94395
    ## 459 HG00108  A/A 31.92036
    ## 461 NA19130  A/A 44.27738

``` r
mean(expr[inds,"exp"])
```

    ## [1] 31.81864

``` r
summary(expr[inds,]$exp)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.40   27.02   31.25   31.82   35.92   51.52

``` r
#Do same thing for G/G
indg <- expr$geno=="G/G"
summary(expr[indg,]$exp)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   6.675  16.903  20.074  20.594  24.457  33.956

``` r
boxplot(exp ~ geno, data = expr, notch=TRUE)
```

![](Class15_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#if notches don't overlapp then there's statistic significance 
```
