Analysis Documentation: The Accuracy Of Proxy Means Tests For Immigrant Populations
================

This document contains the analysis and documentation for my paper "The Accuracy Of Proxy Means Tests For Immigrant Populations: A Case Study In Colombia," recently published in [Princeton University's Journal of Public and International Affairs](https://jpia.princeton.edu/news/accuracy-proxy-means-tests-immigrant-populations-case-study-colombia).

------------------------------------------------------------------------

------------------------------------------------------------------------

Data Preparation
----------------

### Data Sourcing

The data for this analysis is compiled from three datasets, all sourced from the *Departamento Administrativo Nacional de Estadística* (DANE), the Colombian national statistics office:

-   GEIH Viviendas y Hogares: individual data on household assets (2018 data available [here](http://microdatos.dane.gov.co/index.php/catalog/547/get_microdata))

-   Medición de Pobreza Monetaria y Desigualidad: household data on income and individual data on education (2018 data available [here](http://microdatos.dane.gov.co/index.php/catalog/608/get_microdata))

-   GEIH Módulo de Migración: individual data on birthplace and place of residence 5 years and 1 year ago (2018 data available [here](http://microdatos.dane.gov.co/index.php/catalog/640/get_microdata))

For the sake of consistency and replicability, the variables were not renamed. The challenge of translating Spanish documentation to results for a paper in English was not compounded by attempting to deviate from the alphanumeric DANE variable codes. The DANE documentation for each of these datasets is also available at the links above.

### Data Cleaning

``` r
load("PMT_2018.Rdata")
```

A unique identifier `UNIQUEID.HH` was created for each household head by concatenating the `Directorio` and `Secuencia_P` variables. Since *Viviendas y Hogares* and the *Módulo de Migración* were collected at the individual level, and *Medición de Pobreza Monetaria* was at the household level, the analysis aggregated to the household level based on the individual data from self-identified household heads. This repository includes the compiled version of these three datasets for 2018, as `PMT_2018.Rdata`.

### Variable Recoding

DANE surveys code dichotomous responses 1 for 'Yes' and 2 for 'No.' This can make them more difficult to interpret in a regression, so I recoded them to a 0-1 scale. The variable for health insurance status (`P6090`), also required an option for "Not sure," but the others were all true dichotomous variables.

``` r
PMT_2018$P5030 <- ifelse(PMT_2018$P5030 == 2, 0, PMT_2018$P5030)
PMT_2018$P4040 <- ifelse(PMT_2018$P4040 == 2, 0, PMT_2018$P4040)
PMT_2018$P4030S1 <- ifelse(PMT_2018$P4030S1 == 2, 0, PMT_2018$P4030S1)
PMT_2018$P5210S1 <- ifelse(PMT_2018$P5210S1 == 2, 0, PMT_2018$P5210S1)
PMT_2018$P5210S5 <- ifelse(PMT_2018$P5210S5 == 2, 0, PMT_2018$P5210S5)
PMT_2018$P5210S4 <- ifelse(PMT_2018$P5210S4 == 2, 0, PMT_2018$P5210S4)
PMT_2018$P5210S11 <- ifelse(PMT_2018$P5210S11 == 2, 0, PMT_2018$P5210S11)
PMT_2018$P5210S2 <- ifelse(PMT_2018$P5210S2 == 2, 0, PMT_2018$P5210S2)
PMT_2018$P5210S10 <- ifelse(PMT_2018$P5210S10 == 2, 0, PMT_2018$P5210S10)
PMT_2018$P5210S9 <- ifelse(PMT_2018$P5210S9 == 2, 0, PMT_2018$P5210S9)
PMT_2018$P5210S18 <- ifelse(PMT_2018$P5210S18 == 2, 0, PMT_2018$P5210S18)
PMT_2018$P5210S16 <- ifelse(PMT_2018$P5210S16 == 2, 0, PMT_2018$P5210S16)
PMT_2018$P5210S15 <- ifelse(PMT_2018$P5210S15 == 2, 0, PMT_2018$P5210S15)
PMT_2018$P5210S21 <- ifelse(PMT_2018$P5210S21 == 2, 0, PMT_2018$P5210S21)
PMT_2018$P5210S22 <- ifelse(PMT_2018$P5210S22 == 2, 0, PMT_2018$P5210S22)
PMT_2018$P5210S3 <- ifelse(PMT_2018$P5210S3 == 2, 0, PMT_2018$P5210S3)
# Health Insurance Status
PMT_2018$P6090 <- ifelse(PMT_2018$P6090 == 2, 0, PMT_2018$P6090)
PMT_2018$P6090 <- ifelse(PMT_2018$P6090 == 9, NA, PMT_2018$P6090)
```

### Migration Variable Creation

Using dplyr, a `Nationality` variable was created based on self-reported birthplace (`P756`). Colombia doesn't have birthright citizenship, but due to limitations in the data birthplace was used as a proxy for national identity.

``` r
PMT_2018$Nationality <- case_when(
  ((PMT_2018$P6074==1 | PMT_2018$P756!=3)) ~ "Colombian",  
  (PMT_2018$P756S3==3) ~ "Venezuelan", 
  (PMT_2018$P756S3!=3 & PMT_2018$P756==3) ~ "Other",
  TRUE ~ "Undefined"
)
```

This the nationality variable was then combined with information about where the respondent was located 5 years ago and 1 year ago (`P755`, and `P753`, respectively) in ordert to create the `Migration_Status` variable.

``` r
PMT_2018$Migration_Status <- case_when(
  (PMT_2018$Nationality=="Venezuelan" & PMT_2018$P755S3==3 & PMT_2018$P753S3==3) ~ "Venezuelan Recent Migrant",
  (PMT_2018$Nationality=="Venezuelan" & PMT_2018$P755S3==3 & PMT_2018$P753!=4) ~ "Venezuelan Established Migrant",
  (PMT_2018$Nationality=="Venezuelan" & (PMT_2018$P755!=4 | PMT_2018$P753!=4)) ~ "Venezuelan Resident",
  (PMT_2018$Nationality=="Other" & (PMT_2018$P755S3!=3 | PMT_2018$P753S3!=3)) ~ "Other Migrant",
  (PMT_2018$Nationality=="Other" & (PMT_2018$P755!=4 | PMT_2018$P753!=4)) ~ "Other Resident",
  (PMT_2018$Nationality=="Colombian" & (PMT_2018$P755S3==3 | PMT_2018$P753S3==3)) ~ "Colombian Venezuelan Returnee",
  (PMT_2018$Nationality=="Colombian" & (PMT_2018$P755S3!=3 | PMT_2018$P753S3!=3)) ~ "Colombian Other Returnee",
  (PMT_2018$Nationality=="Colombian") ~ "Colombian", 
  TRUE ~ "Undefined"
)
```

### Identifying Complete cases

The last step in the data cleaning is identifying the cases that are complete for all the variables in the PMT model. This will prevent us from generating prediction errors when using the model to predict income. This should happen prior to the data partition.

``` r
# Predicting the LM to identify complete cases
PMT_2018_setup <- lm(INGTOTUGARR ~ factor(DPTO) + factor(P5090) + factor(P5080) + factor(P5070) + 
                       factor(P5020) + factor(P5050) + factor(P4010) + factor(P4020) + factor(P6210) +
                       P5000 + P5010 + P5030 + P4040 + P4030S1 + P5210S1 + P5210S5 + P5210S4 + P5210S11 + 
                       P5210S2 + P5210S10 + P5210S9 + P5210S18 + P5210S16 + P5210S15 + P5210S21 + P5210S22 + 
                       P6008 + P5210S3 + P6090, 
                     weights = FEX_C,
                     data = PMT_2018)
PMT_2018$Complete_Case <- 1
PMT_2018$Complete_Case[na.action(PMT_2018_setup)] <- 0
```

------------------------------------------------------------------------

------------------------------------------------------------------------

Data Partition
--------------

In order to mirror the process of developing and implementing a PMT, I partitioned the data into a training set that was used to create the PMT model estimates, and a testing dataset to evaluate the model's accuracy. With 231128 total cases, 25% was deemed to be a sufficient partition size for the training dataset, while also leaving ample data for testing.

``` r
set.seed(768)
PMT_2018_Partition <- sample(seq_len(nrow(PMT_2018)), size = nrow(PMT_2018)*.25)
PMT_2018_Training <- PMT_2018[PMT_2018_Partition, ]
PMT_2018_Test <- PMT_2018[-PMT_2018_Partition, ]
```

------------------------------------------------------------------------

------------------------------------------------------------------------

PMT Model Development
---------------------

This section describes the generation of the core PMT model

### Dependent Variable Transformation

Before generating the linear model, the dependent variable needed to be transformed. The original annual income variable (INGTOTUGARR) was changed to be in thousands for ease of interpretation (1 COP is approximately 4 thousand USD).

``` r
 PMT_2018_Training$INGTOTUGARR_TH <- PMT_2018_Training$INGTOTUGARR/1000
ggplot(data = PMT_2018_Training, aes((INGTOTUGARR_TH))) + geom_histogram()
```

![](Colombia-PMT-Analysis_files/figure-markdown_github/unnamed-chunk-8-1.png)

The graph appears highly skewed, so I used the natural log of the income variable in order to better approximate a normal distribution and improve model fit.

``` r
ggplot(data = PMT_2018_Training, aes(log1p(INGTOTUGARR_TH))) + geom_histogram()
```

![](Colombia-PMT-Analysis_files/figure-markdown_github/unnamed-chunk-9-1.png) There are some outliers with zero income, which were adressed in a later robustness test. For now, I created the variable `logINGTOTUGARR_TH` for the logged value of income in thousands, which was used to estimate the PMT model.

``` r
 PMT_2018_Training$logINGTOTUGARR_TH <- log1p(PMT_2018_Training$INGTOTUGARR_TH)
```

### Estimating the Model

The model was estimated using a series of variables based on Colombia's SISBEN PMT for subsidized health insurance. The current SISBEN variable list is proprietary to avoid manipulation, but past iterations have covered the following series of variables:

-   Factor variables included Department (`DPTO`), home ownership status (`P5090`), primary cooking fuel (`P5080`), room where food is prepared (`P5070`), type of sewage system (`P5020`), water source (`P5050`), exterior wall material (`P4010`), and floor material (`P4020`).
-   Integer housing variables included number of rooms (`P5000`) and number of bedrooms (`P5010`).
-   Binary Household asset variables included a private bathroom (`P5030`) 24-hour running water (`P4040`), electricity (`P4030S1`), phone line (`P5210S1`), refrigerator (`P5210S5`), washing machine (`P5210S4`), color television (`P5210S11`), cable (`P5210S2`), water heater (`P5210S10`), microwave (`P5210S9`), air conditioning (`P5210S18`), computer (`P5210S16`), stereo system (`P5210S15`), motorcycle (`P5210S21`), car (`P5210S22`), and internet (`P5210S3`).
-   Household head characteristics included education level (`P6210`), health insurance status (`P6090`), and household size (`P6008`).
-   `FEX_C` was used as a weighting variable.

``` r
PMT_2018_Model1 <- lm(logINGTOTUGARR_TH ~ factor(DPTO) + factor(P5090) + factor(P5080) + factor(P5070) + 
                   factor(P5020) + factor(P5050) + factor(P4010) + factor(P4020) + factor(P6210) +
                   P5000 + P5010 + P5030 + P4040 + P4030S1 + P5210S1 + P5210S5 + P5210S4 + P5210S11 + 
                   P5210S2 + P5210S10 + P5210S9 + P5210S18 + P5210S16 + P5210S15 + P5210S21 + P5210S22 + 
                   P6008 + P5210S3 + P6090, 
                weights = FEX_C,
                data = PMT_2018_Training)
```

This model generated the following regression output:

<pre style = "max-height:400px; float: left; width: 910px; overflow-y: auto;">## 
## Call:
## lm(formula = logINGTOTUGARR_TH ~ factor(DPTO) + factor(P5090) + 
##     factor(P5080) + factor(P5070) + factor(P5020) + factor(P5050) + 
##     factor(P4010) + factor(P4020) + factor(P6210) + P5000 + P5010 + 
##     P5030 + P4040 + P4030S1 + P5210S1 + P5210S5 + P5210S4 + P5210S11 + 
##     P5210S2 + P5210S10 + P5210S9 + P5210S18 + P5210S16 + P5210S15 + 
##     P5210S21 + P5210S22 + P6008 + P5210S3 + P6090, data = PMT_2018_Training, 
##     weights = FEX_C)
## 
## Weighted Residuals:
##      Min       1Q   Median       3Q      Max 
## -176.350   -1.505    0.418    2.332   42.709 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      5.508384   0.110887  49.676  < 2e-16 ***
## factor(DPTO)08   0.056166   0.020660   2.719 0.006560 ** 
## factor(DPTO)11   0.075261   0.013309   5.655 1.57e-08 ***
## factor(DPTO)13  -0.008145   0.022881  -0.356 0.721853    
## factor(DPTO)15  -0.094935   0.022769  -4.170 3.06e-05 ***
## factor(DPTO)17  -0.100125   0.025792  -3.882 0.000104 ***
## factor(DPTO)18  -0.101524   0.037528  -2.705 0.006826 ** 
## factor(DPTO)19  -0.247508   0.024145 -10.251  < 2e-16 ***
## factor(DPTO)20  -0.237480   0.028255  -8.405  < 2e-16 ***
## factor(DPTO)23  -0.100368   0.028464  -3.526 0.000422 ***
## factor(DPTO)25   0.127627   0.019568   6.522 6.98e-11 ***
## factor(DPTO)27  -0.384385   0.048194  -7.976 1.55e-15 ***
## factor(DPTO)41  -0.203188   0.026833  -7.572 3.73e-14 ***
## factor(DPTO)44  -0.080970   0.039694  -2.040 0.041368 *  
## factor(DPTO)47  -0.094691   0.031108  -3.044 0.002336 ** 
## factor(DPTO)50  -0.021810   0.028925  -0.754 0.450839    
## factor(DPTO)52  -0.165516   0.023032  -7.186 6.75e-13 ***
## factor(DPTO)54  -0.115743   0.025843  -4.479 7.52e-06 ***
## factor(DPTO)63  -0.083494   0.026199  -3.187 0.001439 ** 
## factor(DPTO)66  -0.045730   0.023678  -1.931 0.053449 .  
## factor(DPTO)68  -0.024704   0.019564  -1.263 0.206687    
## factor(DPTO)70  -0.011106   0.033826  -0.328 0.742662    
## factor(DPTO)73  -0.063913   0.023923  -2.672 0.007553 ** 
## factor(DPTO)76  -0.071877   0.015437  -4.656 3.23e-06 ***
## factor(P5090)2  -0.018350   0.020275  -0.905 0.365434    
## factor(P5090)3  -0.255024   0.009120 -27.962  < 2e-16 ***
## factor(P5090)4  -0.115674   0.011281 -10.254  < 2e-16 ***
## factor(P5090)5  -0.299442   0.025140 -11.911  < 2e-16 ***
## factor(P5090)6  -0.559037   0.107822  -5.185 2.17e-07 ***
## factor(P5080)2   0.132791   0.193320   0.687 0.492148    
## factor(P5080)3  -0.110228   0.027704  -3.979 6.94e-05 ***
## factor(P5080)4  -0.121624   0.028659  -4.244 2.20e-05 ***
## factor(P5080)5  -0.345584   0.034421 -10.040  < 2e-16 ***
## factor(P5080)6   0.153649   0.145787   1.054 0.291923    
## factor(P5080)7  -0.527520   0.387803  -1.360 0.173748    
## factor(P5070)2   0.010597   0.032611   0.325 0.745210    
## factor(P5070)3   0.010444   0.018444   0.566 0.571242    
## factor(P5070)4  -0.020682   0.040955  -0.505 0.613570    
## factor(P5070)5  -0.002173   0.036150  -0.060 0.952078    
## factor(P5020)2  -0.071780   0.014474  -4.959 7.10e-07 ***
## factor(P5020)3  -0.207867   0.027721  -7.499 6.55e-14 ***
## factor(P5020)4  -0.214320   0.075777  -2.828 0.004682 ** 
## factor(P5020)5   0.104306   0.066746   1.563 0.118125    
## factor(P5050)2  -0.197634   0.069539  -2.842 0.004484 ** 
## factor(P5050)3  -0.109676   0.112413  -0.976 0.329241    
## factor(P5050)4  -0.281880   0.118765  -2.373 0.017628 *  
## factor(P5050)5  -0.118815   0.074596  -1.593 0.111217    
## factor(P5050)6   0.244812   0.109943   2.227 0.025971 *  
## factor(P5050)7   0.123697   0.146526   0.844 0.398562    
## factor(P5050)8  -0.033689   0.126284  -0.267 0.789647    
## factor(P5050)9   0.151553   0.075669   2.003 0.045198 *  
## factor(P5050)10  0.118241   0.028150   4.200 2.67e-05 ***
## factor(P4010)2  -0.454404   0.074830  -6.072 1.27e-09 ***
## factor(P4010)3  -0.081789   0.029840  -2.741 0.006129 ** 
## factor(P4010)4  -0.074340   0.027243  -2.729 0.006359 ** 
## factor(P4010)5  -0.104393   0.029980  -3.482 0.000498 ***
## factor(P4010)6   0.596607   0.161892   3.685 0.000229 ***
## factor(P4010)7  -0.055714   0.081505  -0.684 0.494250    
## factor(P4010)8   0.155551   0.099279   1.567 0.117167    
## factor(P4010)9   0.719814   0.348212   2.067 0.038723 *  
## factor(P4020)2   0.025322   0.029524   0.858 0.391074    
## factor(P4020)3   0.106993   0.038336   2.791 0.005258 ** 
## factor(P4020)4   0.194504   0.030623   6.352 2.15e-10 ***
## factor(P4020)5   0.575970   0.068130   8.454  < 2e-16 ***
## factor(P4020)6   0.627016   0.043649  14.365  < 2e-16 ***
## factor(P4020)7   0.201703   0.094567   2.133 0.032936 *  
## factor(P6210)2   0.298443   0.371440   0.803 0.421703    
## factor(P6210)3   0.097160   0.018421   5.274 1.34e-07 ***
## factor(P6210)4   0.201903   0.020026  10.082  < 2e-16 ***
## factor(P6210)5   0.236334   0.019146  12.344  < 2e-16 ***
## factor(P6210)6   0.520129   0.020046  25.946  < 2e-16 ***
## factor(P6210)9   0.047818   0.249288   0.192 0.847887    
## P5000            0.080825   0.004371  18.493  < 2e-16 ***
## P5010            0.036523   0.006759   5.404 6.56e-08 ***
## P5030            0.062979   0.016710   3.769 0.000164 ***
## P4040            0.040482   0.011717   3.455 0.000551 ***
## P4030S1          0.117137   0.100308   1.168 0.242903    
## P5210S1          0.069932   0.010057   6.954 3.60e-12 ***
## P5210S5          0.060705   0.012813   4.738 2.17e-06 ***
## P5210S4          0.096805   0.009519  10.170  < 2e-16 ***
## P5210S11         0.160570   0.017522   9.164  < 2e-16 ***
## P5210S2          0.092387   0.009274   9.962  < 2e-16 ***
## P5210S10         0.080742   0.010755   7.508 6.12e-14 ***
## P5210S9          0.122654   0.010793  11.364  < 2e-16 ***
## P5210S18         0.163141   0.019603   8.322  < 2e-16 ***
## P5210S16         0.067465   0.010098   6.681 2.39e-11 ***
## P5210S15         0.064166   0.007801   8.225  < 2e-16 ***
## P5210S21         0.118209   0.008581  13.776  < 2e-16 ***
## P5210S22         0.380880   0.011457  33.245  < 2e-16 ***
## P6008            0.106014   0.002954  35.892  < 2e-16 ***
## P5210S3          0.147811   0.010506  14.069  < 2e-16 ***
## P6090            0.229041   0.015385  14.887  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.237 on 51099 degrees of freedom
##   (6591 observations deleted due to missingness)
## Multiple R-squared:  0.4231, Adjusted R-squared:  0.4221 
## F-statistic: 411.8 on 91 and 51099 DF,  p-value: < 2.2e-16
</pre>
The adjusted R<sup>2</sup> value of 0.4220639 is within the range of adjusted R<sup>2</sup> values in the PMT literature, and the overall fit appears to be good enough to justify use of this model for error analysis.

### Analyzing Model Accuracy

In order to test the accuracy of the model, I calculated rates of both Inclusion and Exclusion Errors amongst different migration statuses.

First, I used the linear model based on the training dataset to predict income (`INGTOT_PRED_1`) for all complete cases in the testing dataset.

``` r
PMT_2018_Test$logINGTOTUGARRTH_PRED_1 <- NA
PMT_2018_Test$logINGTOTUGARRTH_PRED_1[PMT_2018_Test$Complete_Case == 1] <- predict.lm(PMT_2018_Model1, PMT_2018_Test, na.action = na.exclude)
PMT_2018_Test$INGTOT_PRED_1 <- expm1(PMT_2018_Test$logINGTOTUGARRTH_PRED_1)*1000
```

I then calculated whether these predicted incomes were above or below the poverty line (`LP`, or "Linea Pobreza") as demarcated in the GEIH data.

``` r
PMT_2018_Test$POBRE_PREDICTED_1 <- case_when(PMT_2018_Test$INGTOT_PRED_1>PMT_2018_Test$LP ~ 0,
                                           TRUE ~ 1)
```

Finally, I compared whether the predicted categorization of poverty aligned with the actual poverty categorization variable (`POBRE`) in the Medición de Pobreza data. If the household was predicted to be above the poverty line, but in reality they were below it, they were classified as an 'Exclusion Error.' 'Inclusion Error' was the converse. Otherwise they were classifed as 'Correct.'

``` r
PMT_2018_Test$Errors_1 <- case_when(PMT_2018_Test$POBRE==0 & PMT_2018_Test$POBRE_PREDICTED_1==1  ~ "Inclusion Error",
                                  PMT_2018_Test$POBRE==1 & PMT_2018_Test$POBRE_PREDICTED_1==0  ~ "Exclusion Error",
                                           TRUE ~ "Correct")
```

We can then compile these results by immigration status as a proportion:

``` r
Error_Table <- table(PMT_2018_Test$Migration_Status, PMT_2018_Test$Errors_1)
kable(prop.table(Error_Table, margin = 1))
```

|                                |    Correct|  Exclusion Error|  Inclusion Error|
|--------------------------------|----------:|----------------:|----------------:|
| Colombian                      |  0.7606134|        0.1628860|        0.0765007|
| Colombian Other Returnee       |  0.8508015|        0.0949445|        0.0542540|
| Colombian Venezuelan Returnee  |  0.5721612|        0.3560440|        0.0717949|
| Other Migrant                  |  0.8389831|        0.1101695|        0.0508475|
| Other Resident                 |  0.8792453|        0.0867925|        0.0339623|
| Undefined                      |  0.6428571|        0.2142857|        0.1428571|
| Venezuelan Established Migrant |  0.5908240|        0.3417603|        0.0674157|
| Venezuelan Recent Migrant      |  0.5734331|        0.3489242|        0.0776427|
| Venezuelan Resident            |  0.7202073|        0.2124352|        0.0673575|

We can then test for statistical significance for differences in error rates, focusing on exclusion errors for the statuses of interest:

-   Venezuelan Resident:

``` r
pander((prop.test(c(Error_Table["Colombian", "Exclusion Error"], Error_Table["Venezuelan Resident", "Exclusion Error"]), c( sum(Error_Table["Colombian", ]), sum(Error_Table["Venezuelan Resident", ])))))
```

<table>
<caption>2-sample test for equality of proportions with continuity correction: <code>c(Error_Table[&quot;Colombian&quot;, &quot;Exclusion Error&quot;], Error_Table[&quot;Venezuelan Resident&quot;,  out of c(sum(Error_Table[&quot;Colombian&quot;, ]), sum(Error_Table[&quot;Venezuelan Resident&quot;,     &quot;Exclusion Error&quot;]) out of     ]))</code></caption>
<colgroup>
<col width="22%" />
<col width="6%" />
<col width="13%" />
<col width="33%" />
<col width="12%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Test statistic</th>
<th align="center">df</th>
<th align="center">P value</th>
<th align="center">Alternative hypothesis</th>
<th align="center">prop 1</th>
<th align="center">prop 2</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">3.116</td>
<td align="center">1</td>
<td align="center">0.07751</td>
<td align="center">two.sided</td>
<td align="center">0.1629</td>
<td align="center">0.2124</td>
</tr>
</tbody>
</table>

-   Venezuelan Established Migrant

``` r
pander(prop.test(c(Error_Table["Colombian", "Exclusion Error"], Error_Table["Venezuelan Established Migrant", "Exclusion Error"]), c( sum(Error_Table["Colombian", ]), sum(Error_Table["Venezuelan Established Migrant", ]))))
```

<table>
<caption>2-sample test for equality of proportions with continuity correction: <code>c(Error_Table[&quot;Colombian&quot;, &quot;Exclusion Error&quot;], Error_Table[&quot;Venezuelan Established Migrant&quot;,  out of c(sum(Error_Table[&quot;Colombian&quot;, ]), sum(Error_Table[&quot;Venezuelan Established Migrant&quot;,     &quot;Exclusion Error&quot;]) out of     ]))</code></caption>
<colgroup>
<col width="20%" />
<col width="6%" />
<col width="21%" />
<col width="30%" />
<col width="10%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Test statistic</th>
<th align="center">df</th>
<th align="center">P value</th>
<th align="center">Alternative hypothesis</th>
<th align="center">prop 1</th>
<th align="center">prop 2</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">246.4</td>
<td align="center">1</td>
<td align="center">1.622e-55 * * *</td>
<td align="center">two.sided</td>
<td align="center">0.1629</td>
<td align="center">0.3418</td>
</tr>
</tbody>
</table>

-   Venezuelan Recent Migrant

``` r
pander(prop.test(c(Error_Table["Colombian", "Exclusion Error"], Error_Table["Venezuelan Recent Migrant", "Exclusion Error"]), c( sum(Error_Table["Colombian", ]), sum(Error_Table["Venezuelan Recent Migrant", ]))))
```

<table>
<caption>2-sample test for equality of proportions with continuity correction: <code>c(Error_Table[&quot;Colombian&quot;, &quot;Exclusion Error&quot;], Error_Table[&quot;Venezuelan Recent Migrant&quot;,  out of c(sum(Error_Table[&quot;Colombian&quot;, ]), sum(Error_Table[&quot;Venezuelan Recent Migrant&quot;,     &quot;Exclusion Error&quot;]) out of     ]))</code></caption>
<colgroup>
<col width="20%" />
<col width="6%" />
<col width="21%" />
<col width="30%" />
<col width="10%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Test statistic</th>
<th align="center">df</th>
<th align="center">P value</th>
<th align="center">Alternative hypothesis</th>
<th align="center">prop 1</th>
<th align="center">prop 2</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">266.7</td>
<td align="center">1</td>
<td align="center">5.881e-60 * * *</td>
<td align="center">two.sided</td>
<td align="center">0.1629</td>
<td align="center">0.3489</td>
</tr>
</tbody>
</table>

This clearly demonstrates higher levels of Exclusion Error amongst Venezuelan immigrants. For the paper, the above code was modified to also test differences in Correct and Inclusion Error classifications.

------------------------------------------------------------------------

------------------------------------------------------------------------

Robustness Checks
-----------------

In order to validate the results, I performed a number of robustness checks for various subsets of the data.

#### Near-Povery Error Rates

First, we restricted analysis to only cases with actual income within 200% of the poverty line, as Venezuelan immigrants are more likely to be near the poverty line in the first place.

``` r
PMT_2018_Test$Near_LP <- case_when(PMT_2018_Test$INGTOTUGARR<(PMT_2018_Test$LP*3) & PMT_2018_Test$INGTOTUGARR>(PMT_2018_Test$LP*0)  ~ 1,
                                 TRUE ~ 0)
PMT_2018_Test_NearLP <- PMT_2018_Test[PMT_2018_Test$Near_LP==1, ]
```

To contextualize this sub-group, I looked at their share of the total population and their mean incomes in thousands (Table 6 in the paper):

**Near-poverty share of population, by immigration status**

``` r
NP_Subpop_Share <- table(PMT_2018_Test_NearLP$Migration_Status)/table(PMT_2018_Test$Migration_Status)
```

| Var1                           |       Freq|
|:-------------------------------|----------:|
| Colombian                      |  0.1965353|
| Venezuelan Established Migrant |  0.3043071|
| Venezuelan Recent Migrant      |  0.3751169|
| Venezuelan Resident            |  0.2331606|

**Mean income in thousands, by immigration status:**

``` r
PMT_2018_Test_NearLP$INGTOTUGARR_TH <- PMT_2018_Test_NearLP$INGTOTUGARR/1000
NearLP_Income_Table <- aggregate(PMT_2018_Test_NearLP$INGTOTUGARR_TH, list(PMT_2018_Test_NearLP$Migration_Status), mean)
```

|     | Group.1                        |         x|
|-----|:-------------------------------|---------:|
| 1   | Colombian                      |  508.3909|
| 7   | Venezuelan Established Migrant |  555.5205|
| 8   | Venezuelan Recent Migrant      |  533.5653|
| 9   | Venezuelan Resident            |  575.1563|

**Statistical significance of differences in mean income, versus Colombians:**

-   Venezuelan Resident

``` r
pander(t.test(PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status=="Colombian"], 
       PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status=="Venezuelan Resident"], 
       alternative = "two.sided", var.equal = FALSE))
```

<table>
<caption>Welch Two Sample t-test: <code>PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status ==</code> and <code>PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status ==     &quot;Colombian&quot;]</code> and <code>&quot;Venezuelan Resident&quot;]</code> (continued below)</caption>
<colgroup>
<col width="23%" />
<col width="10%" />
<col width="16%" />
<col width="34%" />
<col width="15%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Test statistic</th>
<th align="center">df</th>
<th align="center">P value</th>
<th align="center">Alternative hypothesis</th>
<th align="center">mean of x</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">-2.333</td>
<td align="center">44.16</td>
<td align="center">0.02428 *</td>
<td align="center">two.sided</td>
<td align="center">508.4</td>
</tr>
</tbody>
</table>

<table style="width:15%;">
<colgroup>
<col width="15%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">mean of y</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">575.2</td>
</tr>
</tbody>
</table>

-   Venezuelan Established Migrant

``` r
pander(t.test(PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status=="Colombian"], 
       PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status=="Venezuelan Established Migrant"], alternative = "two.sided", var.equal = FALSE))
```

<table>
<caption>Welch Two Sample t-test: <code>PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status ==</code> and <code>PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status ==     &quot;Colombian&quot;]</code> and <code>&quot;Venezuelan Established Migrant&quot;]</code> (continued below)</caption>
<colgroup>
<col width="21%" />
<col width="10%" />
<col width="22%" />
<col width="31%" />
<col width="13%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Test statistic</th>
<th align="center">df</th>
<th align="center">P value</th>
<th align="center">Alternative hypothesis</th>
<th align="center">mean of x</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">-3.964</td>
<td align="center">330.8</td>
<td align="center">9.036e-05 * * *</td>
<td align="center">two.sided</td>
<td align="center">508.4</td>
</tr>
</tbody>
</table>

<table style="width:15%;">
<colgroup>
<col width="15%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">mean of y</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">555.5</td>
</tr>
</tbody>
</table>

-   Venezuelan Recent Migrant

``` r
pander(t.test(PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status=="Colombian"], 
       PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status=="Venezuelan Recent Migrant"], alternative = "two.sided", var.equal = FALSE))
```

<table style="width:100%;">
<caption>Welch Two Sample t-test: <code>PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status ==</code> and <code>PMT_2018_Test_NearLP$INGTOTUGARR_TH[PMT_2018_Test_NearLP$Migration_Status ==     &quot;Colombian&quot;]</code> and <code>&quot;Venezuelan Recent Migrant&quot;]</code></caption>
<colgroup>
<col width="20%" />
<col width="9%" />
<col width="11%" />
<col width="29%" />
<col width="14%" />
<col width="14%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Test statistic</th>
<th align="center">df</th>
<th align="center">P value</th>
<th align="center">Alternative hypothesis</th>
<th align="center">mean of x</th>
<th align="center">mean of y</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">-2.355</td>
<td align="center">410.5</td>
<td align="center">0.019 *</td>
<td align="center">two.sided</td>
<td align="center">508.4</td>
<td align="center">533.6</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

Finally, I compared exclusion error rates within this sub-population:

``` r
Error_Table_NearLP <- table(PMT_2018_Test_NearLP$Migration_Status, PMT_2018_Test_NearLP$Errors_1)
Error_Prop.Table_NearLP <- prop.table(Error_Table_NearLP, margin = 1)
kable(Error_Prop.Table_NearLP[c(1, 7:9), ])
```

|                                |    Correct|  Exclusion Error|  Inclusion Error|
|--------------------------------|----------:|----------------:|----------------:|
| Colombian                      |  0.4138952|        0.4975079|        0.0885969|
| Venezuelan Established Migrant |  0.3569231|        0.5661538|        0.0769231|
| Venezuelan Recent Migrant      |  0.3615960|        0.5536160|        0.0847880|
| Venezuelan Resident            |  0.3777778|        0.5111111|        0.1111111|

Thus we see that even within this restricted cohort, immigrant subpopulations have higher rates of exclusion errors.

------------------------------------------------------------------------

#### Work-Status Error Rates

The next robustness check focused on work status (`P6240`), to account for the impact of differential rates of employment between the migrant subpopulations, as shown in the table below (Table 8 in the paper).

``` r
unemp_table <- 100*prop.table(table(PMT_2018_Test$Migration_Status, PMT_2018_Test$P6240), margin = 1)
unemp_table <- unemp_table[c(1, 7:9), c(1:2)]
```

The table displays the percentage rate of workers (`1`) and those looking for work (`2`) by immigrant subpopulation (all other occupations such as student and retired were classified as 'Other'):

|                                |         1|         2|
|--------------------------------|---------:|---------:|
| Colombian                      |  62.75788|  2.345628|
| Venezuelan Established Migrant |  80.80524|  3.277154|
| Venezuelan Recent Migrant      |  80.91674|  5.425631|
| Venezuelan Resident            |  76.68394|  4.663212|

The robustness test then checked the exclusion error rates for three different subsets of the data:

**All self-reported workers**

``` r
PMT_2018_Test_Workers <- PMT_2018_Test[PMT_2018_Test$P6240==1, ]
Error_Prop.Table_Workers <- prop.table(table(PMT_2018_Test_Workers$Migration_Status, PMT_2018_Test_Workers$Errors_1), margin = 1)
```

|                                |    Correct|  Exclusion Error|  Inclusion Error|
|--------------------------------|----------:|----------------:|----------------:|
| Colombian                      |  0.7694163|        0.1363920|        0.0941917|
| Venezuelan Established Migrant |  0.6222480|        0.3093859|        0.0683662|
| Venezuelan Recent Migrant      |  0.6069364|        0.3075145|        0.0855491|
| Venezuelan Resident            |  0.7297297|        0.1891892|        0.0810811|

**Workers near the poverty line**

``` r
PMT_2018_Test_NearLP_Workers <- PMT_2018_Test[PMT_2018_Test$Near_LP==1 & PMT_2018_Test$P6240==1, ]
Error_Prop.Table_NPWorkers <- prop.table(table(PMT_2018_Test_NearLP_Workers$Migration_Status, PMT_2018_Test_NearLP_Workers$Errors_1), margin = 1)
```

|                                |    Correct|  Exclusion Error|  Inclusion Error|
|--------------------------------|----------:|----------------:|----------------:|
| Colombian                      |  0.4110017|        0.4709050|        0.1180934|
| Venezuelan Established Migrant |  0.3666667|        0.5416667|        0.0916667|
| Venezuelan Recent Migrant      |  0.3881579|        0.5098684|        0.1019737|
| Venezuelan Resident            |  0.3200000|        0.5200000|        0.1600000|

**Households near the poverty line but with income (workers and non-workers)**

``` r
PMT_2018_Test$No_Income <- case_when(PMT_2018_Test$INGTOTUGARR==0 ~ 1,
                                   T ~ 0)
PMT_2018_Test_NearLP_Income <- PMT_2018_Test[PMT_2018_Test$Near_LP==1 & PMT_2018_Test$No_Income==0, ]
Error_Prop.Table_NPIncome <- prop.table(table(PMT_2018_Test_NearLP_Income$Migration_Status, PMT_2018_Test_NearLP_Income$Errors_1), margin = 1)
```

|                                |    Correct|  Exclusion Error|  Inclusion Error|
|--------------------------------|----------:|----------------:|----------------:|
| Colombian                      |  0.4110017|        0.4709050|        0.1180934|
| Venezuelan Established Migrant |  0.3666667|        0.5416667|        0.0916667|
| Venezuelan Recent Migrant      |  0.3881579|        0.5098684|        0.1019737|
| Venezuelan Resident            |  0.3200000|        0.5200000|        0.1600000|

Across all these subsets, Colombians demonstrate consistently lower rates of exclusion errors. This indicates that the results are robust to different rates of employment and low-income subsets.

------------------------------------------------------------------------

------------------------------------------------------------------------

Identifying Occupational Downgrading
------------------------------------

In order to identify the impact of occupational downgrading, we need to look at the interaction of immigration status, education, and model accuracy. For this to be a possible mechanism, two things need to be true: 1. Venezuelans immigrants must have lower returns to education compared with Colombians. 2. These lower returns must translate to higher rates of exclusion errors.

First, I checked the relative education levels (`P6210`) for our immigrant populations, grouping no education (1) and primary education only (3) into one category.

``` r
Ed_Table <- table(PMT_2018_Test$Migration_Status, PMT_2018_Test$P6210)
Ed_Table_Pct <- prop.table(Ed_Table, margin = 1)*100
```

|                                |         1|          3|         4|         5|         6|
|--------------------------------|---------:|----------:|---------:|---------:|---------:|
| Colombian                      |  5.346616|  28.771751|  13.05071|  25.74521|  27.06672|
| Venezuelan Established Migrant |  1.310861|   7.771536|  19.56929|  40.91760|  30.43071|
| Venezuelan Recent Migrant      |  1.028999|   8.138447|  20.39289|  39.85033|  30.58934|
| Venezuelan Resident            |  2.072539|  13.471503|  15.02591|  31.60622|  37.82383|

This indicates that Venezuelan immigrants generally have higher levels of education than Colombians, supporting the occupational downgrading theory.

### Returns to Education

To establish returns to education, I ran a simple regression of income on the interaction of years of education (`P6040` ) and immigration status, with age (`P6040`) as a control.

``` r
PMT_2018_Test$logINGTOTUGARR_TH <- log1p(PMT_2018_Test$INGTOTUGARR*1000)
Income_Ed_Age_Reg <- lm(logINGTOTUGARR_TH ~ P6210*Migration_Status + P6040,
                    weights = FEX_C,
                    data = PMT_2018_Test)
```

<pre style = "max-height:300px; float: left; width: 910px; overflow-y: auto;">## 
## Call:
## lm(formula = logINGTOTUGARR_TH ~ P6210 * Migration_Status + P6040, 
##     data = PMT_2018_Test, weights = FEX_C)
## 
## Weighted Residuals:
##     Min      1Q  Median      3Q     Max 
## -480.61   -1.98    0.56    3.20   73.18 
## 
## Coefficients:
##                                                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                          18.9081842  0.0203072 931.109  < 2e-16 ***
## P6210                                                 0.3316315  0.0028832 115.022  < 2e-16 ***
## Migration_StatusColombian Other Returnee             -0.4481522  0.2686582  -1.668 0.095295 .  
## Migration_StatusColombian Venezuelan Returnee        -0.0711727  0.1454889  -0.489 0.624703    
## Migration_StatusOther Migrant                        -3.9495754  0.7413330  -5.328 9.96e-08 ***
## Migration_StatusOther Resident                       -0.4845607  0.4419186  -1.096 0.272865    
## Migration_StatusUndefined                             6.6931757  1.9997679   3.347 0.000817 ***
## Migration_StatusVenezuelan Established Migrant        0.2666120  0.2392440   1.114 0.265112    
## Migration_StatusVenezuelan Recent Migrant            -0.7811925  0.2315128  -3.374 0.000740 ***
## Migration_StatusVenezuelan Resident                  -0.1278206  0.5561427  -0.230 0.818221    
## P6040                                                 0.0148037  0.0002495  59.335  < 2e-16 ***
## P6210:Migration_StatusColombian Other Returnee        0.0800913  0.0515821   1.553 0.120498    
## P6210:Migration_StatusColombian Venezuelan Returnee  -0.0507326  0.0356106  -1.425 0.154261    
## P6210:Migration_StatusOther Migrant                   0.6716805  0.1295104   5.186 2.15e-07 ***
## P6210:Migration_StatusOther Resident                  0.1651201  0.0812359   2.033 0.042094 *  
## P6210:Migration_StatusUndefined                      -1.5477514  0.3769193  -4.106 4.02e-05 ***
## P6210:Migration_StatusVenezuelan Established Migrant -0.0861535  0.0475976  -1.810 0.070291 .  
## P6210:Migration_StatusVenezuelan Recent Migrant       0.0357135  0.0459871   0.777 0.437397    
## P6210:Migration_StatusVenezuelan Resident             0.0488199  0.1084339   0.450 0.652547    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.45 on 173327 degrees of freedom
## Multiple R-squared:  0.07607,    Adjusted R-squared:  0.07598 
## F-statistic: 792.8 on 18 and 173327 DF,  p-value: < 2.2e-16
</pre>
The significant negative interaction with Venezuelan immigrants also suggests that occupational downgrading is occuring.

### Exclusion Errors by Education Level

Finally, I compared exclusion error rates across education levels for all immigrant subpopulations.

``` r
# Calculating number of exclusion errors
Error_Table_Ed <- (table(PMT_2018_Test$Migration_Status, PMT_2018_Test$P6210, PMT_2018_Test$Errors_1=="Exclusion Error"))
Error_Table_Ed[, 7,] <- Error_Table_Ed[, 1,]+Error_Table_Ed[, 2,]+Error_Table_Ed[, 3,]

# Calculating denominator
N_Table_Ed <- (table(PMT_2018_Test$Migration_Status, PMT_2018_Test$P6210))
N_Table_Ed[, 7] <- N_Table_Ed[, 1]+N_Table_Ed[, 2]+N_Table_Ed[, 3]

# Calculating error rates as a percentage
Error_Pct_Table <- 100*Error_Table_Ed[c(1, 7:9), c(4:7), 2]/N_Table_Ed[c(1, 7:9), c(4:7)]

# Rearranging Table
Error_Pct_Table <- Error_Pct_Table[c("Colombian","Venezuelan Resident", "Venezuelan Established Migrant", "Venezuelan Recent Migrant"),c(4,1:3)]

# Renaming Columns
colnames(Error_Pct_Table)[1] = "Primary"
colnames(Error_Pct_Table)[2] = "Secondary"
colnames(Error_Pct_Table)[3] = "Media"
colnames(Error_Pct_Table)[4] = "University"
```

|                                |   Primary|  Secondary|     Media|  University|
|--------------------------------|---------:|----------:|---------:|-----------:|
| Colombian                      |  22.31037|   20.37029|  16.73431|    6.301544|
| Venezuelan Resident            |  36.66667|   20.68966|  27.86885|    9.589041|
| Venezuelan Established Migrant |  34.02062|   41.62679|  39.81693|   21.846154|
| Venezuelan Recent Migrant      |  39.79592|   42.20183|  38.02817|   24.464832|

The relatively higher exclusion error rates at higher education levels indicates that occupational downgrading is likely happening, contributing to the overall bias against Venezuelan immigrants in the PMT model.

------------------------------------------------------------------------

------------------------------------------------------------------------

Modified PMT Model
------------------

To assess a possible way to address this problem, I tested a PMT model that incorporates immigration status.

``` r
PMT_2018_IMM <- lm(logINGTOTUGARR_TH ~ factor(Migration_Status) + factor(DPTO) + factor(P5090) + factor(P5080) + factor(P5070) + factor(P5020) + factor(P5050) + factor(P4010) + factor(P4020) + factor(P6210) +
                   P5000 + P5010 + P5030 + P4040 + P4030S1 + P5210S1 + P5210S5 + P5210S4 + P5210S11 + 
                   P5210S2 + P5210S10 + P5210S9 + P5210S18 + P5210S16 + P5210S15 + P5210S21 + P5210S22 + 
                   P6008 + P5210S3 + P6090, 
                 weights = FEX_C,
                 data = PMT_2018_Training)
```

<pre style = "max-height:400px; float: left; width: 910px; overflow-y: auto;">## 
## Call:
## lm(formula = logINGTOTUGARR_TH ~ factor(Migration_Status) + factor(DPTO) + 
##     factor(P5090) + factor(P5080) + factor(P5070) + factor(P5020) + 
##     factor(P5050) + factor(P4010) + factor(P4020) + factor(P6210) + 
##     P5000 + P5010 + P5030 + P4040 + P4030S1 + P5210S1 + P5210S5 + 
##     P5210S4 + P5210S11 + P5210S2 + P5210S10 + P5210S9 + P5210S18 + 
##     P5210S16 + P5210S15 + P5210S21 + P5210S22 + P6008 + P5210S3 + 
##     P6090, data = PMT_2018_Training, weights = FEX_C)
## 
## Weighted Residuals:
##      Min       1Q   Median       3Q      Max 
## -176.252   -1.504    0.410    2.327   43.393 
## 
## Coefficients:
##                                                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                             5.4632527  0.1108861  49.269  < 2e-16 ***
## factor(Migration_Status)Colombian Other Returnee        0.1436814  0.0463267   3.101 0.001927 ** 
## factor(Migration_Status)Colombian Venezuelan Returnee   0.0987943  0.0424931   2.325 0.020079 *  
## factor(Migration_Status)Other Migrant                  -0.6557976  0.1252758  -5.235 1.66e-07 ***
## factor(Migration_Status)Other Resident                  0.2353953  0.0844448   2.788 0.005313 ** 
## factor(Migration_Status)Undefined                       0.9010861  0.3985635   2.261 0.023774 *  
## factor(Migration_Status)Venezuelan Established Migrant  0.2729148  0.0486477   5.610 2.03e-08 ***
## factor(Migration_Status)Venezuelan Recent Migrant       0.3340443  0.0495529   6.741 1.59e-11 ***
## factor(Migration_Status)Venezuelan Resident             0.1296883  0.1066477   1.216 0.223974    
## factor(DPTO)08                                          0.0536511  0.0206553   2.597 0.009394 ** 
## factor(DPTO)11                                          0.0785266  0.0133019   5.903 3.58e-09 ***
## factor(DPTO)13                                         -0.0084034  0.0228634  -0.368 0.713211    
## factor(DPTO)15                                         -0.0898993  0.0227522  -3.951 7.78e-05 ***
## factor(DPTO)17                                         -0.0984609  0.0257672  -3.821 0.000133 ***
## factor(DPTO)18                                         -0.0960934  0.0374888  -2.563 0.010372 *  
## factor(DPTO)19                                         -0.2428824  0.0241225 -10.069  < 2e-16 ***
## factor(DPTO)20                                         -0.2366040  0.0282501  -8.375  < 2e-16 ***
## factor(DPTO)23                                         -0.0994316  0.0284347  -3.497 0.000471 ***
## factor(DPTO)25                                          0.1291348  0.0195547   6.604 4.05e-11 ***
## factor(DPTO)27                                         -0.3820359  0.0481441  -7.935 2.14e-15 ***
## factor(DPTO)41                                         -0.2019678  0.0268035  -7.535 4.96e-14 ***
## factor(DPTO)44                                         -0.0861965  0.0396751  -2.173 0.029818 *  
## factor(DPTO)47                                         -0.0964488  0.0310814  -3.103 0.001916 ** 
## factor(DPTO)50                                         -0.0175333  0.0288966  -0.607 0.544012    
## factor(DPTO)52                                         -0.1579214  0.0230227  -6.859 6.99e-12 ***
## factor(DPTO)54                                         -0.1230033  0.0258653  -4.756 1.98e-06 ***
## factor(DPTO)63                                         -0.0834726  0.0261769  -3.189 0.001430 ** 
## factor(DPTO)66                                         -0.0475218  0.0236644  -2.008 0.044632 *  
## factor(DPTO)68                                         -0.0254867  0.0195489  -1.304 0.192328    
## factor(DPTO)70                                         -0.0160460  0.0338187  -0.474 0.635166    
## factor(DPTO)73                                         -0.0587788  0.0239022  -2.459 0.013931 *  
## factor(DPTO)76                                         -0.0715407  0.0154392  -4.634 3.60e-06 ***
## factor(P5090)2                                         -0.0188464  0.0202571  -0.930 0.352189    
## factor(P5090)3                                         -0.2566443  0.0091220 -28.135  < 2e-16 ***
## factor(P5090)4                                         -0.1134027  0.0112740 -10.059  < 2e-16 ***
## factor(P5090)5                                         -0.2938610  0.0251193 -11.699  < 2e-16 ***
## factor(P5090)6                                         -0.5516791  0.1077002  -5.122 3.03e-07 ***
## factor(P5080)2                                          0.1393831  0.1931010   0.722 0.470412    
## factor(P5080)3                                         -0.1094532  0.0276760  -3.955 7.67e-05 ***
## factor(P5080)4                                         -0.1215157  0.0286318  -4.244 2.20e-05 ***
## factor(P5080)5                                         -0.3440452  0.0343872 -10.005  < 2e-16 ***
## factor(P5080)6                                          0.1484653  0.1457347   1.019 0.308333    
## factor(P5080)7                                         -0.5261351  0.3873571  -1.358 0.174384    
## factor(P5070)2                                          0.0062369  0.0325802   0.191 0.848188    
## factor(P5070)3                                          0.0077578  0.0184284   0.421 0.673781    
## factor(P5070)4                                         -0.0258060  0.0409546  -0.630 0.528623    
## factor(P5070)5                                          0.0007526  0.0361119   0.021 0.983372    
## factor(P5020)2                                         -0.0715554  0.0144594  -4.949 7.49e-07 ***
## factor(P5020)3                                         -0.2073291  0.0276911  -7.487 7.15e-14 ***
## factor(P5020)4                                         -0.2157674  0.0756909  -2.851 0.004365 ** 
## factor(P5020)5                                          0.0978884  0.0666840   1.468 0.142126    
## factor(P5050)2                                         -0.2012219  0.0694675  -2.897 0.003773 ** 
## factor(P5050)3                                         -0.1108276  0.1122841  -0.987 0.323633    
## factor(P5050)4                                         -0.2836188  0.1186286  -2.391 0.016815 *  
## factor(P5050)5                                         -0.1186654  0.0745111  -1.593 0.111259    
## factor(P5050)6                                          0.2440062  0.1098173   2.222 0.026292 *  
## factor(P5050)7                                          0.1251974  0.1463571   0.855 0.392320    
## factor(P5050)8                                         -0.0266023  0.1261426  -0.211 0.832973    
## factor(P5050)9                                          0.1444233  0.0755986   1.910 0.056088 .  
## factor(P5050)10                                         0.1150751  0.0281234   4.092 4.29e-05 ***
## factor(P4010)2                                         -0.4656263  0.0747724  -6.227 4.78e-10 ***
## factor(P4010)3                                         -0.0775585  0.0298090  -2.602 0.009275 ** 
## factor(P4010)4                                         -0.0724762  0.0272144  -2.663 0.007743 ** 
## factor(P4010)5                                         -0.1000327  0.0299510  -3.340 0.000839 ***
## factor(P4010)6                                          0.5966439  0.1617063   3.690 0.000225 ***
## factor(P4010)7                                         -0.0428039  0.0814242  -0.526 0.599106    
## factor(P4010)8                                          0.1602361  0.0991670   1.616 0.106139    
## factor(P4010)9                                          0.7107343  0.3478152   2.043 0.041015 *  
## factor(P4020)2                                          0.0208862  0.0295005   0.708 0.478953    
## factor(P4020)3                                          0.1056600  0.0383034   2.759 0.005809 ** 
## factor(P4020)4                                          0.1870411  0.0306039   6.112 9.93e-10 ***
## factor(P4020)5                                          0.5606517  0.0680865   8.234  < 2e-16 ***
## factor(P4020)6                                          0.6151883  0.0436342  14.099  < 2e-16 ***
## factor(P4020)7                                          0.1955725  0.0944644   2.070 0.038426 *  
## factor(P6210)2                                          0.3217852  0.3710238   0.867 0.385787    
## factor(P6210)3                                          0.0948282  0.0184044   5.152 2.58e-07 ***
## factor(P6210)4                                          0.1953351  0.0200203   9.757  < 2e-16 ***
## factor(P6210)5                                          0.2300506  0.0191369  12.021  < 2e-16 ***
## factor(P6210)6                                          0.5104684  0.0200548  25.454  < 2e-16 ***
## factor(P6210)9                                          0.0415013  0.2490026   0.167 0.867630    
## P5000                                                   0.0813892  0.0043715  18.618  < 2e-16 ***
## P5010                                                   0.0384671  0.0067579   5.692 1.26e-08 ***
## P5030                                                   0.0672976  0.0167023   4.029 5.60e-05 ***
## P4040                                                   0.0402580  0.0117069   3.439 0.000585 ***
## P4030S1                                                 0.1051547  0.1002040   1.049 0.293996    
## P5210S1                                                 0.0699018  0.0100483   6.957 3.53e-12 ***
## P5210S5                                                 0.0681018  0.0128376   5.305 1.13e-07 ***
## P5210S4                                                 0.1009666  0.0095206  10.605  < 2e-16 ***
## P5210S11                                                0.1715553  0.0175720   9.763  < 2e-16 ***
## P5210S2                                                 0.0939845  0.0092672  10.142  < 2e-16 ***
## P5210S10                                                0.0792378  0.0107528   7.369 1.74e-13 ***
## P5210S9                                                 0.1228641  0.0107843  11.393  < 2e-16 ***
## P5210S18                                                0.1651060  0.0195852   8.430  < 2e-16 ***
## P5210S16                                                0.0692699  0.0100907   6.865 6.74e-12 ***
## P5210S15                                                0.0667172  0.0077986   8.555  < 2e-16 ***
## P5210S21                                                0.1207772  0.0085807  14.075  < 2e-16 ***
## P5210S22                                                0.3824631  0.0114456  33.416  < 2e-16 ***
## P6008                                                   0.1034964  0.0029650  34.906  < 2e-16 ***
## P5210S3                                                 0.1458431  0.0104993  13.891  < 2e-16 ***
## P6090                                                   0.2703397  0.0161209  16.769  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.23 on 51091 degrees of freedom
##   (6591 observations deleted due to missingness)
## Multiple R-squared:  0.4245, Adjusted R-squared:  0.4234 
## F-statistic: 380.7 on 99 and 51091 DF,  p-value: < 2.2e-16
</pre>
This model has a slightly higher adjusted R<sup>2</sup> value (0.4233938), but this increase in accuracy may or may not improve the exclusion error rates. In order to check, I ran the same steps as the original model analysis:

``` r
# Predicting income from LM
PMT_2018_Test$logINGTOTUGARRTH_PRED_IMM <- NA
PMT_2018_Test$logINGTOTUGARRTH_PRED_IMM[PMT_2018_Test$Complete_Case == 1] <- predict.lm(PMT_2018_IMM, PMT_2018_Test, na.action = na.exclude)
PMT_2018_Test$logINGTOTUGARRTH_PRED_IMM <- expm1(PMT_2018_Test$logINGTOTUGARRTH_PRED_IMM)*1000

# Identifying predicted poverty from predicted income
PMT_2018_Test$POBRE_PREDICTED_IMM <- case_when(PMT_2018_Test$logINGTOTUGARRTH_PRED_IMM>PMT_2018_Test$LP ~ 0,
                                           TRUE ~ 1)

# Identifying Errors
PMT_2018_Test$Errors_IMM <- case_when(PMT_2018_Test$POBRE==0 & PMT_2018_Test$POBRE_PREDICTED_IMM==1  ~ "Inclusion Error",
                                  PMT_2018_Test$POBRE==1 & PMT_2018_Test$POBRE_PREDICTED_IMM==0  ~ "Exclusion Error",
                                  TRUE ~ "Correct")

# Generating table of errors by migration group
Error_Table_IMM <- table(PMT_2018_Test$Migration_Status, PMT_2018_Test$Errors_IMM)
```

|                                |    Correct|  Exclusion Error|  Inclusion Error|
|--------------------------------|----------:|----------------:|----------------:|
| Colombian                      |  0.7605600|        0.1628622|        0.0765778|
| Venezuelan Established Migrant |  0.5908240|        0.3426966|        0.0664794|
| Venezuelan Recent Migrant      |  0.5734331|        0.3507951|        0.0757717|
| Venezuelan Resident            |  0.7202073|        0.2124352|        0.0673575|

This shows that even with the inclusion of immigration status in the PMT model, there are higher rates of exclusion error for immigrant subpopulations. I further confirmed that these differences were statistically significant:

-   Venezuelan Resident:

``` r
pander((prop.test(c(Error_Table_IMM["Colombian", "Exclusion Error"], Error_Table_IMM["Venezuelan Resident", "Exclusion Error"]), c( sum(Error_Table_IMM["Colombian", ]), sum(Error_Table_IMM["Venezuelan Resident", ])))))
```

<table>
<caption>2-sample test for equality of proportions with continuity correction: <code>c(Error_Table_IMM[&quot;Colombian&quot;, &quot;Exclusion Error&quot;], Error_Table_IMM[&quot;Venezuelan Resident&quot;,  out of c(sum(Error_Table_IMM[&quot;Colombian&quot;, ]), sum(Error_Table_IMM[&quot;Venezuelan Resident&quot;,     &quot;Exclusion Error&quot;]) out of     ]))</code></caption>
<colgroup>
<col width="22%" />
<col width="6%" />
<col width="13%" />
<col width="33%" />
<col width="12%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Test statistic</th>
<th align="center">df</th>
<th align="center">P value</th>
<th align="center">Alternative hypothesis</th>
<th align="center">prop 1</th>
<th align="center">prop 2</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">3.12</td>
<td align="center">1</td>
<td align="center">0.07734</td>
<td align="center">two.sided</td>
<td align="center">0.1629</td>
<td align="center">0.2124</td>
</tr>
</tbody>
</table>

-   Venezuelan Established Migrant

``` r
pander(prop.test(c(Error_Table_IMM["Colombian", "Exclusion Error"], Error_Table_IMM["Venezuelan Established Migrant", "Exclusion Error"]), c( sum(Error_Table_IMM["Colombian", ]), sum(Error_Table_IMM["Venezuelan Established Migrant", ]))))
```

<table>
<caption>2-sample test for equality of proportions with continuity correction: <code>c(Error_Table_IMM[&quot;Colombian&quot;, &quot;Exclusion Error&quot;], Error_Table_IMM[&quot;Venezuelan Established Migrant&quot;,  out of c(sum(Error_Table_IMM[&quot;Colombian&quot;, ]), sum(Error_Table_IMM[&quot;Venezuelan Established Migrant&quot;,     &quot;Exclusion Error&quot;]) out of     ]))</code></caption>
<colgroup>
<col width="20%" />
<col width="6%" />
<col width="21%" />
<col width="30%" />
<col width="10%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Test statistic</th>
<th align="center">df</th>
<th align="center">P value</th>
<th align="center">Alternative hypothesis</th>
<th align="center">prop 1</th>
<th align="center">prop 2</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">249</td>
<td align="center">1</td>
<td align="center">4.224e-56 * * *</td>
<td align="center">two.sided</td>
<td align="center">0.1629</td>
<td align="center">0.3427</td>
</tr>
</tbody>
</table>

-   Venezuelan Recent Migrant

``` r
pander(prop.test(c(Error_Table_IMM["Colombian", "Exclusion Error"], Error_Table_IMM["Venezuelan Recent Migrant", "Exclusion Error"]), c( sum(Error_Table_IMM["Colombian", ]), sum(Error_Table_IMM["Venezuelan Recent Migrant", ]))))
```

<table>
<caption>2-sample test for equality of proportions with continuity correction: <code>c(Error_Table_IMM[&quot;Colombian&quot;, &quot;Exclusion Error&quot;], Error_Table_IMM[&quot;Venezuelan Recent Migrant&quot;,  out of c(sum(Error_Table_IMM[&quot;Colombian&quot;, ]), sum(Error_Table_IMM[&quot;Venezuelan Recent Migrant&quot;,     &quot;Exclusion Error&quot;]) out of     ]))</code></caption>
<colgroup>
<col width="20%" />
<col width="6%" />
<col width="20%" />
<col width="30%" />
<col width="10%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Test statistic</th>
<th align="center">df</th>
<th align="center">P value</th>
<th align="center">Alternative hypothesis</th>
<th align="center">prop 1</th>
<th align="center">prop 2</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">272.2</td>
<td align="center">1</td>
<td align="center">3.74e-61 * * *</td>
<td align="center">two.sided</td>
<td align="center">0.1629</td>
<td align="center">0.3508</td>
</tr>
</tbody>
</table>

Thus we can conclude that simply including immigration status in the model is not enough to compensate for the overall bias.

------------------------------------------------------------------------

------------------------------------------------------------------------

Conclusion
----------

In addition to documenting the analysis for the Colombia case study, this process can hopefully be adapted for use in other evaluations of PMTs. In particular, this type of analysis should be performed prior to the implementation of PMTs, to ensure that they are not biased against particular vulnerable subpopulations.
