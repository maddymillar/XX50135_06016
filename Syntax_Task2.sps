* Encoding: UTF-8.
* Encodin*File checked visually for missing data. 
*All variables set to missing values at '999'

*Firstly to examine descriptives

DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=Age AutSup1 AutSup2 AutSup3 IM1 IM2 IM3 Energy1 Energy2 Energy3
  /STATISTICS=MEAN STDDEV MIN MAX.

*no coding ambiguities detected in terms of maximum or minimum values. All conform within the range of 1-7.

*Secondly to examine string variables

FREQUENCIES VARIABLES=Gender
  /ORDER=ANALYSIS.

*Similarly, no ambiguities in the data.

*Missing Data:

MVA VARIABLES=Age AutSup1 AutSup2 AutSup3 IM1 IM2 IM3 Energy1 Energy2 Energy3 Gender
  /MAXCAT=25
  /CATEGORICAL=Gender
  /TTEST PROB PERCENT=5
  /CROSSTAB PERCENT=5
  /MPATTERN
  /TPATTERN PERCENT=1 DESCRIBE = Age AutSup1 AutSup2 AutSup3 IM1 IM2 IM3 Energy1 Energy2 Energy3 Gender
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=25).

*Before addressing missing data using imputation, outliers will be examined.

*Outliers
*Univariate outliers using z-scores (therefore, depends on the assumption of skewness and kurtosis to indicate the distribution of the data)

DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=Age AutSup1 AutSup2 AutSup3 IM1 IM2 IM3 Energy1 Energy2 Energy3
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

*Assumption has not been violated, so able to use z-scores to detect outliers.

DESCRIPTIVES VARIABLES=Age AutSup1 AutSup2 AutSup3 IM1 IM2 IM3 Energy1 Energy2 Energy3
  /SAVE
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

*Transform z-scores into absolute values to see positive and negative values calculated as outliers where necessary.

COMPUTE ZAge=abs(ZAge).
EXECUTE.

COMPUTE ZAutSup1=abs(ZAutSup1).
EXECUTE.

COMPUTE ZAutSup2=abs(ZAutSup2).
EXECUTE.

COMPUTE ZAutSup3=abs(ZAutSup3).
EXECUTE.

COMPUTE ZIM1=abs(ZIM1).
EXECUTE.

COMPUTE ZIM2=abs(ZIM2).
EXECUTE.

COMPUTE ZIM3=abs(ZIM3).
EXECUTE.

COMPUTE ZEnergy1=abs(ZEnergy1).
EXECUTE.

COMPUTE ZEnergy2=abs(ZEnergy2).
EXECUTE.

COMPUTE ZEnergy3=abs(ZEnergy3).
EXECUTE.

* According to Tabachnick & Fidell (2007), z scores > 3.29. Therefore, z-scores were recoded. They were also recoded according to Field (2007) to also reflect probable, possible and normal cases. 
* Label extreme outliers as 1, probable outliers as 2, potential outliers as 3 and normal range as 4.

RECODE ZAge
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Age.
EXECUTE.
ADD VALUE LABELS OUTLIER_Age 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZAutSup1
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_AutSup1.
EXECUTE.
ADD VALUE LABELS OUTLIER_AutSup1 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZAutSup2
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_AutSup2.
EXECUTE.
ADD VALUE LABELS OUTLIER_AutSup2 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZAutSup3 
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_AutSup3.
EXECUTE.
ADD VALUE LABELS OUTLIER_AutSup3 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZIM1
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_IM1.
EXECUTE.
ADD VALUE LABELS OUTLIER_IM1 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZIM2
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_IM2.
EXECUTE.
ADD VALUE LABELS OUTLIER_IM2 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZIM3
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_IM3.
EXECUTE.
ADD VALUE LABELS OUTLIER_IM3 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZEnergy1
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Energy1.
EXECUTE.
ADD VALUE LABELS OUTLIER_Energy1 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZEnergy2
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Energy2.
EXECUTE.
ADD VALUE LABELS OUTLIER_Energy2 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZEnergy3
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Energy3.
EXECUTE.
ADD VALUE LABELS OUTLIER_Energy3 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

*Use frequency tables to determine how many outliers there are.

FREQUENCIES VARIABLES=OUTLIER_Age OUTLIER_AutSup1 OUTLIER_AutSup2 OUTLIER_AutSup3 OUTLIER_IM1 
    OUTLIER_IM2 OUTLIER_IM3 OUTLIER_Energy1 OUTLIER_Energy2 OUTLIER_Energy3
  /ORDER=ANALYSIS.

*Multivariate Outliers (using Malhalanobis Distance)

REGRESSION
/MISSING LISTWISE
/STATISTICS COEFF OUTS R ANOVA
/CRITERIA = PIN(.05) POUT (.10)
/NOORIGIN
/DEPENDENT Num 
/METHOD = ENTER AutSup1 AutSup2 AutSup3 IM1 IM2 IM3 Energy1 Energy2 Energy3 AGE GENDER
/SAVE MAHAL

COMPUTE PROB_MAH = 1-CDF.CHISQ(MAH_1,11).
EXECUTE

*based on this, there are no values which are significant at the  < .001 level (Tabachnick & Fidell, 2007). 

*In order to deal with outliers prior to imputation, a sensitivity analysis will be used. 

*Based on missing values, imputation via Expectation Maximization will now be conducted.

MVA VARIABLES=Age AutSup1 AutSup2 AutSup3 IM1 IM2 IM3 Energy1 Energy2 Energy3 Gender
  /MPATTERN
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE='/Users/MaddyMillar/SPSSFiles/file1.sav').
EXECUTE.

*In new file, rename each variable so that when the files are merged, they are different variables.

RENAME VARIABLES (Age = Age_EM).
RENAME VARIABLES (AutSup1 = AutSup1_EM).
RENAME VARIABLES (AutSup2 = AutSup2_EM).
RENAME VARIABLES (AutSup3 = AutSup3_EM).
RENAME VARIABLES (IM1 = IM1_EM).
RENAME VARIABLES (IM2 = IM2_EM).
RENAME VARIABLES (IM3 = IM3_EM).
RENAME VARIABLES (Energy1 = Energy1_EM).
RENAME VARIABLES (Energy2 = Energy2_EM).
RENAME VARIABLES (Energy3 = Energy3_EM).
RENAME VARIABLES (Gender = Gender_EM).

*Merge files

MATCH FILES /FILE=*
  /FILE='DataSet2'
  /BY Num.
EXECUTE.

*Check new variables for missing data

MVA VARIABLES=Age_EM AutSup1_EM AutSup2_EM AutSup3_EM IM1_EM IM2_EM IM3_EM Energy1_EM Energy2_EM 
    Energy3_EM Gender_EM
  /MAXCAT=25
  /CATEGORICAL=Gender_EM.

*There is no missing data. We should re-run outlier analysis to check no new outliers are present.

DESCRIPTIVES VARIABLES=Age_EM AutSup1_EM AutSup2_EM AutSup3_EM IM1_EM IM2_EM IM3_EM Energy1_EM 
    Energy2_EM Energy3_EM
  /SAVE
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

*Assumptions are not violated in terms of kurtosis and skewness.

*Transform z-scores into absolute values to see positive and negative values calculated as outliers where necessary.

COMPUTE ZAge_EM=abs(ZAge_EM).
EXECUTE.

COMPUTE ZAutSup1_EM=abs(ZAutSup1_EM).
EXECUTE.

COMPUTE ZAutSup2_EM=abs(ZAutSup2_EM).
EXECUTE.

COMPUTE ZAutSup3_EM=abs(ZAutSup3_EM).
EXECUTE.

COMPUTE ZIM1_EM=abs(ZIM1_EM).
EXECUTE.

COMPUTE ZIM2_EM=abs(ZIM2_EM).
EXECUTE.

COMPUTE ZIM3_EM=abs(ZIM3_EM).
EXECUTE.

COMPUTE ZEnergy1_EM=abs(ZEnergy1_EM).
EXECUTE.

COMPUTE ZEnergy2_EM=abs(ZEnergy2_EM).
EXECUTE.

COMPUTE ZEnergy3_EM=abs(ZEnergy3_EM).
EXECUTE.

* According to Tabachnick & Fidell (2007), z scores > 3.29. Therefore, z-scores were recoded. They were also recoded according to Field (2007) to also reflect probable, possible and normal cases. 
* Label extreme outliers as 1, probable outliers as 2, potential outliers as 3 and normal range as 4.

RECODE ZAge_EM
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Age_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_Age_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZAutSup1_EM
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_AutSup1_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_AutSup1_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZAutSup2_EM
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_AutSup2_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_AutSup2_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZAutSup3_EM 
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_AutSup3_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_AutSup3_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZIM1_EM
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_IM1_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_IM1_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZIM2_EM
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_IM2_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_IM2_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZIM3_EM
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_IM3_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_IM3_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZEnergy1_EM
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Energy1_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_Energy1_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZEnergy2_EM
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Energy2_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_Energy2_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZEnergy3_EM
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Energy3_EM.
EXECUTE.
ADD VALUE LABELS OUTLIER_Energy3_EM 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

*To check outliers, run a frequency analysis

FREQUENCIES VARIABLES=OUTLIER_Age_EM OUTLIER_AutSup1_EM OUTLIER_AutSup2_EM OUTLIER_AutSup3_EM 
    OUTLIER_IM1_EM OUTLIER_IM2_EM OUTLIER_IM3_EM OUTLIER_Energy1_EM OUTLIER_Energy2_EM 
    OUTLIER_Energy3_EM
  /ORDER=ANALYSIS.

*No additional outliers identified.

*Transforming Variables

COMPUTE AutSup=MEAN(AutSup1,AutSup2,AutSup3).
EXECUTE.

COMPUTE IntMot = MEAN(IM1,IM2,IM3).
EXECUTE.

COMPUTE Energy = MEAN(Energy1, Energy2, Energy3).
EXECUTE.

COMPUTE AutSup_EM=MEAN(AutSup1_EM,AutSup2_EM,AutSup3_EM).
EXECUTE.

COMPUTE IntMot_EM = MEAN(IM1_EM,IM2_EM,IM3_EM).
EXECUTE.

COMPUTE Energy_EM = MEAN(Energy1_EM, Energy2_EM, Energy3_EM).
EXECUTE.

*Mediation Analysis
*Assumptions for linear regression model using EM variables.

*1. Normality - although not an assumption of a regression, useful to know.

DESCRIPTIVES VARIABLES=AutSup_EM IntMot_EM Energy_EM
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

* skewness should be below 2, kurtosis should be below 7. In this case, all variables fall below these respective values.

*2. Linearity and Additivity

*C-Path
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Energy_EM AutSup_EM MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: AutSup_EM=col(source(s), name("AutSup_EM"))
  DATA: Energy_EM=col(source(s), name("Energy_EM"))
  GUIDE: axis(dim(1), label("AutSup_EM"))
  GUIDE: axis(dim(2), label("Energy_EM"))
  ELEMENT: point(position(AutSup_EM*Energy_EM))
  ELEMENT: line(position(smooth.linear(AutSup_EM*Energy_EM)))
END GPL.

*B-Path
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Energy_EM IntMot_EM MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: IntMot_EM=col(source(s), name("IntMot_EM"))
  DATA: Energy_EM=col(source(s), name("Energy_EM"))
  GUIDE: axis(dim(1), label("IntMot_EM"))
  GUIDE: axis(dim(2), label("Energy_EM"))
  ELEMENT: point(position(IntMot_EM*Energy_EM))
  ELEMENT: line(position(smooth.linear(IntMot_EM*Energy_EM)))
END GPL.

*A-Path
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=IntMot_EM AutSup_EM MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: AutSup_EM=col(source(s), name("AutSup_EM"))
  DATA: IntMot_EM=col(source(s), name("IntMot_EM"))
  GUIDE: axis(dim(1), label("AutSup_EM"))
  GUIDE: axis(dim(2), label("IntMot_EM"))
  ELEMENT: point(position(AutSup_EM*IntMot_EM))
  ELEMENT: line(position(smooth.linear(AutSup_EM*IntMot_EM)))
END GPL.

*No violations of the assumption of linearity

*3. Independent Errors
*Durbin Watson test statistic is 1.761 which is within acceptable range for AutSup_EM * Energy_EM
*Durbin Watson test statistic is 1.733 which is within acceptable range for IntMot_EM * Energy_EM

*4. Homoscedasticity
*5. Normally Distributed Errors

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Energy_EM
  /METHOD=ENTER AutSup_EM
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Energy_EM
  /METHOD=ENTER IntMot_EM
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

*Now that assumptions have been met, the mediation analysis can be started.

PROCESS
y=Energy_EM
  /x=AutSup_EM
  /m=IntMot_EM
  /total=1
  /effsize=1
  /stand=1
  /boot=5000
  /seed=50135
  /xmtest=1
  /conf=95
  /model=4.
RUN.

*In order to re-run the analysis without the imputed variables, another simple mediation was conducted using the original variables.

*Assumptions were re-checked

DESCRIPTIVES VARIABLES=AutSup IntMot Energy
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

* skewness should be below 2, kurtosis should be below 7. In this case, all variables fall below these respective values.

*2. Linearity and Additivity

*C'-Path
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Energy AutSup MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: AutSup=col(source(s), name("AutSup"))
  DATA: Energy=col(source(s), name("Energy"))
  GUIDE: axis(dim(1), label("AutSup"))
  GUIDE: axis(dim(2), label("Energy"))
  ELEMENT: point(position(AutSup*Energy))
  ELEMENT: line(position(smooth.linear(AutSup*Energy)))
END GPL.

*B-Path
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Energy IntMot MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: IntMot=col(source(s), name("IntMot"))
  DATA: Energy=col(source(s), name("Energy"))
  GUIDE: axis(dim(1), label("IntMot"))
  GUIDE: axis(dim(2), label("Energy"))
  ELEMENT: point(position(IntMot*Energy))
  ELEMENT: line(position(smooth.linear(IntMot*Energy)))
END GPL.

*A-Path
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=IntMot AutSup MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: AutSup=col(source(s), name("AutSup"))
  DATA: IntMot=col(source(s), name("IntMot"))
  GUIDE: axis(dim(1), label("AutSup"))
  GUIDE: axis(dim(2), label("IntMot"))
  ELEMENT: point(position(AutSup*IntMot))
  ELEMENT: line(position(smooth.linear(AutSup*IntMot)))
END GPL.

*No violations of the assumption of linearity

*3. Independent Errors
*Durbin Watson test statistic is 1.870 which is within acceptable range for AutSup * Energy
*Durbin Watson test statistic is 1.847 which is within acceptable range for IntMot * Energy

*4. Homoscedasticity
*5. Normally Distributed Errors

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Energy
  /METHOD=ENTER AutSup
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Energy
  /METHOD=ENTER IntMot
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

*No regression assumptions violated, therefore, mediation conducted.

PROCESS
y=Energy
  /x=AutSup
  /m=IntMot
  /total=1
  /effsize=1
  /stand=1
  /boot=5000
  /seed=50135
  /xmtest=1
  /conf=95
  /model=4.
RUN.

*Sensitivity Analysis
*Filter outliers that do not conform to set standards of extreme, probable and potential outliers.

USE ALL.
COMPUTE filter_$=(OUTLIER_AutSup2 >= 3 AND OUTLIER_AutSup3 >= 3 AND 
    OUTLIER_IM1 >= 3 AND OUTLIER_IM3 >= 3 AND OUTLIER_Energy1 >= 2 AND OUTLIER_Energy2 >= 3).
VARIABLE LABELS filter_$ 'OUTLIER_AutSup2 >= 3 AND OUTLIER_AutSup3 >= 3 AND '+
    'OUTLIER_IM1 >= 3 AND OUTLIER_IM3 >= 3 AND OUTLIER_Energy1 >= 2 AND OUTLIER_Energy2 >= 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=Num
  /ORDER=ANALYSIS.

*We are now left with 184 participants which is within the acceptable range for power, according to G-Power.

*There is no missing data and the outliers have been excluded. 

*Regression assumptions checked prior to mediation analysis

*1. Normality - although not an assumption of a regression, useful to know.

DESCRIPTIVES VARIABLES=AutSup_EM_SA IntMot_EM_SA Energy_EM_SA
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

* skewness should be below 2, kurtosis should be below 7. In this case, all variables fall below these respective values.

*2. Linearity and Additivity

*C-Path
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Energy_EM AutSup_EM MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: AutSup_EM=col(source(s), name("AutSup_EM"))
  DATA: Energy_EM=col(source(s), name("Energy_EM"))
  GUIDE: axis(dim(1), label("AutSup_EM"))
  GUIDE: axis(dim(2), label("Energy_EM"))
  ELEMENT: point(position(AutSup_EM*Energy_EM))
  ELEMENT: line(position(smooth.linear(AutSup_EM*Energy_EM)))
END GPL.

*B-Path
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Energy_EM IntMot_EM MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: IntMot_EM=col(source(s), name("IntMot_EM"))
  DATA: Energy_EM=col(source(s), name("Energy_EM"))
  GUIDE: axis(dim(1), label("IntMot_EM"))
  GUIDE: axis(dim(2), label("Energy_EM"))
  ELEMENT: point(position(IntMot_EM*Energy_EM))
  ELEMENT: line(position(smooth.linear(IntMot_EM*Energy_EM)))
END GPL.

*A-Path
* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=IntMot_EM AutSup_EM MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: AutSup_EM=col(source(s), name("AutSup_EM"))
  DATA: IntMot_EM=col(source(s), name("IntMot_EM"))
  GUIDE: axis(dim(1), label("AutSup_EM"))
  GUIDE: axis(dim(2), label("IntMot_EM"))
  ELEMENT: point(position(AutSup_EM*IntMot_EM))
  ELEMENT: line(position(smooth.linear(AutSup_EM*IntMot_EM)))
END GPL.

*No violations of the assumption of linearity

*3. Independent Errors
*Durbin Watson test statistic is 1.838 which is within acceptable range for AutSup_EM * Energy_EM
*Durbin Watson test statistic is 1.819 which is within acceptable range for IntMot_EM * Energy_EM

*4. Homoscedasticity
*5. Normally Distributed Errors

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Energy_EM
  /METHOD=ENTER AutSup_EM
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Energy_EM
  /METHOD=ENTER IntMot_EM
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

*Now that assumptions have been met, the mediation analysis can be started.

PROCESS
y=Energy_EM
  /x=AutSup_EM
  /m=IntMot_EM
  /total=1
  /effsize=1
  /stand=1
  /boot=5000
  /seed=50135
  /xmtest=1
  /conf=95
  /model=4.
RUN.

