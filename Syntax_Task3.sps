* Encoding: UTF-8.
*Accuracy of the Data
*Set all missing values to 999 in variable view.

*Descriptives were calculated for scale variables

DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=Age PER1 PER2 PER3 RUM1 RUM2 RUM3 EX1 EX2 EX3 EX4
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS..

*Frequencies were calcuated for nominal variable (gender)

FREQUENCIES VARIABLES=GENDER
  /ORDER=ANALYSIS.

*Missing Data

MVA VARIABLES=Age PER1 PER2 PER3 RUM1 RUM2 RUM3 EX1 EX2 EX3 EX4 GENDER
  /MAXCAT=25
  /CATEGORICAL=GENDER
  /TTEST NOPROB PERCENT = 5
  /CROSSTAB PERCENT = 5
  /DPATTERN
  /MPATTERN
  /TPATTERN PERCENT=1
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=25).

*No variables with less than 5% or more missing values. Little's MCAR test not significant - it can therefore be inferred that data is missing completely at random.

*Values will be imputed and then tests for outliers will be undertaken. As outliers are likely to be extreme but probable, imputation will be carried out before dealing with outliers.

*Expectation Maximization

MVA VARIABLES=Age PER1 PER2 PER3 RUM1 RUM2 RUM3 EX1 EX2 EX3 EX4 GENDER
  /MPATTERN
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=100 OUTFILE='/Users/MaddyMillar/SPSSFiles/file4.sav').
EXECUTE.

*In new file, rename each variable so that when the files are merged, they are different variables.

RENAME VARIABLES (Age = Age_EM).
RENAME VARIABLES (PER1 = PER1_EM).
RENAME VARIABLES (PER2 = PER2_EM).
RENAME VARIABLES (PER3 = PER3_EM).
RENAME VARIABLES (RUM1 = RUM1_EM).
RENAME VARIABLES (RUM2 = RUM2_EM).
RENAME VARIABLES (RUM3 = RUM3_EM).
RENAME VARIABLES (EX1 = EX1_EM).
RENAME VARIABLES (EX2 = EX2_EM).
RENAME VARIABLES (EX3 = EX3_EM).
RENAME VARIABLES (EX4 = EX4_EM).
RENAME VARIABLES (Gender = Gender_EM).

*Merge files

MATCH FILES /FILE=*
  /FILE='DataSet2'
  /BY Num.
EXECUTE.

*Check new variables for missing data

MVA VARIABLES=Age_EM PER1_EM PER2_EM PER3_EM RUM1_EM RUM2_EM RUM3_EM EX1_EM EX2_EM EX3_EM EX4_EM 
  /MAXCAT=25
  /CATEGORICAL=Gender_EM.


*In order to deal with outliers, Inter-Quartile Range (IQR) used.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = PER1_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: PER1_EM=col(source(s), name("PER1_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("PER1_EM"))
ELEMENT: schema(position(bin.quantile.letter(PER1_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = PER2_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: PER2_EM=col(source(s), name("PER2_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("PER2_EM"))
ELEMENT: schema(position(bin.quantile.letter(PER2_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = PER3_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: PER3_EM=col(source(s), name("PER3_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("PER3_EM"))
ELEMENT: schema(position(bin.quantile.letter(PER3_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = RUM1_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: RUM1_EM=col(source(s), name("RUM1_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("RUM1_EM"))
ELEMENT: schema(position(bin.quantile.letter(RUM1_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = RUM2_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: RUM2_EM=col(source(s), name("RUM2_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("RUM2_EM"))
ELEMENT: schema(position(bin.quantile.letter(RUM2_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = RUM3_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: RUM3_EM=col(source(s), name("RUM3_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("RUM3_EM"))
ELEMENT: schema(position(bin.quantile.letter(RUM3_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = EX1_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: EX1_EM =col(source(s), name("EX1_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("EX1_EM"))
ELEMENT: schema(position(bin.quantile.letter(EX1_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = EX2_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: EX2_EM =col(source(s), name("EX2_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("EX2_EM"))
ELEMENT: schema(position(bin.quantile.letter(EX2_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = EX3_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: EX3_EM =col(source(s), name("EX3_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("EX3_EM"))
ELEMENT: schema(position(bin.quantile.letter(EX3_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = EX4_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: EX4_EM =col(source(s), name("EX4_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("EX4_EM"))
ELEMENT: schema(position(bin.quantile.letter(EX4_EM)), label(id))
END GPL.

GGRAPH
/GRAPHDATASET NAME= "graphdataset" VARIABLES = AGE_EM MISSING = LISTWISE REPORTMISSING =NO
/GRAPHSPEC SOURCE = INLINE.
BEGIN GPL
SOURCE: s=userSource (id("graphdataset"))
DATA: AGE_EM =col(source(s), name("AGE_EM"))
DATA: id=col(source(s), name("$CASENUM"), unit.category())
COORD: rect(dim(1), transpose())
GUIDE: axis(dim(1), label("AGE_EM"))
ELEMENT: schema(position(bin.quantile.letter(AGE_EM)), label(id))
END GPL.

*Multivariate Outliers (using Malhalanobis Distance)

REGRESSION
/MISSING LISTWISE
/STATISTICS COEFF OUTS R ANOVA
/CRITERIA = PIN(.05) POUT (.10)
/NOORIGIN
/DEPENDENT Num 
/METHOD = ENTER Age_EM PER1_EM PER2_EM PER3_EM RUM1_EM RUM2_EM RUM3_EM EX1_EM EX2_EM EX3_EM EX4_EM GENDER_EM
/SAVE MAHAL

COMPUTE PROB_MAH = 1-CDF.CHISQ(MAH_1,12).
EXECUTE

*based on this, there are two values which are significant at the  <.001 level (Tabachnick & Fidell, 2007). 

*In order to detect the influence of these values, a sensitivity analysis will be conducted. The analysis will firstly be conducted with the outliers included in the dataset.

*Transforming Variables

COMPUTE Perfectionism =MEAN(PER1_EM, PER2_EM, PER3_EM).
EXECUTE.

COMPUTE Rumination = MEAN(RUM1_EM, RUM2_EM, RUM3_EM).
EXECUTE.

COMPUTE Exhaustion = MEAN(EX1_EM, EX2_EM, EX3_EM, EX4_EM).
EXECUTE.

*Assumptions of Specific Maximum Liklihood Estimation

* Firstly, for multivariate normality:

GRAPH
  /SCATTERPLOT(MATRIX)=PER1_EM PER2_EM PER3_EM RUM1_EM RUM2_EM RUM3_EM EX1_EM EX2_EM EX3_EM EX4_EM
  /MISSING=LISTWISE.

*Now the main analysis can be conducted.

*Sensitivity Analysis
*Outliers will be removed using listwise deletion. 
USE ALL.

*determine median and IQR for the only variable with extreme outliers (RUM1_EM)

FREQUENCIES VARIABLES=RUM1_EM
/NTILES=4  
/ORDER=ANALYSIS.

*Median: 2, IQR = 3-2 = 1

COMPUTE filter_$=(RANGE (RUM1_EM, 2 -3*1, 3 + 3*1) AND PROB_MAH  >= 0.00100).
VARIABLE LABELS filter_$ 'RANGE (RUM1_EM, 2 -3*1, 3 + 3*1) AND PROB_MAH  >= 0.00100'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

DESCRIPTIVE RUM1_EM/STATISTICS=MIN MAX.
EXECUTE.

FREQUENCIES VARIABLES=Num
  /ORDER=ANALYSIS.

*Now we are left with 209 cases.

*New variables can now be transformed to reflect the exclusion of outliers.

COMPUTE Perfectionism_SA =MEAN(PER1_EM, PER2_EM, PER3_EM).
EXECUTE.

COMPUTE Rumination_SA = MEAN(RUM1_EM, RUM2_EM, RUM3_EM).
EXECUTE.

COMPUTE Exhaustion_SA = MEAN(EX1_EM, EX2_EM, EX3_EM, EX4_EM).
EXECUTE.
