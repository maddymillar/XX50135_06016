* Encoding: UTF-8.
*Accuracy of the Data
*Increase decimal points for all variables to check - it appears that age variable was mean centered as some age cases are 23.571.
*Set all missing values to 999 in variable view.

*Descriptives were calculated for scale variables

DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=AGE NegEmot1 NegEmot2 NegEmot3 Egal1 Egal2 Egal3 Ind1 Ind2 Ind3 GovSupport1 
    GovSupport2 GovSupport3
  /STATISTICS=MEAN STDDEV MIN MAX.

*Ind3 has a maximum value that does not conform to the appropriate range (22) - this was removed and replaced with a missing data point (999) as cannot be certain how data was entered by participants. Therefore, difficult
to know whether it was a participant error.

*Frequencies were calcuated for nominal variable (gender)

FREQUENCIES VARIABLES=GENDER
  /ORDER=ANALYSIS.

*Missing Data

MVA VARIABLES=AGE NegEmot1 NegEmot2 NegEmot3 Egal1 Egal2 Egal3 Ind1 Ind2 Ind3 GovSupport1 
    GovSupport2 GovSupport3 GENDER
  /MAXCAT=25
  /CATEGORICAL=GENDER
  /TTEST NOPROB PERCENT = 5
  /CROSSTAB PERCENT = 5
  /DPATTERN
  /MPATTERN
  /TPATTERN PERCENT=1
  /EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=25).

*No variables with less than 5% or more missing values. Little's MCAR test not significant - it can therefore be inferred that data is missing completely at random.

*Listwise deletion for missing data
*Filter variable created (Missing_Data), 0 = cases with missing data, 1= complete cases.

COMPUTE Missing_Data = nmiss(AGE to GovSupport3).
EXECUTE.
RECODE Missing_Data (1=0) (0=1).
ADD VALUE LABELS Missing_Data 1 'Complete Cases' 0 'Missing Data'.
EXECUTE.

USE ALL.
FILTER BY Missing_Data.
EXECUTE.

*Outliers
*Univariate outliers using z-scores (therefore, depends on the assumption of skewness and kurtosis to indicate the distribution of the data)

DESCRIPTIVES VARIABLES=AGE NegEmot1 NegEmot2 NegEmot3 Egal1 Egal2 Egal3 Ind1 Ind2 Ind3 GovSupport1 
    GovSupport2 GovSupport3
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

*According to Kim et al. (2013), values above 2 for skewness and values above 7 for kurtosis indicate an abnormal distribution. 

*As the distribution is normal, z-scores can be calculated.

DESCRIPTIVES VARIABLES=AGE NegEmot1 NegEmot2 NegEmot3 Egal1 Egal2 Egal3 Ind1 Ind2 Ind3 GovSupport1 
    GovSupport2 GovSupport3
  /SAVE
  /STATISTICS=MEAN STDDEV MIN MAX.

*Transform z-scores into absolute values to see positive and negative values calculated as outliers where necessary.

COMPUTE ZAGE=abs(ZAGE).
EXECUTE.

COMPUTE ZNegEmot1=abs(ZNegEmot1).
EXECUTE.

COMPUTE ZNegEmot2=abs(ZNegEmot2).
EXECUTE.

COMPUTE ZNegEmot3=abs(ZNegEmot3).
EXECUTE.

COMPUTE ZEgal1=abs(ZEgal1).
EXECUTE.

COMPUTE ZEgal2=abs(ZEgal2).
EXECUTE.

COMPUTE ZEgal3=abs(ZEgal3).
EXECUTE.

COMPUTE ZInd1=abs(ZInd1).
EXECUTE.

COMPUTE ZInd2=abs(ZInd2).
EXECUTE.

COMPUTE ZInd3=abs(ZInd3).
EXECUTE.

COMPUTE ZGovSupport1=abs(ZGovSupport1).
EXECUTE.

COMPUTE ZGovSupport2=abs(ZGovSupport2).
EXECUTE.

COMPUTE ZGovSupport3=abs(ZGovSupport3).
EXECUTE.

* According to Tabachnick & Fidell (2007), z scores > 3.29. Therefore, z-scores were recoded. They were also recoded according to Field (2007) to also reflect probable, possible and normal cases. 
*Label extreme outliers as 1, probable outliers as 2, potential outliers as 3 and normal range as 4.

RECODE ZAge
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Age.
EXECUTE.
ADD VALUE LABELS OUTLIER_Age 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZNEGEMOT1 
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_NegEmot1.
EXECUTE.
ADD VALUE LABELS OUTLIER_NegEmot1 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZNEGEMOT2 
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_NegEmot2.
EXECUTE.
ADD VALUE LABELS OUTLIER_NegEmot2 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZNEGEMOT3 
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_NegEmot3.
EXECUTE.
ADD VALUE LABELS OUTLIER_NegEmot3 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZEgal1
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Egal1.
EXECUTE.
ADD VALUE LABELS OUTLIER_Egal1 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZEgal2
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Egal2.
EXECUTE.
ADD VALUE LABELS OUTLIER_Egal2 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZEgal3
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Egal3.
EXECUTE.
ADD VALUE LABELS OUTLIER_Egal3 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZInd1
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Ind1.
EXECUTE.
ADD VALUE LABELS OUTLIER_Ind1 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZInd2
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Ind2.
EXECUTE.
ADD VALUE LABELS OUTLIER_Ind2 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZInd3
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_Ind3.
EXECUTE.
ADD VALUE LABELS OUTLIER_Ind3 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZGovSupport1
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_GovSupport1.
EXECUTE.
ADD VALUE LABELS OUTLIER_GovSupport1 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZGovSupport2
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_GovSupport2.
EXECUTE.
ADD VALUE LABELS OUTLIER_GovSupport2 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

RECODE ZGovSupport3
(3.29 thru highest =1) (2.58 thru highest =2) (1.96 through highest=3) (lowest thru 1.95 = 4) INTO OUTLIER_GovSupport3.
EXECUTE.
ADD VALUE LABELS OUTLIER_GovSupport3 1 'Extreme Outlier' 2 'Probable Outlier' 3 'Potential Outlier' 4 'Normal Range'.
EXECUTE.

*Use frequency tables to determine how many outliers there are.

FREQUENCIES VARIABLES=OUTLIER_Age OUTLIER_NegEmot1 OUTLIER_NegEmot2 OUTLIER_NegEmot3 OUTLIER_Egal1 
    OUTLIER_Egal2 OUTLIER_Egal3 OUTLIER_Ind1 OUTLIER_Ind2 OUTLIER_Ind3 OUTLIER_GovSupport1 
    OUTLIER_GovSupport2 OUTLIER_GovSupport3
  /ORDER=VARIABLE.

*In a normal distribution we’d expect about 5% to be greater than 1.96,1% to have absolute values greater than 2.58 and none to be greater than about 3.29. Subseqeuntly, as these values have been violated in: 
NegEmot2, NegEmot3, Egal3. Ind1, GovSupport1. Therefore, a sensitivity analysis will be conducted.

*In terms of age, however, there are 5 extreme outliers and 3 probable outliers. However, as these still fall within the acceptable range of the sample (students), these have been left in the dataset, rather than excluded.

*Multivariate Outliers (using Malhalanobis Distance)

REGRESSION
/MISSING LISTWISE
/STATISTICS COEFF OUTS R ANOVA
/CRITERIA = PIN(.05) POUT (.10)
/NOORIGIN
/DEPENDENT Num 
/METHOD = ENTER NegEmot1 NegEmot2 NegEmot3 Egal1 Egal2 Egal3 Ind1 Ind2 Ind3 GovSupport1 GovSupport2 GovSupport3 AGE GENDER
/SAVE MAHAL

COMPUTE PROB_MAH = 1-CDF.CHISQ(MAH_1,14).
EXECUTE

*based on this, there is only one value which are significant at the  .001 level (Tabachnick & Fidell, 2007). This value was subsequently added to the sensitivity analysis.

*Transforming Variables

*Create the mean of the sub-variables to create a transformed variable.

COMPUTE GovSupport=MEAN(GovSupport1,GovSupport2,GovSupport3).
EXECUTE.

COMPUTE NegEmot = MEAN(NegEmot1,NegEmot2,NegEmot3).
EXECUTE.

COMPUTE Egalitarianism = MEAN(Egal1, Egal2, Egal3).
EXECUTE.

COMPUTE Individualism = MEAN(Ind1, Ind2, Ind3).
EXECUTE.

*Question (1)

*Regression Assumptions

*1. Normality - although not an assumption of a regression, useful to know.

DESCRIPTIVES VARIABLES=AGE GENDER GovSupport NegEmot Egalitarianism Individualism
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

* skewness should be below 2, kurtosis should be below 7. In this case, all variables fall below these respective values. 

*2. Linearity and Additivity

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=NegEmot GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: NegEmot=col(source(s), name("NegEmot"))
  DATA: GovSupport=col(source(s), name("GovSupport"))
  GUIDE: axis(dim(1), label("NegEmot"))
  GUIDE: axis(dim(2), label("GovSupport"))
  ELEMENT: point(position(NegEmot*GovSupport))
  ELEMENT: line(position(smooth.linear(NegEmot*GovSupport)))
END GPL.

*It is a linear relationship.

*3. Independent Errors
Durbin Watson test statistic is 1.821 which is within acceptable range

*4. Homoscedasticity
*5. Normally Distributed Errors

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

EXECUTE. 

*Now that the assumption tests have been run and met, we can run the regression.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

*Question (3a)

*Regression assumptions

*1. Normality - although not an assumption of a regression, useful to know.

DESCRIPTIVES VARIABLES=AGE GENDER GovSupport NegEmot Egalitarianism Individualism
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

* skewness should be below 2, kurtosis should be below 7. In this case, all variables fall below these respective values. 

*2. Linearity and Additivity

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=NegEmot GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: NegEmot=col(source(s), name("NegEmot"))
  DATA: GovSupport=col(source(s), name("GovSupport"))
  GUIDE: axis(dim(1), label("NegEmot"))
  GUIDE: axis(dim(2), label("GovSupport"))
  ELEMENT: point(position(NegEmot*GovSupport))
  ELEMENT: line(position(smooth.linear(NegEmot*GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Egalitarianism GovSupport MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: Egalitarianism=col(source(s), name("Egalitarianism"))
  DATA: GovSupport=col(source(s), name("GovSupport"))
  GUIDE: axis(dim(1), label("Egalitarianism"))
  GUIDE: axis(dim(2), label("GovSupport"))
  ELEMENT: point(position(Egalitarianism*GovSupport))
  ELEMENT: line(position(smooth.linear(Egalitarianism*GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Individualism GovSupport MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: Individualism=col(source(s), name("Individualism"))
  DATA: GovSupport=col(source(s), name("GovSupport"))
  GUIDE: axis(dim(1), label("Individualism"))
  GUIDE: axis(dim(2), label("GovSupport"))
  ELEMENT: point(position(Individualism*GovSupport))
  ELEMENT: line(position(smooth.linear(Individualism*GovSupport)))
END GPL.

*It is a linear relationship for both NegEmot, Egalitarianism and Individualism.

*3. Independent Errors
Durbin Watson test statistic is 1.848 which is within acceptable range

*4. Homoscedasticity
*5. Normally Distributed Errors

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot Egalitarianism Individualism
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

*6. Multicollinearity and Singularity

CORRELATIONS
  /VARIABLES=NegEmot Egalitarianism Individualism
  /PRINT=TWOTAIL NOSIG
  /MISSING=LISTWISE.

*no correlations between variables of above 0.8

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot Egalitarianism Individualism
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

*VIF<10 and Tolerance>0.2 so fine. 

*7. Influential Cases

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot Egalitarianism Individualism
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)
  /SAVE COOK.
 
*Now all assumption tests have been conducted and met, the regression will be run.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot Egalitarianism Individualism
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

EXECUTE.

*Question (3b)

*Regression assumptions

*1. Normality - although not an assumption of a regression, useful to know.

DESCRIPTIVES VARIABLES=AGE GENDER GovSupport NegEmot Egalitarianism Individualism
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

* skewness should be below 2, kurtosis should be below 7. In this case, all variables fall below these respective values. 

*2. Linearity and Additivity

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=NegEmot GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: NegEmot=col(source(s), name("NegEmot"))
  DATA: GovSupport=col(source(s), name("GovSupport"))
  GUIDE: axis(dim(1), label("NegEmot"))
  GUIDE: axis(dim(2), label("GovSupport"))
  ELEMENT: point(position(NegEmot*GovSupport))
  ELEMENT: line(position(smooth.linear(NegEmot*GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Egalitarianism GovSupport MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: Egalitarianism=col(source(s), name("Egalitarianism"))
  DATA: GovSupport=col(source(s), name("GovSupport"))
  GUIDE: axis(dim(1), label("Egalitarianism"))
  GUIDE: axis(dim(2), label("GovSupport"))
  ELEMENT: point(position(Egalitarianism*GovSupport))
  ELEMENT: line(position(smooth.linear(Egalitarianism*GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Individualism GovSupport MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: Individualism=col(source(s), name("Individualism"))
  DATA: GovSupport=col(source(s), name("GovSupport"))
  GUIDE: axis(dim(1), label("Individualism"))
  GUIDE: axis(dim(2), label("GovSupport"))
  ELEMENT: point(position(Individualism*GovSupport))
  ELEMENT: line(position(smooth.linear(Individualism*GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=AGE GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: AGE=col(source(s), name("AGE"))
  DATA: GovSupport=col(source(s), name("GovSupport"))
  GUIDE: axis(dim(1), label("AGE"))
  GUIDE: axis(dim(2), label("GovSupport"))
  ELEMENT: point(position(AGE*GovSupport))
  ELEMENT: line(position(smooth.linear(AGE*GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=GENDER GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: GENDER=col(source(s), name("GENDER"), unit.category())
  DATA: GovSupport=col(source(s), name("GovSupport"))
  GUIDE: axis(dim(1), label("GENDER"))
  GUIDE: axis(dim(2), label("GovSupport"))
  SCALE: cat(dim(1), include("1.00", "2.00"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: point(position(GENDER*GovSupport))
  ELEMENT: line(position(smooth.linear(GENDER*GovSupport)))
END GPL.

*It is a linear relationship for both NegEmot, Egalitarianism and Individualism, AGE and GENDER.

*3. Independent Errors
Durbin Watson test statistic is 2.165 which is within acceptable range

*4. Homoscedasticity
*5. Normally Distributed Errors

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot Egalitarianism Individualism AGE GENDER
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)

*6. Multicollinearity and Singularity

CORRELATIONS
  /VARIABLES=NegEmot Egalitarianism Individualism AGE GENDER
  /PRINT=TWOTAIL NOSIG
  /MISSING=LISTWISE.

*no correlations between variables of above 0.8

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot Egalitarianism Individualism AGE GENDER
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)

*VIF<10 and Tolerance>0.2 so fine. 

*7. Influential Cases

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot Egalitarianism Individualism AGE GENDER
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)
  /SAVE COOK.

EXECUTE.
 
*Now all assumption tests have been conducted and met, the regression will be run.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT GovSupport
  /METHOD=ENTER NegEmot Egalitarianism Individualism AGE GENDER
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)

EXECUTE.

*Sensitivity Analysis
*Filter outliers that do not conform to set standards of extreme, probable and potential outliers, as well as Mahalnobis distance.

USE ALL.
COMPUTE filter_$=(Missing_Data=1 AND OUTLIER_NegEmot2 >= 3 AND OUTLIER_NegEmot3 >= 3 AND 
    OUTLIER_Egal3 >= 3 AND OUTLIER_Ind1 >= 3 AND OUTLIER_GovSupport1 >= 3 AND PROB_MAH  >= 0.00100).
VARIABLE LABELS filter_$ 'Missing_Data=1 AND OUTLIER_NegEmot2 >= 3 AND OUTLIER_NegEmot3 >= 3 AND '+
    'OUTLIER_Egal3 >= 3 AND OUTLIER_Ind1 >= 3 AND OUTLIER_GovSupport1 >= 3 AND PROB_MAH  >= '+
    '0.00100 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=Num
  /ORDER=ANALYSIS.

*We are now left with 188 participants which is within the acceptable range for power.

*Transforming Variables

*Create the mean of the sub-variables to create a transformed variable, which does not include outliers, for the senstitivity analysis

COMPUTE SA_GovSupport=MEAN(GovSupport1,GovSupport2,GovSupport3).
EXECUTE.

COMPUTE SA_NegEmot = MEAN(NegEmot1,NegEmot2,NegEmot3).
EXECUTE.

COMPUTE SA_Egalitarianism = MEAN(Egal1, Egal2, Egal3).
EXECUTE.

COMPUTE SA_Individualism = MEAN(Ind1, Ind2, Ind3).
EXECUTE.

*Question (2) - Sensitivity Analysis

*Regression Assumptions

*1. Normality - although not an assumption of a regression, useful to know.

DESCRIPTIVES VARIABLES=AGE GENDER SA_GovSupport SA_NegEmot SA_Egalitarianism SA_Individualism
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

* skewness should be below 2, kurtosis should be below 7. In this case, all variables fall below these respective values. 

*2. Linearity and Additivity

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=SA_NegEmot SA_GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: SA_NegEmot=col(source(s), name("SA_NegEmot"))
  DATA: SA_GovSupport=col(source(s), name("SA_GovSupport"))
  GUIDE: axis(dim(1), label("SA_NegEmot"))
  GUIDE: axis(dim(2), label("SA_GovSupport"))
  ELEMENT: point(position(SA_NegEmot*SA_GovSupport))
  ELEMENT: line(position(smooth.linear(SA_NegEmot*SA_GovSupport)))
END GPL.

*It is a linear relationship.

*3. Independent Errors
Durbin Watson test statistic is 1.823 which is within acceptable range

*4. Homoscedasticity
*5. Normally Distributed Errors

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

EXECUTE. 

*Now that the assumption tests have been run and met, we can run the regression.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

EXECUTE.

*Question (3a)

*Regression assumptions

*1. Normality - although not an assumption of a regression, useful to know.

DESCRIPTIVES VARIABLES=AGE GENDER SA_GovSupport SA_NegEmot SA_Egalitarianism SA_Individualism
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

* skewness should be below 2, kurtosis should be below 7. In this case, all variables fall below these respective values. 

*2. Linearity and Additivity

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=SA_NegEmot SA_GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: SA_NegEmot=col(source(s), name("SA_NegEmot"))
  DATA: SA_GovSupport=col(source(s), name("SA_GovSupport"))
  GUIDE: axis(dim(1), label("SA_NegEmot"))
  GUIDE: axis(dim(2), label("SA_GovSupport"))
  ELEMENT: point(position(SA_NegEmot*SA_GovSupport))
  ELEMENT: line(position(smooth.linear(SA_NegEmot*SA_GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=SA_Egalitarianism SA_GovSupport MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: SA_Egalitarianism=col(source(s), name("SA_Egalitarianism"))
  DATA: SA_GovSupport=col(source(s), name("SA_GovSupport"))
  GUIDE: axis(dim(1), label("SA_Egalitarianism"))
  GUIDE: axis(dim(2), label("SA_GovSupport"))
  ELEMENT: point(position(SA_Egalitarianism*SA_GovSupport))
  ELEMENT: line(position(smooth.linear(SA_Egalitarianism*SA_GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=SA_Individualism SA_GovSupport MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: SA_Individualism=col(source(s), name("SA_Individualism"))
  DATA: SA_GovSupport=col(source(s), name("SA_GovSupport"))
  GUIDE: axis(dim(1), label("SA_Individualism"))
  GUIDE: axis(dim(2), label("SA_GovSupport"))
  ELEMENT: point(position(SA_Individualism*SA_GovSupport))
  ELEMENT: line(position(smooth.linear(SA_Individualism*SA_GovSupport)))
END GPL.

*It is a linear relationship for both SA_NegEmot, SA_Egalitarianism and SA_Individualism.

*3. Independent Errors
Durbin Watson test statistic is 1.847 which is within acceptable range

*4. Homoscedasticity
*5. Normally Distributed Errors

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot SA_Egalitarianism SA_Individualism
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

*6. Multicollinearity and Singularity

CORRELATIONS
  /VARIABLES=SA_NegEmot SA_Egalitarianism SA_Individualism
  /PRINT=TWOTAIL NOSIG
  /MISSING=LISTWISE.

*no correlations between variables of above 0.8

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot SA_Egalitarianism SA_Individualism
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

*VIF<10 and Tolerance>0.2 so fine. 

*7. Influential Cases

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot SA_Egalitarianism SA_Individualism
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)
  /SAVE COOK.
 
*Now all assumption tests have been conducted and met, the regression will be run.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot SA_Egalitarianism SA_Individualism
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

EXECUTE.

*Question (3b)

*Regression assumptions

*1. Normality - although not an assumption of a regression, useful to know.

DESCRIPTIVES VARIABLES=AGE GENDER SA_GovSupport SA_NegEmot SA_Egalitarianism SA_Individualism
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

* skewness should be below 2, kurtosis should be below 7. In this case, all variables fall below these respective values. 

*2. Linearity and Additivity

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=SA_NegEmot SA_GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: SA_NegEmot=col(source(s), name("SA_NegEmot"))
  DATA: SA_GovSupport=col(source(s), name("SA_GovSupport"))
  GUIDE: axis(dim(1), label("SA_NegEmot"))
  GUIDE: axis(dim(2), label("SA_GovSupport"))
  ELEMENT: point(position(SA_NegEmot*SA_GovSupport))
  ELEMENT: line(position(smooth.linear(SA_NegEmot*SA_GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=SA_Egalitarianism SA_GovSupport MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: SA_Egalitarianism=col(source(s), name("SA_Egalitarianism"))
  DATA: SA_GovSupport=col(source(s), name("SA_GovSupport"))
  GUIDE: axis(dim(1), label("SA_Egalitarianism"))
  GUIDE: axis(dim(2), label("SA_GovSupport"))
  ELEMENT: point(position(SA_Egalitarianism*SA_GovSupport))
  ELEMENT: line(position(smooth.linear(SA_Egalitarianism*SA_GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=SA_Individualism SA_GovSupport MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: SA_Individualism=col(source(s), name("SA_Individualism"))
  DATA: SA_GovSupport=col(source(s), name("SA_GovSupport"))
  GUIDE: axis(dim(1), label("SA_Individualism"))
  GUIDE: axis(dim(2), label("SA_GovSupport"))
  ELEMENT: point(position(SA_Individualism*SA_GovSupport))
  ELEMENT: line(position(smooth.linear(SA_Individualism*SA_GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=AGE SA_GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: AGE=col(source(s), name("AGE"))
  DATA: SA_GovSupport=col(source(s), name("SA_GovSupport"))
  GUIDE: axis(dim(1), label("AGE"))
  GUIDE: axis(dim(2), label("SA_GovSupport"))
  ELEMENT: point(position(AGE*SA_GovSupport))
  ELEMENT: line(position(smooth.linear(AGE*SA_GovSupport)))
END GPL.

* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=GENDER SA_GovSupport MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: GENDER=col(source(s), name("GENDER"), unit.category())
  DATA: GovSupport=col(source(s), name("SA_GovSupport"))
  GUIDE: axis(dim(1), label("GENDER"))
  GUIDE: axis(dim(2), label("SA_GovSupport"))
  SCALE: cat(dim(1), include("1.00", "2.00"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: point(position(GENDER*SA_GovSupport))
  ELEMENT: line(position(smooth.linear(GENDER*SA_GovSupport)))
END GPL.

*It is a linear relationship for both SA_NegEmot, SA_Egalitarianism, SA_Individualism, AGE and GENDER.

*3. Independent Errors
Durbin Watson test statistic is 1.908 which is within acceptable range

*4. Homoscedasticity
*5. Normally Distributed Errors

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot SA_Egalitarianism SA_Individualism AGE GENDER
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)

*6. Multicollinearity and Singularity

CORRELATIONS
  /VARIABLES=SA_NegEmot SA_Egalitarianism SA_Individualism AGE GENDER
  /PRINT=TWOTAIL NOSIG
  /MISSING=LISTWISE.

*no correlations between variables of above 0.8

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot SA_Egalitarianism SA_Individualism AGE GENDER
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)

*VIF<10 and Tolerance>0.2 so fine. 

*7. Influential Cases

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot SA_Egalitarianism SA_Individualism AGE GENDER
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID)
  /SAVE COOK.

*Now all assumption tests have been conducted and met, the regression will be run.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT SA_GovSupport
  /METHOD=ENTER SA_NegEmot SA_Egalitarianism SA_Individualism AGE GENDER
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID) NORMPROB(ZRESID).

EXECUTE.

