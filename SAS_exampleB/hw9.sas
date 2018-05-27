
*PART A;

*getting ready!;
**************************************************************************************************************************************;
libname practice "E:\SAS_Class\Practice_9";
run;
* PART A) Import the dataset, name is 'CHD' and include formats and labels below in DATA step ;
proc import out=CHD
			datafile="E:\SAS_Class\Practice_9\chd.xlsx" 
			dbms=xlsx replace;
			getnames=yes;
run;
*Creating formats;
PROC FORMAT; *Changing formats;
    VALUE CHDfmt
      0 = "Control"
      1 = "Case";
RUN;

DATA CHD; 
SET CHD; *re-saving data with these labels;
FORMAT CHD CHDfmt.;
	LABEL
		SBP = 'Systolic Blood Pressure'
		LDL = 'Low density lipoprotein cholesterol'
		BAI = 'Body adiposity index'
		Famhist = 'Family history of heart disease'; *(Present, Absent);
		BMI = 'Body Mass Index'
		CHD = 'Coronary heart disease'; *(1 = Case / 0 = Control);
	run;

ods rtf file = "E:\SAS_Class\Practice_9\fta2112_hw9_output.rtf"
		startpage = yes; *Do this because you want a rtf output to convert to pdf later;

*and print out first 5 observations of dataset with labels;
PROC PRINT data = CHD (obs=5)LABEL; 
run;


*PART B*                                        ;
*i. Cross-tabular frequency family history (rows) and CHD status (columns);

PROC TABULATE data = CHD;
class Famhist CHD;
table Famhist ALL, CHD ALL; 
keylabel All = "Total"; run; 

*OR I find this way better and mroe conventional-looking than PROC TABULATE for this example;
PROC FREQ data = CHD;
	TABLES famhist * CHD / nocol norow nopercent;run;

*Our data consists of 462 individuals: 302 controls (denoted as "0", persons without CHD) and 160 cases (denoted as "1", persons with CHD);



*ii. Distribution of systolic pressure;

*1) Descriptive statistics (n, mean, median, standard deviation, min, max) of systolic pressure for each level of CHD status. Use two decimal points;
PROC MEANS data = CHD n mean median std min max range MAXDEC = 2;
  CLASS CHD;
  VAR SBP;  
RUN;

*For individuals with and without CHD, the range of values for systolic blood pressure is quite similar. However, individuals with CHD do have a higher mean
SBP, as well are more variability in their SBP. 


*2)Boxplots of systolic blood pressure for each level of CHD status;
PROC SGPLOT data = CHD;
vbox SBP/ category = CHD; 
xaxis label = "Coronary Heart Disease Status";
run;
*The CHD Cases have a higher median than the Controls, and a larger IQR, indicating a greater spread of their data. The IQR is not sensitive to outliers, 
and you see more outliers in the Control group. The outliers could also be affecting the means. Also, there are way more "Control" than "Case" observations,so that 
may be tempering differences.;


*3)Scatterplot and Pearson's correlation coefficient of SBP and tobacco for each level of CHD status;
PROC SORT data = CHD;   *first sort dataset;
BY CHD;
run;

PROC CORR data = CHD  plots=matrix; *Separate by levels of CHD status, also have scatter plots here;
var SBP Tobacco;
BY CHD;
run;

*By looking at the scatterplots, we can see that for those without Coronary Heart Disease, there is a weak linear association 
between tobacco usage and Systolic Blood Pressure. The r for the two variables under the controls is 0.18373, with a significant 
p-value of 0.0013 (our alpha is 0.05).

For the CHD cases, there is a weak linear association between tobacco and SBP, which is counter to what we know about tobacco and CHD risk. The r for those variables is 0.14876, 
but the p-value is not significant at 0.0605 (our alpha is 0.05). Interestingly, the association is smaller for Cases than it is for the Control. When you look at the boxplots, 
you see that the Control group has more high outliers, which affects the r;


*iii. Histograms of body adiposity index for those with and without family history, separately.
Overlay each histogram with normal density curve;
proc sgpanel data=CHD;
  panelby Famhist / rows=2;
  histogram BAI / scale = count;
  density BAI/type = kernel;
run;

*For those who do not have a family history of CHD, the data is not technically bimodal, but there is a HINT of bimodalism. There data for this group is normally distributed;
*For those who do have a family history of CHD, the data is normally distributed, with one mode, but there is a slight left skew;

****************************************************************************************************************************************************************;
*PART C*;
*Macro: Create a macro program that takes two numeric variables as inputs and produces a table with CHD status and family history;

%macro numsumtab (var1, var2);

PROC TABULATE data = CHD ;
	CLASS CHD Famhist ;
	VAR &var1 &var2;
	TABLE (CHD = "" ALL) * (Famhist = "" ALL), (&var1 &var2)*(n*f=3. mean*f=5.1 std*f=5.2) ;
	keylabel N = "Freq"
			 All = "Total"
		     mean = "Mean"
			 std = "Std Dev"; 
run;

%mend;

*examples with my new macro, ooh la la ;
%numsumtab (Alcohol, Tobacco); run;
%numsumtab (BAI, Alcohol); run;
%numsumtab (Typea, LDL); run;
%numsumtab (LDL, SBP); run;
%numsumtab (Age, BMI); run;
*****************************************************************************************************************************************************************;
*PART D;

*i. Is the CHD status independent of family history?;

		*For the hypotheses;

		*H0: CHD status is not associated with family history;
		*HA: CHD status is associated with family history;

*We have to conduct a Chi Squared test of independence;

proc freq data=CHD;
table CHD * Famhist / chisq nocol norow nopercent; 
run;

		*Since we have a chi-square p-value of <.0001 (way less than our alpha of 0.05) we reject the null hypothesis, and conclude that Coronary Heart Disease status 
		and Family history are associated with each other;


*ii. Is there a difference in mean type-A personality score depending on family history?

		*For your hypotheses;

		*H0: mu_nofamhist = mu_famhist
		*HA: mu_nofamhist =/= mu_famhist

	*We have to conduct an Independent 2-sample t-test (comparing means of two groups);

	*Check normality;
	proc univariate data=CHD normal;
		class famhist;
		var typea;
		histogram typea;
		qqplot typea;
	run;

*For Famhist = Absent: we reject the null that the data are normally distributed (Shapiro-Wilk p-value: 0.0175), but we fail to reject the same hypothesis 
for the Famhist =  Present data (Shapiro-Wilk p-value: 0.3693). So we must use a non-parametric test for all the data, the Wilcoxon Signed-Rank Test; 

		*So we change the hypotheses:

		*H0: median_nofamhist = median_famhist
		*HA: median_nofamhist =/= median_famhist ;

*Comparing medians from two independent samples;
proc npar1way data=CHD wilcoxon;
	class famhist;
	var typea;
run;

		*At an alpha of .05, our 2-sided p-value for the 2-sample Wilcoxon Ranked Sum Test is 0.4507.
		We therefore fail to reject the null hypothesis and conclude that the medians type-A score of the two groups are not significantly 
		different from each other;


*iii. Is the Pearson's correlation coefficient of alcohol and tobacco consumption equal to 0?
		
		*For the hypotheses;
		
		*H0: r = 0 (there is no linear correlation between alcohol consumption and tobacco consumption);
		*HA: r =/= 0 (there is some linear correlation between alcohol consumption and tobacco consumption);

PROC CORR data = CHD;  *sample size is 462;
var alcohol tobacco;
run;
		*Since the p-value in the correlation is <.0001, we can reject the null hypothesis that there is no correlation, and conclude that there is a weak positive 
		correlation (of 0.20081) between alcohol consumption and tobacco consumption;


*iv. Is the proportion of having family history greater than 40%? ;

* H0: p_yes = 0.4 (The proportion of having family history of CHD is equal to 40%)
* HA: p_yes > 0.4  (The proportion of having family history is greater than 40%);

proc sort data=CHD out=CHD_pro; by descending famhist; run;

proc freq data=CHD_pro order=data;
	table famhist / binomial (p=0.4) alpha =.05; 
run; 
*Output uses Famhist=Present as the default;

*41.52% of our sample has family history of CHD.
*Since the 1-sided p-value given is 0.2471 you fail to reject the H0, and conclude that the proportion of having family history of CHD is equal to 40%.


*****************************************************************************************************************************************************************;
*PART E;

*Not trying to slim down model, but want to look at all betas for covariates;
*Prefer this to procgenmod because it gives Hosmer Lemeshow;

*Full model with all the covariates;
proc logistic data=CHD descending ;
	class CHD (ref= "0") famhist (ref="Absent") /param=ref;
	model CHD = sbp tobacco ldl bai typea bmi alcohol age famhist / lackfit;


* Model selection: PROC LOGISTIC (could also use HPGENSELECT if so inclined. This gets rid of unnecessary covariates;
proc logistic data=CHD plots(only)=(roc effect) descending;
	class CHD(ref = "0") famhist(ref="Absent");
	model CHD =  sbp tobacco ldl bai typea bmi alcohol age famhist
			/ lackfit outroc = roc selection = stepwise ; * Model selection;
run; *selected Tobacco LDL TypeA Age Famhist(Present)



* Final model....using PROC LOGISTIC. Gives same results as Step Five of the previous model selection. The betas you need;
proc logistic data=CHD plots(only)=(roc effect) descending;
	class famhist(ref="Absent")/param = ref;
	model CHD (event="1") = tobacco ldl typea age famhist / lackfit outroc = roc;  *Need the ROC curve with calculated;
run;


*i. Overall significance;

	*H0: The reduced model is optimal for predicting our response variable;
	*HA: The fitted model is optimal for predicting our response variable;
	
	*We obtain a Likelihood Ratio p-value of <.0001 (alpha = 0.05) so we can reject the null hypothesis and conclude that our fitted model is better than at predicting probability 
	of having CHD;
	*Compared to the intercept-only null model, our fitted model, which has the predictor variables Tobacco consumption (in kg), LDL cholesterol, TypeA personality score, Age in years,
	and Family history (present or absent), is more useful in actually predicting the probability that an individual will have CHD.
	

*ii. ROC curve (if applicable);

	 *Area Under Curve (AUC) value is .7922;
	 *Sensitivity is the probability that the test will be positive if the person truly has has the disease.
	 *Sensitivity is P(T+ given that D+);
	 *Specificity is the probability that the test will be negative if the person truly does not have the disease.
	 *Specificity is P(T- given that D-);
	 *The probability that a randomly selected pair of subjects (one true positive, one true negative) will be correctly ordered by the test is .7922 or 79.22%;

*iii. Goodness-of-fit (if applicable);

	 *H0: This model fits our data well;
     *HA: This model does not fit our data well;

	 *At a significance level of 0.05, we receive a Hosmer and Lemeshow Goodness-of-Fit Chi-Sq p-value of 0.9922. So we fail to reject the null hypothesis
	  and conclude that the final model is a good fit for our data;


*iv. Estimated coefficients (Interpretation, significance);

	*Beta_hat(Intercept);
	*When all numerical covariates are 0, and all categorical covariates are absent, then odds of having CHD are  exp(-6.4464) = .00159;
	*This means for an individual who is 0 years of age, has tobacco use of 0 kilogram, has LDL of 0, TypeA score of 0, and no family history of CHD, the odds of them having CHD are 0.00159;


	*Beta_hat(Tobacco) =  0.0804;
	*For one kilogram increase in tobacco, the odds of having CHD increases by exp( 0.0804) = 1.084 times adjusted for LDL, TypeA, Age, and Famhist (put another way, holding all 
	other variables constant);


	*Beta_hat(LDL) = 0.1620;
	*For one unit increase in LDL cholesterol, the odds of  having CHD increases 
     by exp(0.1620) = 1.176 times adjusted for tobacco, TypeA, Age, and Famhistory (put another way, holding all other variables constant).

	
	*Beta_hat(TypeA) = 0.0371;
	*For one unit increase in Type A, the odds of having CHD increases by exp(0.0371) = 1.038 times adjusted for tobacco, LDL, Age, and Famhist 
	(put another way, holding all other variables constant);


	*Beta_hat(Famhist=Present) = 0.9082;
	*For those who have a Family history of CHD (Famhist = Present), the odds of having CHD is exp( 0.9082) = 2.480 times the odds of those who did not have a 
	family history (Famhist = Absent) adjusted for tobacco, LDL, TypeA, and Age (aka holding all other variables constant);


ods RTF close;	
