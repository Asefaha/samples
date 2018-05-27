
**********************************************************************************PROBLEM 1****************************************************;
libname practice "C:\Users\PubLibrary\Desktop\Practice_7";
run;
* PART A) Import the dataset and name it 'wage' ;
proc import out=practice.wage
			datafile="C:\Users\PubLibrary\Desktop\Practice_7\Wage.xlsx" 
			dbms=xlsx replace;
			getnames=yes;
run;

ods rtf file = "C:\Users\PubLibrary\Desktop\Practice_7\fta2112_hw7_output.rtf"
		startpage = yes;

proc print data = practice.wage (obs=5); run; *Checking it out;

*PART B) Descriptive statistics;

*i. Frequency table and bar chart of race;
PROC FREQ data = practice.wage;
TABLES race/ nocol norow nocum;
Title "Frequency Table of Participants' Race";
run;


PROC SGPLOT data = practice.wage;
vbar race; 
Title "Participants' Race";
*Here, you can see that the majority of participants are white, so we are nowhere near an equal ditribution when it comes to racial makeup.;


*ii. Distribution of age;
*1) Descriptive statistics;
PROC MEANS data = practice.wage n mean median std min max MAXDEC = 2 ;
  var  Age;
  TITLE "Summary Statistics of Age";
RUN;
 
*While the mean and the median are both at the age of 42, we do have a wide range of ages (18-80);

*2);
PROC SGPLOT data = practice.wage;
histogram age / binwidth = 10 scale = count; run;

*Here, the bar graph demonstrates slight right skew on age. This makes sense because labor information would not be collected on children;

*iii. Distribution of wage depending on education level;
*1);
PROC MEANS data = practice.wage n mean median std min max MAXDEC = 2 ;
  CLASS education;
  VAR wage;
  TITLE "Summary Statistics of Education Level";
RUN;

*In terms of education level, we have the most observations when it comes to people who have only have a high school diploma. The n is comparable between those with a college
degree and those with some college, but observations lessen when it comes to those with an advanced degree. However, we have the smallest n in terms of those with less than
a high school diploma;

*When it comes to wage, the mean, median, and standard deviation, and minimum, and maximum all increase as education does. But the spread is what is most interesting. 
To have the highest standard deviation for an advanced degree means that those individuals experiences the highest variability in their wages. So personally, it is not as predictable
that the higher education will necessarily give them the best ROI. For those with the least amount of education in the data, the standard deviation ensures a higher chance of a more 
predictable, albeit much lower, salary. And it should be noted that the best wage outcomes are still there for those with the advanced degrees.


*2);
PROC SGPLOT data = practice.wage;
vbox wage/ category = education; 
xaxis label = "Education Level";
Title "Wage by Education Level";
run;

*On the graph, you can see that there are highly paid individuals within most of the educational levels;

*PART C) Produce the following report;

proc report data=practice.wage nowindows; 
	column education N jobclass, (age, (mean std)  wage,(mean std));
	define education / group;
	define jobclass / across; 
	define age / format = 5.2;      
	define wage / format = 5.1;	*Need to change decimals for wage;	
	Title "Age and Wage by Education and Jobclass";
run;

*Even when separated by jobclass, the positive relationship between education and wage is also observed here. This goes for the standard deviation, of wage, which is also to be expected,
However, the mean ages and standard deviations per educational level were all very similar to each other.

*PART D) Produce the following table;
proc tabulate data = practice.wage;
	class marital jobclass education;
	table (jobclass="" All) * (education="" All), (marital="" All)*(n)/ box = "Job class and education level";
	keylabel N = "Freq"
			 All = "Total";
	Title "Frequency of Marital Status by Jobclass and Educational Level";
run;

*In each jobclass, the majority of participants were married (1000+ in each). For the other marital status, there was a similar distribution between informational and industrial, with the second
highest status being "Divorced" (about 100 or so for each). There were a few of the other statuses.



*PART E) Hypothesis Testing;
*i. Is the mean age different from 40?;
*1) H0: mu_age = 40;
*	HA: mu_age =/= 40;
*2) We must do a one-sample t-test. 
*3)Check normality first;
PROC UNIVARIATE data = practice.wage normal mu0=40;
	var age;
	histogram age;
	qqplot age;
run;

*Kolgomorov-Smirnoff p-value is less than .01, meaning we reject H0 that the distribution is normal. Must use a non-normal test;
*From the histogram, we can see that the distribution for age is right-skewed.
*For non-normal one-sample t-test, use one-sample Wilcoxon Ranked Sign Test;

*4) The p-value for the Signed Rank Test is less than .0001, meaning we reject the H0, and that the mean age is significantly 
different from 40;



*ii. Is there a difference in mean wage depending on education level?;
*1) H0: mu_LessThanHSgrad = mu_HSgrad = mu_SomeCollege = mu_CollegeGrad = mu_AdvDegree;
*	HA: At least one of the means are different;
*2) We must do an ANOVA (comparing means of more than two groups)
*3) Check normality first;
proc univariate data=practice.wage normal;
	class education;
	var wage;
	histogram wage;
	qqplot wage;
run;

* Shapiro-Wilk p-value;
* (LessThanHSgrad) 0.0008 , (HSgrad) <0.0001, (SomeCollege) <0.0001, (CollegeGrad) <0.0001, (AdvDegree) <0.0001;
* We can reject the H0 that all means are normal! Must use Kruskal Wallis Test instead of ANOVA;

proc npar1way data=practice.wage wilcoxon;
	class education;
	var wage;
run;

*4) Since the p-value is <.001, we can reject the H0 that all the median wages are the same. At least one of the medians are different;


*iii. Is the proportion of having health insurance less than 70%?;
*You must be careful because the output selects "NO Insurance" as the default, so your hypothesis is 1- 0.7 instead;
*You are checking if the proportion of people WITHOUT health insurance equals .3 or not.
* H0: p = .3 ;
* HA: p < .3;

proc freq data=practice.wage;
	table insurance / binomial (p=.3) alpha =.05; 
run;

*Since the p-value given is greater than 0.05, you fail to reject the H0 that the proportion of people without health insurance is 
30%. 
*Since the proportion of people WITHOUT health insurance is .3, therefore the proportion of people WITH health insurance is 
70%.


*iv. Among those who were never married, is job class independent of education level?;

*First save a new dataset only with non-married people;

DATA practice.Nevermarried;
SET practice.wage;
IF Marital = "Never Married"; run;


PROC PRINT data = practice.Nevermarried (obs=5); run; 

*For your hypotheses;
*H0: Among those who never married, jobclass is independent of education level;
*HA: Among those who never married, jobclass is not independent of education level;

*For two categorical variables, must use ;


proc freq data=practice.Nevermarried;
	table jobclass * education / chisq exact; 
run;

*Since the p-value is <.0001, you can reject the null hypothesis.; 
*Jobclass is not independent of education level, among those who never married;

ods RTF close;	
