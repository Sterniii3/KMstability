### input

1. Choose the .csv data set including individual observations of
<ul>
  <li>option 1: a <b>start and stop date variable</b>, indicating the start date of the observation period and the date of the last observation, <b>or</b></li>
  <li>option 2: a <b>time interval variable</b>, indicating the time interval of the observation period <b>and</b></li>
  <li>an <b>event variable</b>, indicating if an event (e.g. death; coded as 1) or censoring (coded as 0) took place </li>
</ul>
by browsing your directory.

2. Select if you use option 1 or option 2.

3. Specify the name of the start and the stop date variable or the time interval variable, as well as the event variable in you data set, respectively. 

4. Choose which figure format (png, jpeg or pdf) you whish.

5. Click on "Go" and wait for the results to apear in the "plots" and "derived metrics" tab, respectively.

### output

In the "plots" tab, the following figures are shown and can be downloaded by clicking on "save Figure"
<ul>
	<li> Figure 1: Kaplan-Meier estimate of survivor function for overall survival, with 95% confidence intervals and numbers at risk.</li>
	<li> Figure 2: Upper and lower limits for Kaplan–Meier estimate as proposed by Betensky (2015).</li>
	<li> Figure 3: Kaplan-Meier estimates of time to censoring, C, observation time, T, and time to censoring among those who are censored, C|C < X.</li>
	<li> Figure 4: Difference curve between upper and lower limits of Kaplan–Meier and partial difference curves between Kaplan–Meier and upper and lower limits.</li>
</ul>

In the "derived metrics" tab, the following tables are shown and can be downloaded by clicking on "save Table"
<ul>
<li> Table 1: Quantile summaries of KM estimate and proposed upper and lower bounds with associated 95% confidence intervals (lower CI, upper CI)</li> 
<li> Table 2: Quantile summaries of C, C|C < X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI)</li> 
</ul>