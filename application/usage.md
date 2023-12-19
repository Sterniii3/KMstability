### input

1. Choose the .csv data set including individual observations of
<ul>
  <li>option 1: a <b>start and stop date variable</b>, indicating the start date of the observation period and the date of the last observation, <b>or</b></li>
  <li>option 2: a <b>time interval variable</b>, indicating the time interval of the observation period <b>and</b></li>
  <li>an <b>event variable</b>, indicating if an event (e.g. death; coded as 1) or censoring (coded as 0) took place </li>
  <li>in case of group comparison: an <b>group variable</b>, indicating the goup belonging 0 or 1 </li>
</ul>
Upload this .csv file to the R shiny application by browsing your directory.

2. Indicate whether you are using option 1 (start and stop date variable) or option 2 (time interval variable), compare step 1.

3. Specify the name of the start and the stop date variable or the time interval variable, as well as the event variable in your data set, respectively. 

4. Choose the number of groups in your data: 1 or 2. If 2, choose the name of the group variable in your data set.

5. Choose which figure format (png, jpeg or pdf) you wish.

6. Click on "Go" and wait for the results to appear in the "plots" and "derived metrics" tab, respectively.

### output

In the "plots" tab, the following figures are shown and can be downloaded by clicking on "save Figure"
<ul>
	<li> Figure 1: Kaplan-Meier estimate of survivor function for overall survival, with 95% confidence intervals and numbers at risk.</li>
	<li> Figure 2: Upper and lower limits of the stability interval (SIU, SIL) for the Kaplan–Meier estimate (KM) as proposed by Betensky (2015).</li>
	<li> Figure 3: Kaplan-Meier estimates of time to censoring, C, observation time, T, and time to censoring among those who are censored, C|C < X.</li>
	<li> Figure 4: Figure 4: Difference curve between upper and lower limits of the stability interval (SIU, SIL) and partial difference curves between Kaplan–Meier estimate and SIU and SIL, respectively.</li>
</ul>

In the "derived metrics" tab, the following tables are shown and can be downloaded by clicking on "save Table"
<ul>
<li> Pretty tables: </li>
	<ul>
	<li> Table 1a: Quantile summaries of KM estimate and proposed upper and lower bounds with associated 95% confidence intervals (lower CI, upper CI)</li> 
	<li> Table 2a: Quantile summaries of C, C|C < X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI)</li> 
	</ul>
<li> Tables for secondary analysis: </li>
	<ul>
<li> Table 1b: Quantile summaries of KM estimate and proposed upper and lower bounds with associated 95% confidence intervals (lower CI, upper CI)</li> 
<li> Table 2b: Quantile summaries of C, C|C < X and T=min(X, c) with associated 95% confidence intervals (lower CI, upper CI)</li> 
</ul>
</ul>