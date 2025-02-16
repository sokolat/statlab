
%macro bootstrap_macro ;

/* Initialize the empty dataset before running the macro */
data bootstrap_results;
    length iteration 8 bootstrap_ate_estimate 8;
run;

%do i=1 %to 500;

proc surveyselect data=dataset  method=urs n=4777
                  out=SampleSRS;
run;
  
data bootstrap_sample;
 set SampleSRS;
run;

proc logistic data=bootstrap_sample outest=betas_logist;
	class smoking sex(param=ref ref='2') feeling_depressed;
	model smoking(ref='0') = age sex feeling_depressed SES;
	output out=bootstrap_sample p=pred;
run;

data bootstrap_ipt_weights;
    set bootstrap_sample;
    if smoking = 1 then iptw = 1 / pred;
    else iptw = 1 / (1 - pred);
run;

proc means noprint data=bootstrap_ipt_weights mean;
    var weight;
    weight iptw;
    class smoking;
    output out=bootstrap_means mean=bootstrap_mean;
run;

data _null_;
    set bootstrap_means;
    if smoking = 1 then call symputx("bootstrap_treated_mean", bootstrap_mean, "L");
    if smoking = 0 then call symputx("bootstrap_control_mean", bootstrap_mean, "L");
run;

%let bootstrap_ate_estimate = %sysevalf(&bootstrap_treated_mean - &bootstrap_control_mean);

data single_result;
    iteration = &i;
    bootstrap_ate_estimate = &bootstrap_ate_estimate;
   	output;
run;

proc append base=bootstrap_results data=single_result force;
run;

%end;

%mend;

%bootstrap_macro;