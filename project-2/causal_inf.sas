/* Code généré (IMPORT) */
/* Fichier source : addh.txt */
/* Chemin source : /home/u63768419/sasuser.v94 */
/* Code généré le : 06/02/2025 10:59 */

%web_drop_table(work.import);

filename reffile '/home/u63768419/sasuser.v94/addh.txt';

proc import datafile=reffile
	dbms=dlm
	out=work.import;
	delimiter='09'x;
	getnames=yes;
	datarow=2;
run;

%web_open_table(work.import);

proc sql;
    select 
        'feeling_depressed' as variable, 
        sum(feeling_depressed = "NA") as missing_count, 
        (sum(feeling_depressed = "NA") / count(*)) * 100 as missing_percentage
    from work.import

    union all

    select 
        'smoking', 
        sum(smoking = "NA"), 
        (sum(smoking = "NA") / count(*)) * 100 
    from work.import

    union all

    select 
        'SES', 
        sum(SES = "NA"), 
        (sum(SES = "NA") / count(*)) * 100 
    from work.import

    union all

    select
        'weight', 
        sum(weight = "NA"), 
        (sum(weight = "NA") / count(*)) * 100 
    from work.import

    union all

    select 
        'age', 
        sum(age = "NA"), 
        (sum(age = "NA") / count(*)) * 100 
    from work.import

    union all

    select 
        'sex', 
        sum(sex = "NA"), 
        (sum(sex = "NA") / count(*)) * 100 
    from work.import;
quit;

proc sql noprint;
    select count(*) into :total_rows from work.import;

    select count(*) into :rows_with_na
    from work.import
    where feeling_depressed = "NA" 
       or smoking = "NA" 
       or SES = "NA" 
       or weight = "NA" 
       or age = "NA" 
       or sex = "NA";
quit;

%let total_rows = %sysevalf(&total_rows.);
%let rows_with_na = %sysevalf(&rows_with_na.);

data missing_summary;
    percentage = (&rows_with_na. / &total_rows.) * 100;
    total_rows = &total_rows.;
    rows_with_na = &rows_with_na.;
run;

proc report data=missing_summary nowd;
    columns total_rows rows_with_na percentage;
    define total_rows / "Total Rows";
    define rows_with_na / "Rows with NA";
    define percentage / "Percentage of Rows with NA" FORMAT=8.2;
RUN;


proc sql noprint;
    create table dataset as
    select input(weight, best32.) as weight,
        input(age, best32.) as age,
        input(SES, best32.) as SES,
        input(smoking, best.) as smoking,
        input(sex, best. ) as sex,
        input(feeling_depressed, best.) as feeling_depressed
    from work.import
    where feeling_depressed ne "NA"
      and smoking ne "NA"
      and SES ne "NA"
      and weight ne "NA"
      and age ne "NA"
      and sex ne "NA";
quit; 

proc contents data=dataset;
run;

proc sort data=dataset;
	by smoking;
run;

/* Régression logistique avec output des coefficients et output des probabilités estimées   */

proc logistic data=dataset outest=betas_logist;
	class smoking sex(param=ref ref='2') feeling_depressed;
	model smoking(ref='0') = age sex feeling_depressed SES;
	output out=new p=pred;
run;

data ipt_weights;
    set new;
    if smoking = 1 then iptw = 1 / pred;
    else iptw = 1 / (1 - pred);
run;

proc means data=ipt_weights;
	by smoking;
	var age SES;
run;

proc means data=ipt_weights;
	by smoking;
	var age SES;
	weight iptw;
run;

proc freq data=ipt_weights;
    by smoking;
    tables sex feeling_depressed;
run;

proc freq data=ipt_weights;
    by smoking;
    tables sex feeling_depressed;
    weight iptw;
run;

%bootstrap_macro;

















