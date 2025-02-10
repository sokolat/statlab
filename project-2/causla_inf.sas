
/* Code généré (IMPORT) */
/* Fichier source : addh.txt */
/* Chemin source : /home/u63768419/sasuser.v94 */
/* Code généré le : 06/02/2025 10:59 */

%web_drop_table(WORK.IMPORT);

/* %include '/home/u63768419/sasuser.v94/tablen_032020_pharmasug.sas' */
FILENAME REFFILE '/home/u63768419/sasuser.v94/addh.txt';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.IMPORT;
	DELIMITER='09'x;
	GETNAMES=YES;
	DATAROW=2;
RUN;

PROC CONTENTS DATA=WORK.IMPORT; RUN;


%web_open_table(WORK.IMPORT);

PROC SQL;
    SELECT 
        'feeling_depressed' AS variable, 
        SUM(feeling_depressed = "NA") AS missing_count, 
        (SUM(feeling_depressed = "NA") / COUNT(*)) * 100 AS missing_percentage
    FROM WORK.IMPORT

    UNION ALL

    SELECT 
        'smoking', 
        SUM(smoking = "NA"), 
        (SUM(smoking = "NA") / COUNT(*)) * 100 
    FROM WORK.IMPORT

    UNION ALL

    SELECT 
        'SES', 
        SUM(SES = "NA"), 
        (SUM(SES = "NA") / COUNT(*)) * 100 
    FROM WORK.IMPORT

    UNION ALL

    SELECT 
        'weight', 
        SUM(weight = "NA"), 
        (SUM(weight = "NA") / COUNT(*)) * 100 
    FROM WORK.IMPORT

    UNION ALL

    SELECT 
        'age', 
        SUM(age = "NA"), 
        (SUM(age = "NA") / COUNT(*)) * 100 
    FROM WORK.IMPORT

    UNION ALL

    SELECT 
        'sex', 
        SUM(sex = "NA"), 
        (SUM(sex = "NA") / COUNT(*)) * 100 
    FROM WORK.IMPORT;
QUIT;

PROC SQL NOPRINT;
    SELECT COUNT(*) INTO :total_rows FROM WORK.IMPORT;

    SELECT COUNT(*) INTO :rows_with_na
    FROM WORK.IMPORT
    WHERE feeling_depressed = "NA" 
       OR smoking = "NA" 
       OR SES = "NA" 
       OR weight = "NA" 
       OR age = "NA" 
       OR sex = "NA";
QUIT;

%LET total_rows = %SYSEVALF(&total_rows.);
%LET rows_with_na = %SYSEVALF(&rows_with_na.);

DATA missing_summary;
    percentage = (&rows_with_na. / &total_rows.) * 100;
    total_rows = &total_rows.;
    rows_with_na = &rows_with_na.;
RUN;

PROC REPORT DATA=missing_summary NOWD;
    COLUMNS total_rows rows_with_na percentage;
    DEFINE total_rows / "Total Rows";
    DEFINE rows_with_na / "Rows with NA";
    DEFINE percentage / "Percentage of Rows with NA" FORMAT=8.2;
RUN;


/*
PROC SQL;
    CREATE TABLE dataset AS
    SELECT INPUT(weight, best32.) AS weight,
        INPUT(age, best32.) AS age,
        INPUT(SES, best32.) AS SES,
        INPUT(smoking, best.) AS smoking,
        INPUT(sex, best. ) AS sex,
        INPUT(feeling_depressed, best.) AS feeling_depressed
    FROM WORK.IMPORT
    WHERE feeling_depressed NE "NA"
      AND smoking NE "NA"
      AND SES NE "NA"
      AND weight NE "NA"
      AND age NE "NA"
      AND sex NE "NA";
QUIT;
*/

PROC CONTENTS DATA=dataset;
RUN;

/*
%TABLEN(data=dataset, by=smoking , 
     var=age sex feeling_depressed smoking SES weight,
     type=1 2 2 2 1 1, outdoc=~/table1.rtf);
*/
PROC SQL NOPRINT;
    SELECT count(*) INTO :n FROM dataset WHERE smoking = 1;
QUIT;

/* Régression logistique avec output des coefficients et output des probabilités estimées   */



PROC LOGISTIC DATA=final OUTEST=betas_logist;

CLASS drug male(PARAM=ref REF='0') LDD(PARAM=ref REF='0') PSY(PARAM=ref REF='0');

      MODEL   drug(REG='FLUOXETINE') = age male cal_year imd LDD PSY BZDOTHER;

OUTPUT OUT=new P=pred;

run;








