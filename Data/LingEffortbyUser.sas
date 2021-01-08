****** Distribution of lingcod effort by stat area*****;
* Modified by MDS 1/2/18 in order to use data from 03-16 for lingcod report. Will use this file to map lc fishing effort
by port and user group from 2003-2016.;
* Modifed AMR 12/17/20 for same report. Create xls file to send to R.
* orginal found here O:\DSF\GOAB\Ling_Reports\LingReport_03-14\Harvest_And_Effort_Distribution\Effort

LIBNAME SASDATA 'O:\DSF\GOAB\SASDATA\Intervw';
TITLE ' ';
ods html close; ods html; run;

* First narrow the data to include only records with stat areas and lingcod effort;
DATA LCEFFORT0317;
	SET sasdata.int9204 sasdata.int05 sasdata.int06 sasdata.int07 sasdata.int08
		sasdata.int09 sasdata.int10 sasdata.int11 sasdata.int12 sasdata.int13
		sasdata.int14 sasdata.int15 sasdata.int16 sasdata.int17;
	if port ne 'CCI';
	if month ge 7;   *exclude data prior to July because fishery closed - can't be effort for lingcod;
	if ADFGStat = . THEN DELETE;
	if User = 'Unknown' or User = '        ' THEN DELETE;
	*use one or the other of the following lines;
	if Target = 'L' OR Target = 'B' OR Target = 'B+S';
	*if Target = 'L'; *alternative;

* First get the total number of angler-days for trips with stat area provided;
proc sort data=LCEFFORT0317;
  by Port Year User ADFGStat;
PROC MEANS SUM DATA=LCEFFORT0317 NOPRINT;
  BY Port Year User;
  OUTPUT OUT=TAngl SUM(ANGLDAYS)=TotADays;

*Now get total angl-days by stat area and divide to get proportions;
PROC MEANS SUM DATA=LCEFFORT0317 NOPRINT;
  BY PORT YEAR USER ADFGSTAT;
  OUTPUT OUT=ADBYSTAT SUM(ANGLDAYS)=ADAYS;

DATA ADBYSTAT;
  MERGE ADBYSTAT TANGL;
	BY PORT YEAR USER;
	DROP _TYPE_ _FREQ_;
	P = ADAYS/TOTADAYS;
	SEP = SQRT(P*(1-P)/(TOTADAYS-1));

PROC SORT DATA = ADBYSTAT;
	BY PORT YEAR USER ADFGSTAT;

PROC PRINT DATA=ADBYSTAT;
  TITLE 'Proportions of lingcod effort (angl-days) by ADFG stat area';
	FORMAT P 5.3 SEP 5.3;
RUN;

proc export data = adbystat outfile = 'H:\My Documents\Southcentral halibut and groundfish\Lingcod report_03-17\Ling_effort.xls'
dbms = xls replace;
run;
