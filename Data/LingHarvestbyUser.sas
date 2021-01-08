*this code queries interview data and provides total lingcod kept by port, year, and user;
* Modifed by AMR 12/17/20 for lingcode report.  Create xls file to send to R.
* orginal found here O:\DSF\GOAB\Ling_Reports\LingReport_03-14\Harvest_And_Effort_Distribution\Harvest

LIBNAME SASDATA 'O:\DSF\GOAB\SASDATA\Intervw';
TITLE ' ';
ods html close; ods html; run;

DATA LCKEPT0317;
	SET sasdata.int9204 sasdata.int05 sasdata.int06 sasdata.int07 sasdata.int08
		sasdata.int09 sasdata.int10 sasdata.int11 sasdata.int12 sasdata.int13
		sasdata.int14 sasdata.int15 sasdata.int16 sasdata.int17;
	if year = 2017;
	IF PORT ne 'CCI';
	IF USER = '' THEN USER = 'Unknown';
	IF ADFGSTAT = . THEN DELETE;

PROC SORT DATA = LCKEPT0317;
	BY USER PORT YEAR ADFGSTAT;

PROC MEANS DATA = LCKEPT0317 NOPRINT;
	BY USER PORT YEAR ADFGSTAT;
	OUTPUT OUT = CRAP SUM (LCKEPT)=LCKEPT;

PROC MEANS DATA = LCKEPT0317 NOPRINT;
	BY USER PORT YEAR;
	OUTPUT OUT = TOT SUM (LCKEPT) = TOTLC;

DATA CRAP2;
	MERGE CRAP TOT;
	BY USER PORT YEAR;
	DROP _TYPE_ _FREQ_;
	P = LCKEPT/TOTLC;
	SE = SQRT((P*(1-P))/(TOTLC-1));

PROC SORT DATA = CRAP2;
	BY PORT USER ADFGSTAT YEAR ASCENDING;

PROC PRINT DATA = CRAP2;
	OPTIONS LINESIZE = 102 PAGENO=1 NODATE;
	FORMAT P 4.3 SE 4.3;
	TITLE 'Lingcod kept from interviews by port, year and user 2003-2016';

RUN;


proc export data = CRAP2 outfile = 'O:\DSF\GOAB\Ling_Reports\LingReport_03-14\Harvest_And_Effort_Distribution\Harvest\2017.xls'
dbms = xls;
run;

libname sasdata 'O:\DSF\GOAB\MAPS\GIS'; * Saves the crap2 dataset to folder so that it can be merged with mapping data in PWS_charter_lc_maps file;
	data sasdata.lcharvestbystat0317;
	set crap2;
run;

	
