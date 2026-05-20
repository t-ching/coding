***=====================================================================================;
***		(1) Variable pre-scanning														;
***			Input:																		;
***				dsin	input dataset													;
***				dsout	output dataset													;
***				ivcat	list of independent categorical variables (separate by space)	;
***				ivnum	list of independent numeric variables (separate by space)		;
***				cutlvl	# of maximum number of levels to keep a categorical variable	;
***				cutperc	exclude variable if > this cutoff assume 1 single value			;
***			Output:		1 table with variables to be excluded							;
***=====================================================================================;

%macro scanvar(dsin, dsout, ivcat, ivnum, cutlvl, cutperc);
	proc contents data=&dsin out=_tmp noprint; run;

	proc sql noprint;
		select count(*) into :n from &dsin;
		select max(length(name)) into :maxlen from _tmp;
		create table &dsout (variable varchar(&maxlen), lvlcnt int, lvlmaxp num);
	quit;

	%let k    = 1;
	%let expl = %scan(&ivcat, &k);
	%do %while(&expl ne);
		proc sql noprint;
			select count(*), max(p) format=6.2 into :_varlvlcnt, :_varlvlp from
			(select &expl, 100*count(*)/&n as p from &dsin group by 1);
		quit;
		%if &_varlvlcnt > &cutlvl or &_varlvlp > &cutperc %then %do;
			proc sql noprint;
				insert into &dsout values("&expl", &_varlvlcnt, &_varlvlp);
			quit;
		%end;
		%let k = %eval(&k + 1);
		%let expl = %scan(&ivcat, &k);
	%end;

	%let k    = 1;
	%let expl = %scan(&ivnum, &k);
	%do %while(&expl ne);
		proc sql noprint;
			select max(p) format=6.2 into :_varlvlp from
			(select &expl, 100*count(*)/&n as p from &dsin group by 1);
		quit;
		%if &_varlvlp > &cutperc %then %do;
			proc sql noprint;
				insert into &dsout values("&expl", ., &_varlvlp);
			quit;
		%end;
		%let k = %eval(&k + 1);
		%let expl = %scan(&ivnum, &k);
	%end;

	proc delete data=_tmp; run;
%mend scanvar;



***=====================================================================================;
***		(2) Perform Box-Cox transformation. The macro searches for the optimal value	;
***			of lambda, transforms the data, and tests the transformed data for the		;
***			assumption of normality														;
***																						;
***			Input:																		;
***				dsin	input dataset													;
***				dsout	output dataset													;
***				key		primary key for the dataset										;
***				iv		independent numeric variables to be transformed (must be +ve)	;
***			Output:		3 output datasets												;
***				dsout		output dataset, retain key, the orig and transformed var	;
***				keeplog		display all lambda's and their loglikelihood				;
***				keeplogmax	display the optimal lambda									;
***=====================================================================================;

%macro boxcox(dsin, dsout, key, iv);
	data keeplog;
		loglike = .;
		l = .;
		output;
		stop;
	run;

	%do lbig = -20 %to 20;
		data _new;
			set &dsin nobs=obs;
			n = obs;
			l = &lbig./10;
			x = &iv;
			if l ne 0 then xt = ((x**l)-1)/l;
			else xt = log(x);
		run;

		proc means mean noprint;
			var xt; output out = _mean1 mean = mxt;
		data _gmean1;
			set _mean1;
			call symput('gmean',mxt);
		run;

		data _new2;
			set _new;
			retain mxt;
			if _n_ = 1 then set _mean1;
			term1 = (xt - mxt)**2;
			term2 = log(x);
		run;

		proc means sum noprint;
			var term1 term2;
			output out = _sum1 sum = sterm1 sterm2;
		run;

		data loglike;
			set _sum1;
			set _new (obs=1);
			loglike = -1*(n/2)*log((1/n)*sterm1)+(l-1)*sterm2;
		run;

		data keeplog;
			set keeplog loglike;
			keep loglike l;
		run;
	%end;

	proc means max data=keeplog idmin noprint;
		var loglike;
		output out=_max max=maxloglike;

	data keeplogmax;
		set keeplog;
		keep l maxloglike;
		retain maxloglike;
		if _n_ = 1 then set _max;
		if loglike = maxloglike then output;
	run;

	proc print data=keeplogmax;
	title 'Optimal Value of Lambda';

	data _null_;
		set keeplogmax;
		if _n_ = 1 then call symput('l',l);
		stop;
	run;

	data &dsout;
		set &dsin;
		if &l ne 0 then tr_&iv. = ((&iv.**&l)-1)/&l;
		else tr_&iv. = log(&iv.);
		keep &key &iv tr_&iv.;
	proc sort nodupkey; by &key;
	run;

	proc delete data=loglike _gmean1 _max _mean1 _new _new2 _sum1; run;

	proc univariate normal plot data=&dsout (keep=&iv);
		title 'Normality Assessment for';
		title2 'Original Variable';
	run;

	proc univariate normal plot data=&dsout (keep=tr_&iv.);
		title 'Normality Assessment for';
		title2 'Power-Transformed Variable';
	run;
%mend boxcox;



***=====================================================================================;
***		(3) Compute WOE and Information Value											;
***																						;
***			Input:																		;
***				dsin	input dataset													;
***				dsout	output dataset prefix											;
***				key		primary key for the dataset										;
***				dv		dependent or response variable (1 or 0)							;
***				iv		list of independent numeric variables (separate by space)		;
***			Output:		3 output datasets with user specified prefix (dsout)			;
***				.._rk	output dataset for variable rank								;
***				.._iv	output dataset for IV and KS									;
***				.._woe	output dataset for IV and WOE									;
***=====================================================================================;

%macro woeiv(dsin, dsout, key, dv, iv);
	%let	tiermax	= 10;				*max number of bins to assign to variables;
	%let	outcome	= pct_cust_resp;	*name of response for summary tables;
	%let	outname	= % Responses;		*label of response for summary tables;

	***	get # of levels for each variable and assign counter;
	ods output nlevels=ivlvl;
	proc freq data=&dsin nlevels; tables &iv/noprint; run;
	ods output close;

	*** set macro values, and select variables with more than predefined number of tiers;
	proc sql noprint;
		select tablevar into :ivx separated by ' ' from ivlvl where nlevels > &tiermax;
		select count(*) into :obscnt from &dsin;
	quit;

	data _null_;
		call symput('kivx', compress(count(strip("&ivx"),' ')+1));
		call symput('kiv' , compress(count(strip("&iv") ,' ')+1));
	run;

	proc sql noprint;
		select tablevar into :v1-:v&kivx from ivlvl where nlevels > &tiermax;
		select tablevar into :x1-:x&kiv  from ivlvl;
	quit;

	*** rank those variables with more than 10 values;
	proc rank data=&dsin groups=&tiermax
		out=&dsout._rk (keep=&key &ivx &dv);
		var &ivx;
		ranks &ivx;
	run;

	*** combine with other variables;
	data tmp; set &dsin (keep=&key &iv); proc sort nodupkey; by &key; run;
	proc sort data=&dsout._rk nodupkey; by &key; run;

	data &dsout._rk;
		merge &dsout._rk (in=a) tmp (in=b drop=&ivx);
		by &key;
		if a and b;
	run;

	*** compute Information Value and Weight of Evidence;
	%macro calivwoe;
		%do i=1 %to &kiv;
			** count good and bad;
			proc sql noprint;
				select	sum(case when &dv=1 then 1 else 0 end),
						sum(case when &dv=0 then 1 else 0 end),
						count(*)
				into	:tot_good, :tot_bad, :tot_both
				from	&dsout._rk;
			quit;
			proc sql noprint;
				select count(*) into :kmiss from &dsout._rk where &&x&i = .;
			quit;

			** compute Weight of Evidence (WoE);
			proc sql noprint;
				create table woe&i as
				select	"&&x&i"															as variable format = $50. length = 50,
						&&x&i															as tier,
						count(*)														as cnt,
						count(*)/&tot_both												as cnt_pct,
						sum(case when &dv=1 then 1 else 0 end)							as sum_good,
						calculated sum_good/&tot_good									as dist_good,
						sum(case when &dv=0 then 1 else 0 end)							as sum_bad,
						calculated sum_bad/&tot_bad										as dist_bad,
						log((calculated dist_good)/(calculated dist_bad))				as woe,
						((calculated dist_good)-(calculated dist_bad))*(calculated woe)	as pre_iv,
						calculated sum_good/count(*)									as &outcome
				from	&dsout._rk
				group by 2
				order by &&x&i;
			quit;

			** compute Information Value (IV);
			proc sql noprint;
				create table iv&i as
				select	"&&x&i"				as variable format = $50. length = 50,
						sum(pre_iv)			as iv,
						(&kmiss/&obscnt)	as pct_missing
				from	woe&i;
			quit;
		%end;
	%mend calivwoe;
	%calivwoe;

	*** finalize dataset for IV;
	data ivall ; set iv1  - iv&kiv ; proc sort; by descending iv; run;
	data &dsout._iv;
		set ivall;
		ivrank = _n_;
	proc sort; by variable;
	run;

	*** finalize dataset for WOE;
	data woeall; set woe1 - woe&kiv; proc sort; by variable; run;
	data &dsout._woe;
		merge &dsout._iv woeall;
		by variable;
	proc sort; by ivrank tier;
	run;

	%let retvar=variable iv ivrank tier cnt cnt_pct sum_good dist_good sum_bad dist_bad woe &outcome pct_missing;
	data &dsout._woe(keep=&retvar);
		retain &retvar;
		set &dsout._woe;
		label variable    = "Variable";
		label iv          = "Information Value";
		label ivrank      = "IV Rank";
		label tier        = "Tier/Bin";
		label cnt         = "# Customers";
		label cnt_pct     = "% Custoemrs";
		label sum_good    = "# Responses";
		label dist_good   = "% Responses";
		label sum_bad     = "# Non-Responses";
		label dist_bad    = "% Non-Responses";
		label woe         = "Weight of Evidence";
		label &outcome    = "&outname";
		label pct_missing = "% Missing Values";
	run;

	*** examine KS and add to IV dataset;
	proc npar1way data=&dsin edf noprint;
		var &iv;
		class &dv;
		output out=ks(keep= _var_ _D_ rename=(_var_=variable _D_=ks));
	run;
	proc sort data=ks; by variable; run;

	data &dsout._iv;
		retain variable iv ivrank ks pct_missing;
		merge &dsout._iv (in=a) ks (in=b);
		by variable;
		if a;
		keep variable iv ivrank ks pct_missing;
		label variable    = "Variable";
		label iv          = "Information Value";
		label ivrank      = "IV Rank";
		label pct_missing = "% Missing Values";
	proc sort; by ivrank;
	run;

	*** remove temp files;
	%macro clrivwoe;
		%do i=1 %to &kiv;
			proc delete data=iv&i ; run;
			proc delete data=woe&i; run;
		%end;
	%mend clrivwoe;
	%clrivwoe;

	proc delete data=ivall ivlvl ks tmp woeall; run;
%mend woeiv;



***=====================================================================================;
***		(4) Compute Persaon, Hoeffding, and Spearman correlation						;
***																						;
***			Input:																		;
***				dsin	input dataset													;
***				dsout	output dataset													;
***				dv		dependent or response variable (1 or 0)							;
***				iv		list of independent numeric variables (separate by space)		;
***			Output:		1 output dataset with user specified name (dsout)				;
***=====================================================================================;

%macro corr(dsin, dsout, dv, iv);
	ods listing close;
	ods output spearmancorr  = spearman
			   hoeffdingcorr = hoeffding
			   pearsoncorr   = pearson;

	proc corr data=&dsin spearman hoeffding pearson rank;
		var &iv;
		with &dv;
	run;

	ods listing;

	data _null_;
		call symput('kiv', compress(count(strip("&iv"),' ')+1));
	run;

	data spearman (keep=variable scorr spvalue ranksp);
		length variable $50;
		set spearman;
		array best(*) best1--best&kiv;
		array r(*)	  r1--r&kiv;
		array p(*)	  p1--p&kiv;
		do i=1 to dim(best);
			variable = best(i);
			scorr	 = r(i);
			spvalue	 = p(i);
			ranksp	 = i;
			output;
		end;
	run;

	data hoeffding (keep=variable hcorr hpvalue rankho);
		length variable $50;
		set hoeffding;
		array best(*) best1--best&kiv;
		array r(*)	  r1--r&kiv;
		array p(*)	  p1--p&kiv;
		do i=1 to dim(best);
			variable = best(i);
			hcorr	 = r(i);
			hpvalue	 = p(i);
			rankho	 = i;
			output;
		end;
	run;

	data pearson (keep=variable pcorr ppvalue rankpe);
		length variable $50;
		set pearson;
		array best(*) best1--best&kiv;
		array r(*)	  r1--r&kiv;
		array p(*)	  p1--p&kiv;
		do i=1 to dim(best);
			variable = best(i);
			pcorr	 = r(i);
			ppvalue	 = p(i);
			rankpe	 = i;
			output;
		end;
	run;

	*** combine the 3 measures;
	proc sort data=spearman;  by variable; run;
	proc sort data=hoeffding; by variable; run;
	proc sort data=pearson;   by variable; run;

	data &dsout;
		merge pearson spearman hoeffding;
		by variable;
	proc sort; by rankpe;
	run;

	proc delete data=spearman hoeffding pearson; run;
%mend corr;



***=====================================================================================;
***		(5) Perform variable clustering using VARCLUS									;
***																						;
***			Input:																		;
***				dsin	input dataset													;
***				dsout	output dataset													;
***				iv		list of independent numeric variables (separate by space)		;
***			Output:		1 output dataset with user specified name (dsout)				;
***=====================================================================================;

%macro vclus(dsin, dsout, iv);
	%let maxev	= 0.7;

	ods listing close;
	ods output clusterquality=summary rsquare=clusters;

	proc varclus data=&dsin maxeigen=&maxev hi short;
		var &iv;
		title "Variable Clustering";
	run;
	ods listing;

	data _null_;
		set summary;
		call symput('kclus', compress(numberofclusters));
	run;

	%put ;
	%put *** # of variable clusters: &kclus;

	data varclus;
		set clusters (rename=(cluster=orig_cluster));
		if numberofclusters=&kclus;

		retain cluster;
		if orig_cluster ne '' then cluster + 1; else cluster + 0;

		r2ratio = rsquareratio;

		keep variable cluster r2ratio;
	proc sort; by cluster r2ratio;
	run;

	data &dsout;
		set varclus;
		by cluster r2ratio;
		if first.cluster then f_leastr2 = 1; else f_leastr2 = 0;
	run;

	proc delete data=summary clusters varclus; run;
%mend vclus;



***=====================================================================================;
***		(6) Perform variable binning base on decile (unsupervised)						;
***																						;
***			Input:																		;
***				dsin	input dataset													;
***				key		primary key for the dataset										;
***				iv		ONE independent numeric variable								;
***			Output:		2 output datasets with default name								;
***				&iv._vbin	record key, original iv, iv groups, and flags				;
***				&iv._vbmap	iv groups, min, max											;
***=====================================================================================;

%macro varbin(dsin, key, iv);
	%let tiermax = 10;

	proc rank data=&dsin groups=&tiermax
		out=rk (keep=&key &iv &iv._rk);
		var &iv;
		ranks &iv._rk;
	run;

	proc sql noprint;
		create table grp as
		select	&iv._rk		as grp,
				min(&iv)	as min,
				max(&iv)	as max,
				count(&key)	as k
		from	rk group by 1;
	quit;

	data grp;
		grp + 1;
		set grp (drop=grp);
	run;

	proc sql noprint;
		 select count(grp) into :kgrp from grp;
	quit;
	data _null_;
		call symput('kgrp', compress(&kgrp));
	run;

	proc sql noprint;
		select ., max into :min1, :min2-:min&kgrp from grp where grp ne &kgrp;
		select max    into        :max1-:max&kgrp from grp;
	quit;

	data &iv._vbin; set &dsin (keep=&key &iv); run;

	%macro setbin;
		%do i=1 %to &kgrp;
			data &iv._vbin;
				set &iv._vbin;
				format &iv._vbgrp $30. &iv._vb&i 1.;
				min = putn(&&min&i,12.2);
				max = putn(&&max&i,12.2);

				if &i = 1 then do;
					if &iv le max then &iv._vbgrp = '01. <= '||compress(put(max,12.2));
				end;
				else if &i = &kgrp then do;
					if &iv gt min then &iv._vbgrp = put(&i,z2.)||'. > '||compress(put(min,12.2));
				end;
				else do;
					if min < &iv <= max then
						&iv._vbgrp  = put(&i,z2.)||'. ( '||compress(put(min,12.2))||' , '||compress(put(max,12.2))||' ]';
				end;
				drop min max;

				&iv._vb&i = (putn(substr(&iv._vbgrp,1,2),2.) = &i);
			run;
		%end;
	%mend;
	%setbin;

	proc sort data=&iv._vbin nodupkey; by &key; run;

	proc sql;
		create table &iv._vbmap as
		select	&iv._vbgrp, min(&iv) as min, max(&iv) as max, count(&key) as k
		from	&iv._vbin group by 1;
	quit; 

	proc delete data=grp rk; run;
%mend varbin;



***=====================================================================================;
***		(7) Perform optimal binning base on entropy (supervised)						;
***																						;
***			Input:																		;
***				dsin	input dataset													;
***				key		primary key for the dataset										;
***				dv		dependent or response variable (1 or 0)							;
***				iv		ONE independent numeric variable								;
***			Output:		2 output datasets with default name								;
***				&iv._obin	record key, original iv, iv groups, and flags				;
***				&iv._obmap	iv groups, min, max											;
***=====================================================================================;

%macro optbin(dsin, key, dv, iv);
	%let maxbin   = 10;
	%let maxcutpt = 350;
	%let binmerg  = 0.1;

	***	determine potential cut points, midpoint between each successive pair of distinct IV values;
	proc sql noprint;
		create table iv as select distinct &iv from &dsin where &iv ne .;
	quit;
	data iv;
		obs = _n_;
		set iv;
	run;
	proc sql noprint;
		create table cutpts as
		select mean(a.&iv, b.&iv) as t
		from   iv a, iv b
		where  a.obs = b.obs - 1;
	quit;

	*** # of cut points capped at user-defined number (maxcutpt);
	proc sql noprint;
		select count(*) into :kcutpt from cutpts;
	quit;

	%if &kcutpt > &maxcutpt %then %do;
		data cutpts;
			obs = int(_n_ * &maxcutpt / &kcutpt);
			set cutpts;
		run;

		data cutpts (drop=obs);
			set cutpts;
			by obs;
			if first.obs then output;
		run;
	%end;

	*** set additional macro variables;
	proc sql noprint;
		select count(*) into :kcutpt from cutpts;
		select max(&iv) into :maxiv  from &dsin;
		select min(&iv) into :miniv  from &dsin;
	quit;

	data _null_;
		call symput("kcutpt", compress(&kcutpt));
	run;

	proc sql noprint;
		select t into :t1-:t&kcutpt from cutpts;
	quit;

	%put ;
	%put Optimal binning for &iv;
	%put *** minimum val  = %left(&miniv);
	%put *** maximum val  = %left(&maxiv);
	%put *** # cut point  = %left(&kcutpt);

	*** create table for boundary of bins, initialize with IV min and max;
	proc sql noprint;
		create table bin (lftbin num, rhtbin num, n num, entropy num, skip num);
		insert into bin values(&miniv, &maxiv, ., ., 0);
	quit;

	%let stop = 0;
	%let kbin = 1;

	*** iterative algorithm to determine cut points;
	%do %while (&kbin lt &maxbin and &stop = 0);
		%let stop = 1;

		*** identify bins to process;
		proc sql noprint;
			select count(*) into :kgdbin from bin where skip ne 1;
		quit;
		data _null_;
			call symput("kgdbin", compress(&kgdbin));
		run;
		proc sql noprint;
			select lftbin into :lft_1-:lft_&kgdbin from bin where skip ne 1;
			select rhtbin into :rht_1-:rht_&kgdbin from bin where skip ne 1;
		quit;
		data bin; set bin; skip = 1; run;

		%do i = 1 %to &kgdbin;
			%let min_en  = 1;

			*** find optimal cut points between each left & right;
			%do j = 1 %to &kcutpt;
				%if %sysevalf(&&lft_&i lt &&t&j and &&t&j lt &&rht_&i) %then %do;
					proc sql noprint;
						select sum(&dv), count(*) into :lft_dv1, :lft_n
						from   &dsin
						where  &iv ne . and &iv ge &&lft_&i and &iv le &&t&j;

						select sum(&dv), count(*) into :rht_dv1, :rht_n
						from   &dsin
						where  &iv ne . and &iv gt &&t&j and &iv le &&rht_&i;
					quit;

					%if %sysevalf(&lft_n * &rht_n) gt 0 %then %do;
						%let lft_p1    = %sysevalf(&lft_dv1 / &lft_n);
						%let lft_p0    = %sysevalf(1 - &lft_p1);
						%let lft_logp1 = 0;
						%if &lft_p1 > 0 %then %let lft_logp1 = %sysfunc(sum(0, %sysfunc(log2(&lft_p1))));
						%let lft_logp0 = 0;
						%if &lft_p0 > 0 %then %let lft_logp0 = %sysfunc(sum(0, %sysfunc(log2(&lft_p0))));
						%let lft_entro = %sysevalf(-1 * &lft_p1 * &lft_logp1 - &lft_p0 * &lft_logp0);

						%let rht_p1    = %sysevalf(&rht_dv1 / &rht_n);
						%let rht_p0    = %sysevalf(1 - &rht_p1);
						%let rht_logp1 = 0;
						%if &rht_p1 > 0 %then %let rht_logp1 = %sysfunc(sum(0, %sysfunc(log2(&rht_p1))));
						%let rht_logp0 = 0;
						%if &rht_p0 > 0 %then %let rht_logp0 = %sysfunc(sum(0, %sysfunc(log2(&rht_p0))));
						%let rht_entro = %sysevalf(-1 * &rht_p1 * &rht_logp1 - &rht_p0 * &rht_logp0);

						%let entropy   = %sysevalf((&lft_n * &lft_entro + &rht_n * &rht_entro) / (&lft_n + &rht_n));
						
						%if &entropy < &min_en %then %do;
							%let cutpt      = &&t&j;
							%let min_en     = &entropy;
							%let min_lft_en = &lft_entro;
							%let min_rht_en = &rht_entro;
							%let min_lft_n  = &lft_n;
							%let min_rht_n  = &rht_n;
						%end;
					%end;
				%end;
			%end;
			%put ;
			%put *** left bound   = %left(&&lft_&i);
			%put *** right bound  = %left(&&rht_&i);
			%put *** cut point    = %left(&cutpt);
			%put *** min entropy  = %left(&min_en);

			*** update bin table if accept the cut base on MDLP Acceptance Criterion;
			%if &min_en lt 1 %then %do;
				proc sql noprint;
					select sum(&dv), count(*) into :dv1, :n
					from   &dsin
					where  &iv ne . and &iv ge &&lft_&i and &iv le &&rht_&i;

					select count(distinct &dv) into :lft_k
					from   &dsin
					where  &iv ne . and &iv ge &&lft_&i and &iv le &cutpt;

					select count(distinct &dv) into :rht_k
					from   &dsin
					where  &iv ne . and &iv gt &cutpt and &iv le &&rht_&i;
				quit;

				%let p1      = %sysevalf(&dv1 / &n);
				%let p0      = %sysevalf(1 - &p1);
				%let logp1   = %sysfunc(sum(0, %sysfunc(log2(&p1))));
				%let logp0   = %sysfunc(sum(0, %sysfunc(log2(&p0))));
				%let entropy = %sysevalf(-1 * &p1 * &logp1 - &p0 * &logp0);

				%let delta   = %sysevalf(%sysfunc(log2(7)) - 2 * &entropy + &lft_k * &min_lft_en + &rht_k * &min_rht_en);

				%put information gain = %left(%sysevalf(&entropy - &min_en));
				%put MDLP threshold   = %left(%sysevalf(%sysfunc(log2(&n-1))/&n + &delta/&n));

				%if %sysevalf(1000 * (&entropy - &min_en)) > %sysevalf(1000 * (%sysfunc(log2(&n-1))/&n + &delta/&n)) %then %do;
					data bin;
						set bin;
						if lftbin = &&lft_&i and rhtbin = &&rht_&i then delete;
					run;
					%put delete ( %left(&&lft_&i) , %left(&&rht_&i) );

					proc sql noprint;
						insert into bin values(&&lft_&i, &cutpt, &min_lft_n, &min_lft_en, 0);
						insert into bin values(&cutpt, &&rht_&i, &min_rht_n, &min_rht_en, 0);
					quit;
					%put insert ( %left(&&lft_&i) , %left(&cutpt) ) and ( %left(&cutpt) , %left(&&rht_&i) ); 

					%let stop = 0;
				%end;
			%end;
		%end;

		proc sort data=bin; by lftbin; run;
		proc sql noprint;
			select count(*) into :kbin from bin;
		quit;
		data _null_;
			call symput("kbin", compress(&kbin));
		run;
	%end;

	*** merge bin if ratio of its size to that of a neighboring bin is smaller than user-specified threshold (binmerg);
	data bin;
		obs = _n_;
		set bin (drop=skip);
	run;

	proc sql noprint;
		create table bin_merg as
		select	case when b.obs = . and c.obs = .                                  then a.obs
					 when a.n/min(b.n, c.n) lt &binmerg and b.entropy = .          then c.obs
					 when a.n/min(b.n, c.n) lt &binmerg and c.entropy = .          then b.obs
					 when a.n/min(b.n, c.n) lt &binmerg and b.entropy lt c.entropy then b.obs
					 when a.n/min(b.n, c.n) lt &binmerg and b.entropy gt c.entropy then c.obs
					 when a.n/min(b.n, c.n) lt &binmerg and b.n       lt c.n       then b.obs
					 when a.n/min(b.n, c.n) lt &binmerg                            then c.obs
				else a.obs end as obs,
				a.lftbin, a.rhtbin, a.n
		from	bin a
		left join bin b
		on		a.obs = b.obs + 1
		left join bin c
		on		a.obs = c.obs - 1
		;
	quit;

	*** finalizing 2 outputs;
	proc sql noprint;
		create table &iv._obmap as
		select	obs, min(lftbin) as min,
				max(rhtbin) as max,
				sum(n) as k
		from	bin_merg group by 1;
	quit;

	data &iv._obmap;
		bin = _n_;
		set &iv._obmap (drop=obs);
	run;

	proc sql noprint;
		 select count(bin) into :kgrp from &iv._obmap;
	quit;
	data _null_;
		call symput('kgrp', compress(&kgrp));
	run;

	%if &kgrp = 1 %then %do;
		proc sql noprint;
			select min, max into :min1, :max1 from &iv._obmap;
		quit;
	%end;

	%if &kgrp > 1 %then %do;
		proc sql noprint;
			select ., max into :min1, :min2-:min&kgrp from &iv._obmap where bin ne &kgrp;
			select max    into        :max1-:max&kgrp from &iv._obmap;
		quit;
	%end;

	data &iv._obin; set &dsin (keep=&key &iv); run;

	%macro setbin;
		%do i=1 %to &kgrp;
			data &iv._obin;
				set &iv._obin;
				format &iv._obgrp $30. &iv._ob&i 1.;
				min = putn(&&min&i,12.2);
				max = putn(&&max&i,12.2);

				if &i = 1 then do;
					if &iv le max then &iv._obgrp = '01. <= '||compress(put(max,12.2));
				end;
				else if &i = &kgrp then do;
					if &iv gt min then &iv._obgrp = put(&i,z2.)||'. > '||compress(put(min,12.2));
				end;
				else do;
					if min < &iv <= max then
						&iv._obgrp  = put(&i,z2.)||'. ( '||compress(put(min,12.2))||' , '||compress(put(max,12.2))||' ]';
				end;
				drop min max;

				&iv._ob&i = (putn(substr(&iv._obgrp,1,2),2.) = &i);
			run;
		%end;
	%mend;
	%setbin;

	proc sort data=&iv._obin nodupkey; by &key; run;

	proc sql;
		create table &iv._obmap as
		select	&iv._obgrp, min(&iv) as min, max(&iv) as max, count(&key) as k
		from	&iv._obin group by 1;
	quit;

	*** clean up working files/variables;
	proc datasets library = work nolist nodetails;
		delete bin bin_merg cutpts iv;
	run;
	quit;
%mend optbin;



***=====================================================================================;
***		(8) Clustering levels of categorical inputs										;
***																						;
***			Input:																		;
***				dsin	input dataset													;
***				dv		dependent or response variable (1 or 0)							;
***				iv		ONE independent categorical variable							;
***			Output:		1 output dataset with default name, &iv._c						;
***=====================================================================================;

%macro lvlclus(dsin, dv, iv);
	*** get freq and % for each level;
	proc means data=&dsin noprint nway;
		class &iv;
		var &dv;
		output out=level mean=prop;
	run;

	*** variable clustering;
	ods listing close;
	ods output clusterhistory=cluster;
	proc cluster data=level outtree=fortree method=ward;
		freq _freq_;
		var prop;
		id &iv;
	run;
	ods listing;

	*** determine optimum number of clusters and the corresponding levels;
	proc freq data=&dsin noprint;
		tables &iv * &dv / chisq;
		output out=chi(keep=_pchi_) chisq;
	run;

	data cutoff;
		if _n_ = 1 then set chi;
		set cluster;
		if numberofclusters > 1;
		chisquare	= _pchi_ * rsquared;
		degfree		= numberofclusters-1;
		logpvalue	= logsdf('CHISQ',chisquare,degfree);
	run;

	proc sql noprint;
		select numberofclusters into :ncl
		from cutoff
		having logpvalue = min(logpvalue);
	quit;

	*** create dendrogram for optimal clustering;
	proc tree data=fortree h=rsq vaxis=axis1
			nclusters=&ncl out=&iv._c;
		id &iv;
		axis1 label=("Proportion of Chi-Sq Stat");
	run;

	proc sort data=&iv._c; by cluster &iv; run;

	*** delete interim tables;
	proc delete data=level cluster fortree chi cutoff; run;
%mend lvlclus;



***=====================================================================================;
***    (9) Compute c-statistics for both training and validation dataset for all the	;
***			best subset selection using selection=score									;
***																						;
***			Input:																		;
***				dsin_trn	input dataset for training									;
***				dsin_vld	input dataset for validation								;
***				bestsubset	produced by initial selection=score							;
***				dv			dependent or response variable (1 or 0)						;
***			Output:			1 output dataset with default name							;
***				mdlopt		mdlno, modelinputs, datarole, c								;
***=====================================================================================;

%macro getcstat(dsin_trn, dsin_vld, bestsubset, dv);
	proc sql noprint;
		select count(*) into :kmdl from &bestsubset;
	quit;

	proc datasets
		library=work
		nodetails
		nolist;
		delete mdlopt;
	run;
	quit;

	proc sql noprint;
		create table mdlopt (mdlno int, modelinputs varchar(500), datarole char(9), c num);
	quit;

	%do i=1 %to &kmdl;
		*** get model inputs;
		data _null_ ;
			set &bestsubset;
			if _n_ = &i;
			call symput('list', variablesinmodel);
		run;

		*** score training and validation dataset;
		proc logistic noprint data=&dsin_trn descending;
			model  &dv = &list;
			score data = &dsin_trn out = score_trn (keep=&dv p_1 p_0);
			score data = &dsin_vld out = score_vld (keep=&dv p_1 p_0);
		run;

		*** get c-statistics;
		data score_trn; set score_trn; logit = log(p_1/p_0); run;
		data score_vld; set score_vld; logit = log(p_1/p_0); run;

		proc logistic data=score_trn descending;
			model &dv = logit;
			ods output association=assoc_trn;
		run;

		proc logistic data=score_vld descending;
			model &dv = logit;
			ods output association=assoc_vld;
		run;

		proc sql noprint;
			select nvalue2 into :c_trn from assoc_trn where label2 = 'c';
			select nvalue2 into :c_vld from assoc_vld where label2 = 'c';
		quit;

		proc sql noprint;
			insert into mdlopt values(&i, "&list", 'Train', &c_trn);
			insert into mdlopt values(&i, "&list", 'Validate', &c_vld);
		quit;
	%end;

	proc delete data=score_trn score_vld assoc_trn assoc_vld; run;
%mend getcstat;



***=====================================================================================;
***	   (10) 3-Step approach in model selection as proposed by							;
***			Shtatland, Kleinman, and Cain (2003). Stepwise Methods in Using SAS PROC	;
***			LOGISTIC and SAS Enterprise Miner for Prediction							;
***			1. Construct a full stepwise sequence										;
***			2. Find optimal models with regard to some info criteria					;
***			3. Construct sub-optimal models by using best subset selection				;
***																						;
***			Input:																		;
***				dsin_trn	input dataset for training									;
***				dsin_vld	input dataset for validation								;
***				dv			dependent or response variable (1 or 0)						;
***				iv			list of independent numeric variables (separate by space)	;
***			Output:			1 output dataset with default name							;
***				mdlopt		mdlno, modelinputs, datarole, c								;
***=====================================================================================;

%macro model3step(dsin_trn, dsin_vld, dv, iv);
	*** constructing a full stepwise sequence;
	ods output ModelBuildingSummary=mdlsum;
	ods output FitStatistics=mdlfit;
	proc logistic data=&dsin_trn descending;
		model	&dv	      = &iv /
				selection = stepwise
				slentry   = 0.99
				slstay    = 0.99;
	run;
	ods output close;

	*** compute IC(1), IC(3/2), and IC(2);
	data infocrit;
		set mdlfit;
		if step > 0 and criterion = '-2 Log L';
		ic1    = interceptandcovariates + (step + 1);
		ic32   = interceptandcovariates + 1.5 * (step + 1);
		ic2    = interceptandcovariates + 2.0 * (step + 1);
		keep step ic1 ic32 ic2;
	proc sort nodupkey; by step;
	run;

	proc sort data=mdlsum nodupkey; by step; run;

	data infocrit;
		merge mdlsum (in=a keep=step effectentered) infocrit (in=b);
		by step;
		if a and b;
	run;

	*** determine optimal models with regard to IC(1), IC(3/2), and IC(2);
	*** set start stop for best subset model;
	proc means noprint data=infocrit min;
		var ic1 ic32 ic2;
		output out=optstep minid(ic1(step) ic32(step) ic2(step))
							= optic1_step optic32_step optic2_step min=;
	run;

	proc sql noprint;
		select	max(step) into :maxstep from infocrit;
		select	max(1, min(optic1_step, optic32_step, optic2_step)-1),
				min(&maxstep, max(optic1_step, optic32_step, optic2_step)+1)
		into	:bs_strt, :bs_stop from optstep;
	quit;
	%put best subset start = %left(&bs_strt);
	%put best subset stop  = %left(&bs_stop);

	*** perform best subset selection;
	ods listing close;
	ods output bestsubsets=bs;
	proc logistic data=&dsin_trn des;
		model	&dv	      = &iv /
				selection = score
				best      = 2
				start     = &bs_strt
				stop      = &bs_stop;
	run;
	ods listing;

	*** remove interim tables;
	proc delete data=infocrit mdlfit mdlsum optstep; run;

	*** compute c-statistics for both training and validation dataset for
		all the candidate models specified in the best subset selection;
	%getcstat(&dsin_trn, &dsin_vld, bs, &dv);
%mend model3step;



***=====================================================================================;
***											WIP											;
***	   (11)	Model selection based on the purposeful selection algorithm as proposed		;
***			by Bursac, Gauss, Williams, Kleinman, and Hosmer (2008). Purposeful			;
***			Selection of Variables in Logistic Regression								;
***			1. Include variables having significant univariate test						;
***			2. Iteratively remove variable one at a time if it is non-significant and	;
***			   not a confounder															;
***			3. Include variables not selected in step 1 but significant after step 2	;
***																						;
***			Input:																		;
***				dsin	input dataset													;
***				dv		dependent or response variable (1 or 0)							;
***				iv		list of independent numeric variables (separate by space)		;
***			Output:		1 output dataset with default name								;
***				mdlopt		mdlno, modelinputs, datarole, c								;
***=====================================================================================;

%macro PurposefulSelection(dsin, dv, iv);
	%let p1      = 0.25;					*p-value for variable inclusion in step 1;
	%let p2      = 0.1;						*p-value for variable retention in step 2;
	%let betachg = 15;						*% change in para estimates above which considered as confounding;
	%let p3      = 0.15;					*p-value for variable inclusion in step 3;

	*** scan input iv;
	%let i = 1;
	%do %while (%scan(&iv, &i) ne);
		%let iv&i = %scan(&iv, &i);
		%put IV&i is &&iv&i;
		%let i = %eval(&i + 1);
	%end;
	%let n = %eval(&i - 1);					*&n = number of covariates;
	%put Number of covariates = &n;

	*** identify significant variables base on univariate test;
	proc datasets
		library=work
		nodetails
		nolist;
	run;
	quit;

	%do i = 1 %to &n;
		proc logistic data=&dsin descending;
			model &dv = &&iv&i;
			ods output ParameterEstimates=paraest&i;
		run;
    %end;

	data candidates;
		format variable $50.;
		set paraest1-paraest&n;
		if variable = 'Intercept' then delete;
		if probchisq le &p1 then firstpass = 1; else firstpass = 0;
	run;

	*** fit a logistic model with significant candidates (step 1);
	proc sql noprint;
		select variable into :sigvar separated by ' ' from candidates (where=(firstpass=1));
		%let ksigvar=&sqlobs;
	quit;

	%put Number of significant variables for first round = %left(&ksigvar);

	%if &ksigvar > 0 %then %do;
		proc logistic data=&dsin descending;
			model &dv = &sigvar / scale=none aggregate;
			ods output ParameterEstimates = paraest;
		run;

		data paraest;
			set paraest;
			if variable = 'Intercept' then delete;
		proc sort; by descending probchisq;
		run;

		data paraest;
			set paraest;
			k = _n_;
		run;
	%end;

	*** loop through each variable, remove those with p-value > P2 and is non-confounding i.e. < betachg (step 2);
	%if &ksigvar > 0 %then %do;
		%let varcnt = 1;
		data _null_;
			set paraest;
			if _n_ = &varcnt then call symput('pvaluecheck',left(probchisq));
			if _n_ = &varcnt then call symput('checkvar',variable);
		run;

		%do %while (%sysevalf(&pvaluecheck > &p2));
			%put Evaluating &checkvar with p-value = &pvaluecheck ...;

			proc sql noprint;
				select variable into :reducedsigvars separated by ' ' from paraest where k ne &varcnt;
			quit;

			proc logistic data=&dsin descending;
				model &dv = &reducedsigvars / scale=none aggregate;
				ods output ParameterEstimates = reducedest;
			run;

			proc sql noprint;
				create table estchg as
				select	a.variable, b.estimate as fullest,
						a.estimate as reducedest,
						100 * abs((a.estimate - b.estimate)/b.estimate) as estchg
				from	reducedest a
				join	paraest b
				on		a.variable = b.variable
				order by 4 desc;
			quit;

			data _null_;
				set estchg;
				if _n_ = 1 then call symput('checkbetachg',left(estchg));
			run;

			%put Max change in point estimates = &checkbetachg;

			%if %sysevalf(&checkbetachg >= &betachg) %then %do;
				%put retain &checkvar;
				%let varcnt  = %eval(&varcnt + 1);
			%end;
			%else %if %sysevalf(&checkbetachg < &betachg) %then %do;
				%put remove &checkvar;
				data paraest;
					set reducedest;
					if variable = 'Intercept' then delete;
				proc sort; by descending probchisq;
				run;

				data paraest;
					set paraest;
					k = _n_;
				run;
			%end;

			data _null_;
				set paraest;
				if _n_ = &varcnt then call symput('pvaluecheck',left(probchisq));
				if _n_ = &varcnt then call symput('checkvar',variable);
			run;
		%end;
	%end;

	*** add back any variable not identified as significant in step 1 but is significant,
		with p-value < P3, in the presence of other variables (step 3);
	proc sql noprint;
		select count(*) into :kinsigvar from candidates where firstpass = 0;
		select variable into :insigvar1-:insigvar99999 from candidates where firstpass = 0;
	quit;
	
	data _null_;
		call symput('kinsigvar', left(&kinsigvar));
	run;

	%put Number of non-candidates variables: &kinsigvar;

	%if &kinsigvar > 0 %then %do;
		* fit one variable at a time for insignificant variables;
		%do i = 1 %to &kinsigvar;
			proc logistic data=&dsin descending;
				model &dv = &&insigvar&i;
				ods output ParameterEstimates=insignparaest&i;
			run;
	    %end;
	%end;



	*** clean up;
	proc datasets nolist nodetials;
		*delete paraest1-paraest&n;
		*delete insignparaest1-insignparaest&kinsigvar;
		*delete candidates estchg reducedest;
	run; quit;
%mend PurposefulSelection;



***=====================================================================================;
***    (12)	Assign decile to training and validation dataset							;
***																						;
***			Input:																		;
***				dsin_trn	scoring dataset for training with variable dv phat			;
***				dsin_vld	scoring dataset for validation with variable dv phat		;
***				dv			dependent or response variable (1 or 0)						;
***				phat		probability score for response variable						;
***			Output:			2 output datasets with default name							;
***				&dsin_trn_perf		decile, min_phat, max_phat, k_cust, k_resp			;
***				&dsin_vld_perf		decile, min_phat, max_phat, k_cust, k_resp			;
***=====================================================================================;

%macro assigndec(dsin_trn, dsin_vld, dv, phat);
	*** get decile threshold from training dataset;
	proc sort data=&dsin_trn out=tmp1; by &phat; run;

	data tmp2 (keep=&phat mean_rank);
		set tmp1 nobs=dsobst;
		by &phat;
		if _n_ = 1 then call symput ('dsobs',put(dsobst,13.));
		retain cntr 0 const1 cntrt;
		if first.&phat then do;
			cntrt = 0;
			const1 = 0;
		end;
		cntr + 1;
		cntrt = cntrt + cntr;
		const1 + 1;
		if last.&phat then do;
			mean_rank = cntrt / const1;
			output;
		end;
	run;

	data tmp3 (drop=mean_rank);
		merge tmp1 (in=a) tmp2 (in=b);
		by &phat;
		if a;
		origdec = 10 - floor(mean_rank * 10 / (&dsobs + 1));
	run;

	*** traning dataset performance;
	proc sql noprint;
		create table tmp4 as
		select	origdec,
				min(&phat) as min_phat format=12.10,
				max(&phat) as max_phat format=12.10,
				count(*)   as k_cust   format=comma8.0,
				sum(&dv)   as k_resp   format=comma8.0
		from	tmp3
		group by 1;
	quit;

	proc sql noprint; select sum(k_resp)/sum(k_cust) into :ovr_conv from tmp4; quit;

	data &dsin_trn._perf;
		decile = _n_;
		set tmp4 (drop=origdec);
		format p_resp 8.2 cum_cust cum_resp comma8.0 cum_p_resp 8.2 p_uplift 8.0;
		retain cum_cust cum_resp 0;
		p_resp     = 100 * k_resp / k_cust;
		cum_cust   = cum_cust + k_cust;
		cum_resp   = cum_resp + k_resp;
		cum_p_resp = 100 * cum_resp / cum_cust;
		p_uplift   = 100 * (cum_resp / cum_cust / &ovr_conv - 1);
	run;

	*** apply decile to validation dataset;
	proc sql noprint; select count(*) into :kdec from &dsin_trn._perf; quit;

	data _null_; call symput('kdec', compress(&kdec)); run;
	proc sql noprint;
		select decile   into :dec1-:dec&kdec from &dsin_trn._perf;
		select min_phat format=12.10 into :min1-:min&kdec from &dsin_trn._perf;
		select min_phat format=12.10 into :max2-:max&kdec from &dsin_trn._perf where decile ne &kdec;
	quit;

	data tmp5;
		set &dsin_vld;
		format decile 2.;
		if &phat ge (&min1-0.00000001) then decile = 1;
		%put ;
		%put Decile threshold;
		%do i = 2 %to %eval(&kdec-1);
			%put &i: ( &&min&i , &&max&i );
			if (&phat ge (&&min&i-0.00000001)) and (&phat lt &&max&i) and decile = . then decile = &i;
		%end;
		if &phat lt &&max&kdec and decile = . then decile = &kdec;
	run;

	proc sql noprint;
		create table &dsin_vld._perf as
		select	decile,
				min(&phat) as min_phat format=12.10,
				max(&phat) as max_phat format=12.10,
				count(*)   as k_cust   format=comma8.0,
				sum(&dv)   as k_resp   format=comma8.0
		from	tmp5
		group by 1;
	quit;

	proc sql noprint; select sum(k_resp)/sum(k_cust) into :ovr_conv from &dsin_vld._perf; quit;

	data &dsin_vld._perf;
		set &dsin_vld._perf;
		format p_resp 8.2 cum_cust cum_resp comma8.0 cum_p_resp 8.2 p_uplift 8.0;
		retain cum_cust cum_resp 0;
		p_resp     = 100 * k_resp / k_cust;
		cum_cust   = cum_cust + k_cust;
		cum_resp   = cum_resp + k_resp;
		cum_p_resp = 100 * cum_resp / cum_cust;
		p_uplift   = 100 * (cum_resp / cum_cust / &ovr_conv - 1);
	run;

	*** clean up;
	proc datasets nolist nodetails;
		delete tmp1 tmp2 tmp3 tmp4 tmp5;
	run; quit;
%mend assigndec;



***=====================================================================================;
***    (13)	Simple Bootstrap															;
***																						;
***			Input:																		;
***				dsin	input dataset													;
***				dsout	output dataset													;
***				var		variable to compute the statistics								;
***			Output:		1 output dataset												;
***				dsout	the sample statistics for each replicated sample				;
***=====================================================================================;

%macro sbs(dsin, dsout, var);
	sasfile &dsin load;					*load dataset into memory to speed up processing;
	proc surveyselect
		data     = &dsin
		out      = _tmp
		seed     = 30459584
		method   = urs
		samprate = 1
		outhits
		rep      = 1000;
	run;
	sasfile &dsin close;

	ods listing close;
	proc univariate data=_tmp;
		var &var;
		by Replicate;
		output out = &dsout kurtosis = curt;
	run;
	ods listing;

	proc univariate data=&dsout;
		var curt;
		output out=final pctlpts=2.5, 97.5 pctlpre=ci;
	run;
%mend sbs;
