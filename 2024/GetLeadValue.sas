/*-----------------------------------------------------------------------------
  ELIXIR CLINICAL RESEARCH

  Project Name / No: 

  SAS Version:       9.4
  Operating System:  WIN
-------------------------------------------------------------------------------

  Author:            Xianhua Zeng
  Creation Date:     31thAug2021 

  Program name:      GetLeadValue.sas

  Files Created:     GetLeadValue.log

  Program Purpose:   To obtain leading value for a variable (i.e., the next 
                     value in order of observations). Opposite behavior of 
                     existing Lag() function.

  Macro Parameters   NA

-----------------------------------------------------------------------------*/

%macro GetLeadValue(dataIn  =
                    , dataOut = &dataIn
                    , varIn   =
                    , varOut  = 
                    , byVar   =
                    );

/* Parameter check */
/* Check if parameter is null */
%if "%superQ(dataIn)" = "" %then %do;
    %let glv_returnmsg = Source dataset (dataIn) is null.;
    %goto MacErr;
%end;

%if "%superQ(varIn)" = "" %then %do;
    %let glv_returnmsg = Source variable (varIn) is null.;
    %goto MacErr;
%end;

/* Check if parameter is valid */
/* Check if parameter dataIn is a valid library name and/or dataset name */
%if %qSysFunc(prxMatch(%qSysFunc(
        prxParse(/^%bQuote(([a-zA-Z_]{1}[a-zA-Z_0-9]{0,7}\.)?[a-zA-Z_]{1}[a-zA-Z_0-9]{0,31})$/)), %superQ(dataIn))) ~= 1 %then %do;
    %let glv_returnmsg = %str(Parameter dataIn=&dataIn. has an invalid library name and/or dataset name, please correct.);
    %goto MacErr;
%end;

/* Check if parameter dataOut is a valid library name and/or dataset name */
%if %qSysFunc(prxMatch(%qSysFunc(
        prxParse(/^%bQuote(([a-zA-Z_]{1}[a-zA-Z_0-9]{0,7}\.)?[a-zA-Z_]{1}[a-zA-Z_0-9]{0,31})$/)), %quote(&dataOut))) ~= 1 %then %do;
    %let glv_returnmsg = %str(Parameter dataOut=&dataOut. has an invalid library name and/or dataset name, please correct.);
    %goto MacErr;
%end;

%do i = 1 %to %sysfunc(countw(&varIn, @));
    %let varIn = &varIn;
    %let varIn = %sysfunc(prxchange(s/\@$//i, -1, %quote(&varIn)));
    %let varIn&i = %scan(&varIn, &i, @);
    /* Check if parameter varIn is a valid variable name */
    %if %qSysFunc(prxMatch(%qSysFunc(
            prxParse(/^%bQuote([a-zA-Z_]{1}[a-zA-Z_0-9]{0,31})$/)), %quote(&&varIn&i))) ~= 1 %then %do;
        %let glv_returnmsg = %str(Parameter varIn=&&varIn&i. has an invalid variable name, please correct.);
        %goto MacErr;
    %end;

    /* Check if parameter varIn exists in dataset dataIn */
    %if %index(&dataIn, .) %then %do;
        %let glv_lib = %sysfunc(prxchange(s/(.+)\.(.+)$/\U\1/, 1, %quote(&dataIn)));
        %let glv_dsn = %sysfunc(prxchange(s/(.+)\.(.+)$/\U\2/, 1, %quote(&dataIn)));
    %end;
    %else %do;
        %let glv_lib = WORK;
        %let glv_dsn = %upcase(&dataIn);
    %end;

    proc sql noprint;
        select NAME into :var_list separated by ' '
          from dictionary.columns 
          where LIBNAME = "&glv_lib" and MEMNAME = "&glv_dsn"
          ;
    quit;

    %if not %sysfunc(prxmatch(/\b&&varIn&i\b/i, %quote(&var_list))) %then %do;
        %let glv_returnmsg = %str(Parameter varIn=&&varIn&i. does not exist in dataset &dataIn, please check.);
        %goto MacErr;
    %end;
%end;

/* Check if parameter byVar exists in dataset dataIn */
%if "%superQ(byVar)" ^= "" %then %do;

    %let byVar = %sysfunc(compbl(%sysfunc(strip(&byVar))));

    %let ggvl_byVar = %sysfunc(prxchange(s/\b(descending)\b//i, -1, %quote(&byVar)));

    %do i=1 %to %sysfunc(countw(&ggvl_byVar));
        %let glv_byVar_ind = %scan(&ggvl_byVar, &i, %str( ));

        %if not %sysfunc(prxmatch(/&glv_byVar_ind/i, %quote(&var_list))) %then %do;
            %let glv_returnmsg = %str(Parameter byVar &glv_byVar_ind. does not exist in dataset &dataIn, please check.);
            %goto MacErr;
        %end;
    %end;
%end;

%if "%superQ(varOut)" ^= "" %then %do;
    /* Check if variable number of varOut is equal with number of varIn */
    %if  %sysfunc(countw(&varIn, @)) ^= %sysfunc(countw(&varOut, @)) %then %do;
        %let glv_returnmsg = %str(Variable number of varOut is not equal with variable number of varIn);
        %goto MacErr;
    %end;

    /* Check if parameter varOut is a valid variable name */
    %do i = 1 %to %sysfunc(countw(&varOut, @));
        %let varOut = &varOut;
        %let varOut = %sysfunc(prxchange(s/\@$//i, -1, %quote(&varOut)));
        %let varOut&i = %scan(&varOut, &i, @);
        %if %qSysFunc(prxMatch(%qSysFunc(
                prxParse(/^%bQuote([a-zA-Z_]{1}[a-zA-Z_0-9]{0,31})$/)), %quote(&&varOut&i))) ~= 1 %then %do;
            %let glv_returnmsg = %str(Parameter varOut= &&varOut&i. has an invalid variable name, please correct.);
            %goto MacErr;
        %end;
    %end;
%end;

%if "%superQ(varOut)" = "" and not %sysfunc(prxmatch(/\@/, %quote(&varIn))) %then %let varOut=GGLV_&varIn;
%else %if "%superQ(varOut)" = "" and %sysfunc(prxmatch(/\@/, %quote(&varIn))) %then %let varOut=GGLV_%sysfunc(prxchange(s/\@/\@GGLV_/, -1, %quote(&varIn)));;

/* Drive variable */
%if "%superQ(byVar)" ^= "" %then %do;
  proc sort data = &dataIn out = glv_temp;
      by &byvar;
  run;
%end;

%do i = 1 %to %sysfunc(countw(&varIn, @));
  data &dataOut;
      if _N_ = 1 then do;
          dcl hash h(ordered: "a") ;
          h.definekey("GGLV_SEQ");
          h.definedata("GGLV_SEQ", "%scan(&varOut, &i, @)");
          h.definedone();
          dcl hiter hi('h');
          do until(eof);
              set
              %if &i=1 %then %do;
                  %if "%superQ(byVar)" = "" %then &dataIn; 
                  %else glv_temp;
              %end;
              %else &dataOut;
              end = eof;
              %scan(&varOut, &i, @) = %scan(&varIn, &i, @);
              GGLV_SEQ + 1;
              h.add();
          end;
      end;
      set 
      %if &i=1 %then %do;
          %if "%superQ(byVar)" = "" %then &dataIn; 
          %else glv_temp;;
      %end;
      %else &dataOut;;
      hi.setcur(key: _N_);
      GGLV_RC = hi.next();
      if GGLV_RC ^= 0 then call missing(%scan(&varOut, &i, @));
      drop GGLV_SEQ GGLV_RC;
  run;
%end;

/* For BY group*/
%if "%superQ(byVar)" ^= "" %then %do;
    data &dataOut;
        set &dataOut;
        by &byVar; 
        %do i = 1 %to %sysfunc(countw(&varOut, @));
            if last.&glv_byVar_ind then call missing(%scan(&varOut, &i, @));
        %end;
    run;
%end;

%goto macEnd;

/* ERR HANDLING */
%macErr:;

%if "%superQ(glv_returnmsg)" ^= "" %then %do;
    %put ERROR[ELR]: &glv_returnmsg;
    %ABORT CANCEL;
%end;

%macEnd:;

%mend GetLeadValue;

/* Usage *
%GetLeadValue(dataIn = sashelp.class
           , dataOut = demo
           , varIn   = NAME @ AGE
           , varOut  = NAME_LEAD @ AGE_LEAD
           , byVar   = SEX
             );

/*EOP*/