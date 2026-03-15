data a;
   set sashelp.class;
   key_obs = _N_ + +1;
   if 0 then set sashelp.class(keep=name rename=(name=name_next_1));
   retain _N_20 1;
   if _N_20 = 1 then do;
       rc20=dosubl("data temp20/view= temp20;  set sashelp.class;  key_obs = _N_ ;  name_next_1=name; run;  ");
       declare hash h20(dataset:"temp20(keep= key_obs name_next_1 )" , duplicate:'E');
       h20.definekey("key_obs");
       h20.definedata("name_next_1");
       h20.definedone();
       _N_20 = 0 ;
   end;
   drop _N_20 ;
   if h20.find() ne 0 then do;
       call missing(of name_next_1 );
   end;
run;

