%%%-------------------------------------------------------------------
%%% Created :  by comptekki May 20, 2010
%%%            (started with yaws/src/ymnesia.erl code 
%%              by Torbjorn Tornkvist <tobbe@tornkvist.org>)
%%% Desc.   : A postgresql table viewer Yaws-app interface.
%%%
%%% @author comptekki
%%%
%%% @doc dbooks is a Yaws appmod to view/search postgresql tables.
%%%      Add dbooks as an appmod to your Yaws configuration.
%%%      
%%%      You can search on any text.
%%%
%%%      To test it, add it as an appmod to you Yaws configuration, e.g:
%%%
%%%          appmods = <dbooks, dbooks>
%%%
%%%      then point your browser to:  http://localhost/dbooks/
%%%
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(dbooks).

-export([out/1]).

-import(lists, [map/2, foldl/3, reverse/1, flatten/1]).

-include("/usr/local/lib/yaws/include/yaws_api.hrl").

-include("pg.hrl").

-define(MAX_LEN, 30).

%%% @private
out(A) ->
	ServerPath=string:strip(A#arg.server_path, both, $/),
	{Client_IP,_Port}=A#arg.client_ip_port,
	io:format("~n~nIP: ~p - Date/Time: ~p~n",[Client_IP, calendar:local_time()]),
	L = yaws_api:parse_query(A),
	case lists:keysearch("tablename", 1, L) of
		{value, {_, "dbooks"}} ->
			{Cbox, Rest0} = extract_cbox(L),
			Offset=lk("offset", Rest0),
			{_, Rest1} = extract_offset(Rest0),
			Rpp=lk("rpp", Rest1),
			{_, Rest2} = extract_rpp(Rest1),
			S=lk("s", Rest2),
			{_, Rest} = extract_s(Rest2),
			Name = "dbooks", %lk("tablename", Rest),
			Ls = select_fields(Rest),
			case Ls of
				[] -> Sp = (catch select_pattern(Name));
				_  -> Sp = (catch select_pattern(Name, Ls, S))
			end,
			SpOffset = Sp ++ " offset " ++ Offset ++ " limit " ++ Rpp,
			table(Cbox, Sp, SpOffset, list_to_integer(Rpp), ServerPath, Ls, S);
		false ->
			case lists:keysearch("hdr", 1, L) of
				 {value, {"hdr", "1"}} ->
				 		 return_top_page(ServerPath, 1);
				 _ ->
				 		 return_top_page(ServerPath, 0)
			end
	end.

%%% Get the fields to be part of the select
select_fields([{"tablename",_}|T]) -> select_fields(T);
select_fields([{_,undefined}|T])   -> select_fields(T);
select_fields([H|T]) ->
	{Field, Value} = H,
	H2 = {Field, string:substr(Value, 1, ?MAX_LEN)}, %% truncate value to MAX_LEN chars
	[H2|select_fields(T)];
select_fields([])                  -> [].


select_pattern(Name) ->
	S="select * from " ++ Name ++ " order by title",
	io:format("~n~s",[S]),
	S.

% postgresql commands:	
% ~~ is equiv to like
% ~~* is equiv to ilike (case insensitive)
% use ! for not

select_pattern(Name, Ls, S) ->
	case S of
		"0" -> Op = " and ";
		"1" -> Op = " or "
	end,
	S2=flatten([fieldq(Op, Field, Value) || {Field, Value} <- Ls]),
	S3="select * from " ++ Name ++ " where " ++ sandor(S2) ++ " order by title",
	io:format("~n~s",[S3]),
	S3.

fieldq(Op, Field, Value) ->
	Sval = sanitize(Value),
	fdq(Sval, Field, Op) ++ "%'".

fdq("---" ++ Rest, Field, Op) ->
	Op ++ Field ++ "::text ~~* '%-"++Rest;	
fdq("-" ++ Rest, Field, _) ->
	" and " ++ Field ++ "::text !~~* '%" ++ Rest;
fdq(Text, Field, Op) ->
	Op ++ Field ++ "::text ~~* '%" ++ Text.	

sandor(" and " ++ Rest) ->
	Rest;
	
sandor(" or " ++ Rest) ->
	Rest.
	
sanitize(S) ->
	S2=re:replace(S,"'","''",[{return,list}, global]),
	S3=re:replace(S2,"\\\\","\\\\\\\\",[{return,list}, global]),
	S5=re:replace(S3,"%","\\\\%",[{return,list}, global]),
	re:replace(S5,"_","\\\\_",[{return,list}, global]).

% 1: inside jQuery-desktop
% _: dbooks stand-alone

return_top_page(ServerPath, Hdr) ->
	
	case Hdr of
		1 ->
			{ehtml, [
				rttp_main(ServerPath, Hdr)
			]};
		_ ->{ehtml,
			 [
			  {html, [],
			   [
				{head, [],
				 [
				  {pre_html,
				     "<META HTTP-EQUIV=\"EXPIRES\" CONTENT=\""
				     "Sun, 16 Oct 2004 11:12:01 GMT\">\n"
				   },
 				   rttp_css()
				  ]},
				  rttp_main(ServerPath, Hdr)
				]}
			  ]}
	end.

rttp_main(ServerPath, Hdr) ->
	
	 [
	  {script, [{type, "text/javascript"}, {src, ?JQUERY}], ""},
	  
	  {script, [{type, "text/javascript"}], "$(document).ready(function() {$('#click_fview').click(function() {$('#s').val(0);$('#fview').show('slow');$('#click_qsview').show('slow');$('#qsview').hide('slow');$('#click_fview').hide('slow');$('#title').focus(); ajfun0();})})"},
	  
	  {script, [{type, "text/javascript"}], "$(document).ready(function() {$('#click_qsview').click(function() {$('#s').val(1);$('#fview').hide('slow');$('#click_qsview').hide('slow');$('#qsview').show('slow');$('#click_fview').show('slow');$('#single_input_dbooks').focus(); ajfun1(); }) })"},
	  
	  {script, [{type, "text/javascript"}], "$(document).ready(function() {$('#single_input_dbooks').focus(); ajfun1();})"},
	  
	  {body, [],mk_table_tab(10,0, ServerPath, Hdr)}
	  ].

rttp_css() ->
	[
                  {style, [{type, "text/css"}],
                   [
                    "body {background-color: #aaaaaa;}\n"
                    "table {border-collapse: collapse; border: solid black 1px; background-color: #cccccc;}\n"
                    "p {padding: 5px; font-weight: bold;}\n"
                    "input[type=text] {vertical-align: bottom; width: 100%; font-size: 80%;}\n"
                    "input[type=checkbox] {vertical-align: top; font-size: 80%;}\n"
                    "span.attribute {vertical-align: top; font-size: 80%;}\n"
                    "th {padding: 5px; border: solid black 1px;}\n"
                    "td {padding: 5px; border: solid black 1px;}\n"
                    "span.hl {padding: 0 2px 0 2px; /* margin: 0 2px 0 2px; */ background-color:yellow; /*color:white;*/}\n"
                    "#fview, #click_qsview {display:none;}\n"
                    ".click {padding: 3px 3px 3px 3px; background-color: #dddddd; color:green; /*width: 245px;*/ }\n"
                    ".click A:link, A:visited {text-decoration: none}\n"
                    ".click A:hover {padding: 0 2px 0 2px; background-color: #eeffee;}\n"
                    ".rows {background-color: #cccccc;}\n"
                    ".record {width:700px; margin:5px; background-color: #dddddd;}\n"
					"/*#rang_input_view_s {display:none;}*/\n"
                    ]}
	 ].

setfields() ->
	"' + '&rpp=' + $('#range_input').val() + "++ " '&offset=' + $('#offset').val() + " ++
    map(
    	fun(Col) -> 
    		" '&"++a2l(Col)++"=' + encodeURIComponent($('#"++a2l(Col)++"').val()) + ($('#cbox_"++a2l(Col)++"').attr('checked')?'&cbox_"++a2l(Col)++"='+ $('#cbox_"++a2l(Col)++"').val():'') + "
    	end,
    	get_columns()) ++ "''".

setfields_single() ->
	"' + '&rpp=' + $('#range_input').val() + '&offset=' + $('#offset').val() + " ++
    " '&title=' + encodeURIComponent($('#single_input_dbooks').val())  + " ++ 
    " '&author_editor=' + encodeURIComponent($('#single_input_dbooks').val())  + " ++ 
    " '&date_of_publication=' + encodeURIComponent($('#single_input_dbooks').val())  + " ++ 
    " '&publisher=' + encodeURIComponent($('#single_input_dbooks').val())  + " ++ 
    " '&key_words=' + encodeURIComponent($('#single_input_dbooks').val())  + " ++ 
    " '&notes=' + encodeURIComponent($('#single_input_dbooks').val())  + " ++ 
    " '&valuation=' + encodeURIComponent($('#single_input_dbooks').val())  + " ++ 
    "'&purchase_price=' + encodeURIComponent($('#single_input_dbooks').val())".

js3a(ServerPath) ->
		{script,
			[{type, "text/javascript"}],
					"
					ajfun0 = function() {
						$('#offset').val(0);
						$.ajax({
							url: '/" ++ ServerPath ++ "',
							type: 'GET',
							data: 'tablename=dbooks&s=0" ++ setfields() ++ ",
							success: function(data) {
								$('#data').html(data) 
							},
							error:function(XMLHttpRequest, textStatus, errorThrown) {
								alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
							}
					  	});
					}
					"
		}.

js3b() ->
	{script,
	 [{type, "text/javascript"}],
	 map(
	   fun(Col) ->
"
				$(document).ready(
					function() {
						$('#" ++ a2l(Col) ++ "').keyup(function(event) {
							if (event.keyCode != 16) {  // ignore shift key -  && event.keyCode !=13
							   ajfun0();
							} // if
					});
				});					
"
	   end,
	   get_columns()
	  )
	}.
		
js4(ServerPath) ->
		{script,
			[{type, "text/javascript"}],
					"
					ajfun1 = function() {
						$('#offset').val(0);
						$.ajax({
							url: '/" ++ ServerPath ++ "',
							type: 'GET',
							data: 'tablename=dbooks&s=1" ++ setfields_single() ++ ",
							success: function(data) {
								$('#data').html(data) 
							},
							error:function(XMLHttpRequest, textStatus, errorThrown) {
						 		alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
							}
						  });
					}
					$(document).ready(
						function() {
							$('#single_input_dbooks').keyup(function(event) {
// ignore windows list 0, return 13, shift 16,  ctr 17, alt 18, windows flag 91
// js 'in' operator requires key:value pair so all keys just have value 0
								if (!(event.keyCode in {0:0,13:0,16:0,17:0,18:0,91:0})) {
								   ajfun1();
								} // if
						});
					});
					"
		}.
		
%%% Build the result page.
table(Cbox, Sp, SpOffset, RowsPerPage, ServerPath, Ls, S) ->
	case do_query(SpOffset) of
		{_, error} ->
				{ehtml, {table, [], {tr, [], {td, [], "Error!  Please try back later."}}}};
        {_, Result} ->
			case do_query(Sp) of
				{_, error} ->
					{ehtml, {table, [], {tr, [], {td, [], "Error!  Please try back later."}}}};
				{_, Res2} ->
					table2(Cbox, RowsPerPage, ServerPath, Ls, S, Result, Res2)
			end
	end.

table2(Cbox, RowsPerPage, ServerPath, Ls, S, Result, Res2) ->
	Count=length(Res2),
	case Count==0 of
		true ->
			{ehtml, {table, [], {tr, [], {td, [], "No Data"}}}};
		_ ->
			Headers=get_columns(),
			Vp = view_pattern(Cbox, map(fun(X) -> a2l(X) end, Headers)),
			Nav={'div', [{id, "nav"}],
				 {table, [],
				  {tr, [],
				   mk_nav(Count, RowsPerPage, ServerPath, S)
				  }
				 }
				},
			{ehtml,
			 [{'div', [],
			   [
				{br},
				{'div', [{id, "riv"}],
				 {table, [],
				  {tr, [],
				   [{td, [],
					  [
"<select id='range_input_view_s'>
<option value='10'>10</option>
<option value='20'>20</option>
<option value='30'>30</option>
<option value='40'>40</option>
<option value='50'>50</option>
<option value='60'>60</option>
<option value='70'>70</option>
<option value='80'>80</option>
<option value='90'>90</option>
<option value='100'>100</option>
</select>
"
					   ,
					{input, [{id,"range_input_view"},{type,"range"}, {name, "range_input_view"}, {min,"10"}, {max,"100"}, {value,RowsPerPage}, {step,"5"}]}]
					},
					{td, [{class,"rows"}],
					 {span, [],
					  ["Show ",
					   {span, [{id,"range_val"}], "10"},
					   " items per page"
					  ]
					 }
					}
				   ]
				  }
				 }},
				{script,
				 [{type, "text/javascript"}],
"
$(document).ready(function() {			
    $('#range_input_view_s').change(function() {
        $('#range_input_view').val($('#range_input_view_s').val());
        $('#range_input_view').change();
        $('#range_input_view').click().mouseup();
    });

    if ($.browser.mozilla)
        $('#range_input_view').hide()
    else
        $('#range_input_view_s').hide()

    $('#range_input_view_s').val($('#range_input_view').val());
    $('#range_val').html($('#range_input_view').val());
    $('#range_input_view').change(
		function() {
			$('#range_val').html($('#range_input_view').val());
	    	$('#range_input').val($('#range_input_view').val());
			$('#offset').val(0);
		//$(':input:text:first:visible').focus()
	    });
	$('#range_input_view').click().mouseup(
		function() {
			$.ajax({
			  url: '/" ++ ServerPath ++ "',
			  type: 'GET',
			  data: 'tablename=dbooks&s=" ++ s_fields(S) ++ ",
			  success: function(data) {
			      $('#data').html(data) 
		          },
			  error:function(XMLHttpRequest, textStatus, errorThrown) {
		          alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
				  }
		    });
	    })
	});"
				},
				{table, [],
				 {tr, [],
				  {td, [],
				   [
					{p, [], "Items Found: "++io_lib:format("~p",[Count])}
				   ]}
				 }
				}
			   ]},
			  {'div', [],
			   [
				Nav
			   ]
			  } |
			  [mk_tab(Vp, Headers, Result, Ls),
			   Nav]
			 ]}
	end.	

s_fields(S) ->
	case S of
		"0" ->
			S ++ setfields();
		"1" ->
			S ++ setfields_single()
	end.
	
mk_nav(Count, RowsPerPage, ServerPath, S) ->
	Ni=Count div RowsPerPage,
	Nd=Count rem RowsPerPage,
	
	case Nd > 0 of
		true -> Nii = Ni + 1;
		_ -> Nii = Ni
	end,
	case Nii > 10 of
		true -> NavL = 10;
		_ -> NavL = Nii
	end,
	build_nav(1, NavL, RowsPerPage, ServerPath, S).

build_nav(Start, End, RowsPerPage, ServerPath, S) ->

	case Start==End of
		false -> 
			[{td, [], {a, [{href, "javascript:void(0);"}, {id, Start},
				{onclick, "$('#offset').val(" ++ io_lib:format("~p",[(Start-1)*RowsPerPage]) ++");
			$.ajax({
				 url: '/" ++ ServerPath ++ "',
				 type: 'GET',
				 data: 'tablename=dbooks&s=" ++ s_fields(S) ++ ",
				 success: function(data) {
					 $('#data').html(data) 
				 },
				 error:function(XMLHttpRequest, textStatus, errorThrown) {
				  alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
				 }
		   });$(':input:text:first').focus();
			
			"}], [io_lib:format("~p",[Start])]}} | build_nav(Start+1,End, RowsPerPage, ServerPath, S)];
		_ -> {td, [], {a, [{href, "javascript:void(0);"}, {id, Start}, 
			{onclick, "$('#offset').val(" ++ io_lib:format("~p",[(Start-1)*RowsPerPage]) ++");
		
			$.ajax({
				 url: '/" ++ ServerPath ++ "',
				 type: 'GET',
				 data: 'tablename=dbooks&s=" ++ s_fields(S) ++ ",
				 success: function(data) {
					 $('#data').html(data) 
				 },
				 error:function(XMLHttpRequest, textStatus, errorThrown) {
				  alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
				 }
		   });	$(':input:text:first').focus();	
		
		"}], [io_lib:format("~p",[Start])]}}
	end.
	
%%% Create a pattern denoting which fields to show in the result.
view_pattern(Cs, L) -> view_pattern(Cs, L, 1).

view_pattern([Cbox|Cs], [Cbox|T], N) -> [N | view_pattern(Cs, T, N+1)];
view_pattern(Cs, [_|T], N)           -> view_pattern(Cs, T, N+1);
view_pattern([], [], _)              -> [].

	
do_query(Sp) ->	
	case pgsql:connect(?HOST, ?USERNAME, ?PASSWORD, [{database, ?DB}]) of
		{error,_} ->
			{Sp, error};
		{ok, Db} -> 
			case pgsql:squery(Db, Sp) of
				{error,_} ->
					{Sp, error};
				{_,_,Res} ->
					pgsql:close(Db),
					{Sp, Res}
			end
	end.

%%% Create a table of: Table | Table-attribute-1 | ... | Table-attribute-N
%%% where each table is a Form

mk_table_tab(RowsPerPage, Offset, ServerPath, Hdr) ->
    [
    {input, [{id, "s"}, {type, "hidden"}, {value, "0"}]},
    {input, [{id, "range_input"}, {type, "hidden"}, {value, RowsPerPage}]},
    {input, [{id, "offset"}, {type, "hidden"}, {value, Offset}]},
    
	case Hdr of
		1 -> "";
		_ ->
			{table, [],
			 {tr, [], 
				{td, [{colspan,"9"}], 
					{p, [{style, "text-align: center; text-transform: uppercase;color: #949610; font-size:1.5em"}], "Duplicate Books"}
				}
			 }
			}
	end,
		{br,[]},

		js3a(ServerPath),
		js3b(),

		{'div', [{id, "click_fview"}, {class, "click"}],
			{a, [{href, "javascript:void(0);"}], ["Click this line to use the Field Search"]}
		},
		js4(ServerPath),
		{'div', [{id, "click_qsview"}, {class, "click"}],
			{a, [{href, "javascript:void(0);"}], ["Click this line to use the Quick Search"]}
		},
		{'div', [{id, "fview"}],
			{table, [],
				{tr, [], 
					[{td, [], "Field<br>Search"} | mk_input_fields()]								
				}
			}						
		},
		{'div', [{id, "qsview"}],
				{table, [],
					{tr, [],
						[{td, [], "Quick<br>Search"},
						{td, [{colspan,"8"}], 
							{input, 
								[
								{id, "single_input_dbooks"}, 
								{name, "single_input_dbooks"},
								{style, "width:500px;"},
								{maxlength, ?MAX_LEN}
								]
							}
						}]
					}
				}
       },
     {'div', [{id, "data"}]}
    ].

%%% create each table cell; consisting of the attribute name and an input field.
mk_input_fields() ->
    As = get_columns(),
    Max = 8,
    map(fun(0) ->
                {td, [], []};
           (Attribute) ->
                A = a2l(Attribute),
                {td, [], 
                 [
                  {span, [{class, "attribute"}], title(A)}, 
                  {input, [{id, A}, {type, "text"}, {name, A}, {maxlength, 30}]}]}
        end, As ++ lists:duplicate(Max-length(As), 0)).


extract_cbox(L) ->
    extract_cbox(L, [], []).

extract_cbox([{"cbox_"++Cbox,_}|T], Cs, Rs) ->
    extract_cbox(T, [Cbox|Cs], Rs);
extract_cbox([H|T], Cs, Rs) ->
    extract_cbox(T, Cs, [H|Rs]);
extract_cbox([], Cs, Rs) ->
    {reverse(Cs), reverse(Rs)}.

extract_s(L) ->
    extract_s(L, [], []).
extract_s([{"s"++S,_}|T], Cs, Rs) ->
    extract_s(T, [S|Cs], Rs);
extract_s([H|T], Cs, Rs) ->
    extract_s(T, Cs, [H|Rs]);
extract_s([], Cs, Rs) ->
    {reverse(Cs), reverse(Rs)}.
    
%
extract_rpp(L) ->
    extract_rpp(L, [], []).

extract_rpp([{"rpp"++Rpp,_}|T], Cs, Rs) ->
    extract_rpp(T, [Rpp|Cs], Rs);
extract_rpp([H|T], Cs, Rs) ->
    extract_rpp(T, Cs, [H|Rs]);
extract_rpp([], Cs, Rs) ->
    {reverse(Cs), reverse(Rs)}.

%
extract_offset(L) ->
    extract_offset(L, [], []).

extract_offset([{"offset"++Offset,_}|T], Cs, Rs) ->
    extract_offset(T, [Offset|Cs], Rs);
extract_offset([H|T], Cs, Rs) ->
    extract_offset(T, Cs, [H|Rs]);
extract_offset([], Cs, Rs) ->
    {reverse(Cs), reverse(Rs)}.
    
%%% Build the result table.
mk_tab(Vp, Headers, Rows, Ls) ->
	Hdrs=[{th, [{style, "width:175px; text-align:right; vertical-align:top;"}], title(a2l(X))} || X <- vp(Vp,Headers)],
    [{'div', [],
      [{table, [{class,"data"}],
        [
        {tr, [],
          [

         map(fun(RowTuple) ->

		   [_|Row]=tuple_to_list(RowTuple),

         	{table, [{class, "record datat"}],
         		[
         		thtd(Hdrs, [{td, [], massage(A,Ls,I)} || {A,I} <- lists:zip(vp(Vp,Row), lists:seq(1,length(Row)))])	 
                ]}

			end, Rows)

        	]}]

      }]
    }].

thtd([TH|THR], [TD|TDR]) ->
	[{tr, [], [TH, TD]}|thtd(THR, TDR)];
thtd([],[]) -> [].
	
%%% Match the view pattern to select which entries to let through.
vp([], L) -> L;
vp(Vp, L) -> vp(Vp, L, 1).

vp([N|Vp], [H|T], N) -> [H|vp(Vp, T, N+1)];
vp(Vp, [_|T], N)     -> vp(Vp, T, N+1);
vp([], [], _)        -> [].

massage(A,Ls,I) ->
	case is_integer(A) of
		true ->
			io_lib:format("~w", [A]);
		false ->
			A2=hl(A,Ls,I),
			io_lib:format("~s", [A2])
	end.

escape([H|T]) ->
	case H of
		35 -> H2 = "\\#";
		36 -> H2 = "\\$";
		38 -> H2 = "\\&";
		40 -> H2 = "\\(";
		41 -> H2 = "\\)";
		42 -> H2 = "\\*";
		43 -> H2 = "\\+";
		46 -> H2 = "\\.";
		63 -> H2 = "\\?";
		91 -> H2 = "\\[";
		92 -> H2 = "\\\\";
		94 -> H2 = "\\^";
		124 -> H2 = "\\|";
		_	-> H2 = H
	end,
	[H2|escape(T)];
escape([]) ->
	[].
	
chk_dash("---"++Rest) ->
	{"---",Rest};
chk_dash(Any) ->
	Any.

hl(A,[Lsf|Lsr],I) ->
	{Field, Lsv2}= Lsf, %% Lsvue = Lsv unescaped

	Lsv =
		case chk_dash(Lsv2) of
			{"---",Rest} -> "-"++escape(Rest);
			_ ->	escape(Lsv2)
		end,
	Rv =
		case length(Lsv) of
			0 -> "";
			_ ->
				"<span class='hl'>" ++ string:to_upper(Lsv) ++ "</span>"
		end,
	case Field of
		"title" ->
			case I == 1 of
				true ->
					case A /= null of
						true -> re:replace(A, Lsv, Rv, [{return,list}, global, caseless]);
						false -> []
					end;
				_ ->
					hl(A,Lsr,I)
			end;
		"author_editor" ->
			case I == 2 of
				true ->
					case A /= null of
						true -> re:replace(A, Lsv, Rv, [{return,list}, global, caseless]);
						false -> []
					end;
				_ ->
					hl(A,Lsr,I)
			end;
		"date_of_publication" ->
			case I == 3 of
				true ->
					case A /= null of
						true -> re:replace(A, Lsv, Rv, [{return,list}, global, caseless]);
						false -> []
					end;
				_ ->
					hl(A,Lsr,I)
			end;
		"publisher" ->
			case I == 4 of
				true ->
					case A /= null of
						true -> re:replace(A, Lsv, Rv, [{return,list}, global, caseless]);
						false -> []
					end;
				_ ->
					hl(A,Lsr,I)
			end;
		"key_words" ->
			case I == 5 of
				true ->
					case A /= null of
						true -> re:replace(A, Lsv, Rv, [{return,list}, global, caseless]);
						false -> []
					end;
				_ ->
					hl(A,Lsr,I)
			end;
		"notes" ->
			case I == 6 of
				true ->
					case A /= null of
						true -> re:replace(A, Lsv, Rv, [{return,list}, global, caseless]);
						false -> []
					end;
				_ ->
					hl(A,Lsr,I)
			end;
		"valuation" ->
			case I == 7 of
				true ->
					case A /= null of
						true -> re:replace(A, Lsv, Rv, [{return,list}, global, caseless]);
						false -> []
					end;
				_ ->
					hl(A,Lsr,I)
			end;
		"purchase_price" ->
			case I == 8 of
				true ->
					case A /= null of
						true -> re:replace(A, Lsv, Rv, [{return,list}, global, caseless]);
						false -> []
					end;
				_ ->
					hl(A,Lsr,I)
			end
	end;

hl(A,[],_) ->
	A.

get_columns() -> %%Table) ->
	["title", "author_editor", "date_of_publication", "publisher", "key_words", "notes", "valuation", "purchase_price"].
	
a2l(A) when is_atom(A) -> atom_to_list(A);
a2l(L) when is_list(L) -> L.

lk(Key, L) ->
   case lists:keysearch(Key, 1, L) of
       {value,{_,Val}} ->
       		Val;
       false -> 
			case Key of
				"offset" -> "0";
				"rpp" -> "10";
				_ -> ""
			end
   end.

title(String) ->
   {Title, _} = lists:mapfoldl(fun f/2, 'sep', String),
   Title.

f($_, _) -> 
	{$ , 'sep'};
f(C, 'sep') -> 
	{string:to_upper(C), 'char'};
f(C, _) -> 
	{C, 'char'}.
