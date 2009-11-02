-module(chat).
-export([init/1]).
-export([split/2]).
-export([process_params/1]).
-export([parse_json_file/1]).
-export([write_json_file/2]).
%-export([parse_json/1]).

%query record
-record(qry,{
  type,
  params
}).

% message structure
-record(message, {
  type, % e.g. sendmessage, findname ...
  name,
  myname,
  message,
  ip,   %peer's ip
  ttl
}).

init(Port) ->
  case gen_tcp:listen(Port, [binary, {packet,http}, {reuseaddr,true},{active,false}]) of
    {ok, Listen} ->
      spawn(fun() -> listen(Listen) end);
    {error, Reason} ->
      {stop, Reason}
  end.
    

listen(Listen) ->

  {ok, Socket} = gen_tcp:accept(Listen),
  Pid = spawn(fun() -> handler(Socket) end),
  register(http_server, Pid),
  listen(Listen),
  
  receive
    kill -> 
      gen_tcp:close(Listen),
      exit(error, "Killed")
  end.

% We need only GET requests...
handler(Socket) ->
  case gen_tcp:recv(Socket,0) of
    {ok, Data} ->
      io:format("sock: ~p~n",[Socket]),
      case Data of
        {http_request, 'GET', Query, _} ->
          {abs_path, Req} = Query,
          Params = parse_params(Req),
          
          % put message to nice data structure
          M = process_params(Params),
          
          % io:format("Name: ~p~nType: ~p~n",[M#message.name, M#message.type])
          
          % proccess message
          process_message(M, Socket)
      end,
           
      gen_tcp:close(Socket);
    {error, closed} ->
      closed
  end.

% string:tokens(Str, "limiter") BIF is better solution...
split(String,Delimiter) ->
  split(String,[],[],Delimiter).
  
split(String, Token, Res, Delimiter) ->
  case String of
    [Delimiter|T] ->
      split(T, [], [lists:reverse(Token)|Res], Delimiter);
    [H|T] ->
      split(T,[H|Token], Res, Delimiter);
    [] ->
      lists:reverse([lists:reverse(Token)|Res])
  end.

parse_params(Query) ->
  [Type|Qarr] = split(Query,$?),
  
  case Type of
    "/chat/findname" ->
      Command = findname;
      
    "/chat/sendname" ->
      Command = sendname;
      
    "/chat/asknames" -> 
      Command = asknames;
    
    "/chat/sendmessage" ->
      Command = sendmessage;
    _ ->
      Command = undefined
  end,
    
  
  case Qarr of
    [Qstring|_] ->
      % key=value string tokens
      Tokens = split(Qstring, $&),
      %[{type, Command}, gen_params_struct(Tokens,[])];
      #qry{type = Command, params = gen_params_struct(Tokens,[])};
    _ ->
      []
  end.

gen_params_struct([],Res)->
  Res;  
  
gen_params_struct([H|T], Res) ->
  Keyval = split(H,$=),
  case Keyval of 
    [Key|Rest] ->
      case Rest of
        [] ->
          Val = "";
        _ ->
          [Val|_] = Rest
      end,
      gen_params_struct(T, [{list_to_atom(Key),Val} | Res]);
    _ ->
      Res
  end.

process_params([]) ->
  #message{type=undefined};

process_params(Params) ->
  io:format("params: ~p~n",[Params]),
  Type = Params#qry.type,
  M = #message{type = Type},
  process_params(Params#qry.params, M).

process_params([],M) ->
  M;

process_params([Param|Params],M) ->
  case Param of
    {name, Name} ->
      Message = M#message{name=Name};
    {myname, Myname} ->
      Message = M#message{myname=Myname};
    {ip, Ip} ->
      Message = M#message{ip=Ip};
    {ttl, Ttl} ->
      Message = M#message{ttl=Ttl};
    {message, Msg} ->
      Message = M#message{message=Msg};
    _ ->
      Message = M
  end,
  
  process_params(Params, Message).
  
process_message(M, Socket) ->
  case M#message.type of 
    findname ->
      ok;
    sendname ->
      ok;
    sendmessage ->
      io:format("~p> ~p~n",[M#message.myname, M#message.message]);
    undefined ->
      error
  end.
  
parse_json_file(Filename) ->
  Environ = [],
  case file:read_file(Filename) of
    {ok, Binary} ->
      %Lines =  string:tokens(erlang:binary_to_list(Binary), "\n"),
      %Lines;
      Data = "Names = " ++ binary_to_list(Binary) ++ ".",
      {ok, Scanned, _} = erl_scan:string(Data),
      
      case erl_parse:parse_exprs(Scanned) of
        {ok, Parsed} ->
          Res = erl_eval:exprs(Parsed,Environ),
          
          case Res of 
            {_, Names, _} ->
              Names;
            _ ->
              []
          end;
          
        {error, Reason} ->
          io:format("Err: ~p~n", [Reason])
      end;
      
    {error, Why} ->
      io:format("Error opening file: ~p~n", [Why])
  end.
  
write_json_file(Filename, Data) ->
  case file:open(Filename, write) of
    {ok, File} ->
      % normal way
      io:format(File, "~p", [Data]);
    {error, Why} ->
      io:write("Error: ~p~n", [Why])
  end.

