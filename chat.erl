-module(chat).
-export([init/1]).
-export([split/2]).
-export([stop_server/0]).
-export([process_params/1]).
-export([parse_json_file/1]).
-export([write_json_file/2]).
-export([find_name/1]).
-export([send_message/1]).
-export([findchat/1]).
%-export([parse_json/1]).

-define(not_implemented_501, "HTTP/1.1 501 Not Implemented\r\n\r\n").
-define(forbidden_403, "HTTP/1.1 403 Forbidden\r\n\r\n").
-define(not_found_404, "HTTP/1.1 404 Not Found\r\n\r\n").
-define(ok_200, "HTTP/1.1 200 OK\r\n\r\n").
-define(DEBUG(Value), io:format("*** debug : ~p~n", [Value])).

%query record
-record(qry,{
  type,
  params
}).

% message structure
-record(message, {
  type, % e.g. sendmessage, find_name ...
  name,
  myname,
  myip,
  message,
  ip,   %peer's ip
  ttl,
  port = 7000
}).

init(Port) ->
  case gen_tcp:listen(Port, [binary, {packet,http}, {reuseaddr,true},{active,false}]) of
    {ok, Listen} ->
      ChatPid = chats(),
      register(chatproc, ChatPid),
      Pid = spawn_link(fun() -> listen(Listen) end),
      register(server, Pid),
      ok;
    {error, _} ->
      io:format("Some error...")
  end.   

listen(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> handler(Socket) end),
  listen(Listen).

stop_server() ->
  exit(server).

% We need only GET requests...
handler(Socket) ->
  ?DEBUG("HANDLER"),
  case gen_tcp:recv(Socket,0) of
    {ok, Data} ->
      case Data of
        {http_request, 'GET', Query, _} ->
          {abs_path, Req} = Query,
          Params = parse_params(Req),

          % put message to nice data structure
          M = process_params(Params),

          % proccess message
          process_message(M, Socket)
      end;
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
    "/chat/find_name" ->
      Command = find_name;
      
    "/chat/sendname" ->
      Command = sendname;
      
    "/chat/sendnames" -> 
      Command = sendnames;
    
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
    [Key,Val|_Rest] ->
      gen_params_struct(T, [{list_to_atom(Key),Val} | Res]);
    _ ->
      Res
  end.

process_params([]) ->
  #message{type=undefined};

process_params(Params) ->
  ?DEBUG([Params]),
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
    find_name ->
      Name = find_name(M#message.name),
      case Name of
        {ok, Data} ->
          [N,IP|_Rest] = Data,
          ?DEBUG(N),
          ?DEBUG(IP);
        notfound ->
         % send request to every known host
         error
      end;
      
    sendname ->
      ok;
    sendnames ->
      case file:read_file("names.json") of
        {ok, Binary} ->
          List = binary_to_list(Binary),
          Len = length(List),
          gen_tcp:send(Socket,
          "HTTP/1.1 200 OK\r\n" ++ 
          "Content-length: " ++ integer_to_list(Len) ++ "\r\n" ++
          "Content-type: text/plain" ++ "\r\n\r\n" ++ Binary);
        _ ->
          error
      end;
    sendmessage ->
      gen_tcp:send(Socket, ?ok_200),
      io:format("~p> ~p~n",[M#message.myname, M#message.message]),
      Chat = {M#message.myname, M#message.ip, M#message.message},

      case add_chat(chatproc, Chat) of
        ok ->
          ?DEBUG("CHAT ADDED"),
          Chats = get_chat_list(chatproc),
          io:format("Chats: ~p~n", [Chats]);
        _ ->
          ?DEBUG("ERROR ADDING CHAT")
      end;
    undefined ->
      error
  end,
  gen_tcp:close(Socket).
  
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
  
find_name(Name) ->
  Json = parse_json_file("names.json"),
  find_name(Name,Json).
  
find_name(_Name,[]) ->
  notfound;

find_name(Name,[J|Json]) ->
  case J of
    [Name|_IP] ->
      {ok, J};
    _ ->
      find_name(Name,Json)
  end.
  
send_message(M) ->
  case M#message.type of
    sendmessage ->
      %http:request(get, {Addr,[{"connection", "close"}]},[],[]),
      ok;
    _ ->
      io:format("boooo!!!~n")
    
  end.

% add initiated chats, so it is easrier to replay
% return such datastruct that we can direcly send_message
chats() ->
  spawn_link(fun() -> chats([]) end).

% todo: addchat and getchat can use same function?? 
chats(Res) ->   
  receive
    {addchat, Caller, Chat} ->
       Caller ! ok,
       {Nam,_,_} = Chat,
       % check if name exist in initiated chats
       Result = [Find || {Find,_,_} <- Res, compare(Nam,Find)],
       case Result of
         [] ->
           chats([Chat|Res]);
         _ ->
           ?DEBUG("Updating chat data..."),
           % now replace existing chat... 
           % N - name, I - IP, M - message
           NewRes = [{N, I, M} || {N, I, M} <- Res, not compare(Nam, N)],
           chats([Chat|NewRes])
       end;
    {getchat, Caller, Name} ->
      Chat = findchat(Name),
      % case chat of later...
      Caller ! {ok, Chat},
      chats(Res);

    {deletechat, Caller, _Index} ->
      Caller ! ok,
      chats(Res);
    {getlist, Caller} ->
      Caller ! {ok, Res},
      chats(Res);
    {_, Caller} ->
      Caller ! undefined,
      chats(Res)
  end.

findchat(Name) ->
  chatproc ! {getlist, self()},
  receive
    {ok, Chats} ->
      [{N, I, M} || {N,I,M} <- Chats, compare(Name,N)]
  end.
compare(A,B) ->
  if 
    A==B -> true; 
    true-> false
  end.

add_chat(Pid, Chat) ->
  Pid ! {addchat, self(), Chat},
  receive
    ok ->
      ok;
    undefined ->
      error
  end.

get_chat_list(Pid) ->
  Pid ! {getlist, self()},
  receive
    {ok, Chat} ->
      Chat;
    _ ->
      chat
  after 
    2000 ->
      timeout
  end.
