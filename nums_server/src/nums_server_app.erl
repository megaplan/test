%%% 
%%% nums_server: Nums server application
%%% 
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author arkdro <arkdro@gmail.com>
%%% @sinse 2011-07-15 10:00
%%% @doc 
%%% 

-module(nums_server_app).
-behaviour(application).

%%%-----------------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------------

% application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%%%-----------------------------------------------------------------------------
%%% application callbacks
%%%-----------------------------------------------------------------------------

-spec start() -> pid().
%%
%% Starts the nums server supervisor and writes log
%% @sinse 2011-07-15 12:00
%%
start() ->
    Res = nums_server_sup:start_link(),
    error_logger:info_msg("app start res:~n~p~n", [Res]),
    Res.

-spec start(any(), any()) -> pid().
start(_Type, _Args) ->
    start().
%%------------------------------------------------------------------------------

-spec stop(any()) -> ok.
%%
%% Stops the application
%% @since 2011-07-15 12:00
%%
stop(_State) ->
	ok.

-spec stop() -> ok.
stop() ->
	ok.
%%------------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Public API
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
