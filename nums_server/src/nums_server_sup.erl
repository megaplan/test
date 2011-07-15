%%% 
%%% nums_server_sup: Nums server application
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
%%% @license MIT
%%% 

-module(nums_server_sup).
-behaviour(supervisor).

%%%-----------------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------------

% supervisor callbacks
-export([start_link/0, init/1]).

%%%-----------------------------------------------------------------------------
%%% supervisor callbacks
%%%-----------------------------------------------------------------------------

-spec start_link() -> pid().
%%
%% Starts supervisor
%% 
start_link() ->
	supervisor:start_link({local, nums_supervisor}, nums_server_sup, []).

-spec init(any()) -> tuple().
%%
%% Returns supervision policy
%% @since 2011-07-15 12:00
%%
init(_Args) ->
    {ok, {
            {one_for_one, 2, 3},
            [{nums, {nums_server, start_link, []},
              permanent, brutal_kill, worker, [nums_server]
            }]
         }
	}.
