%%%-----------------------------------------------------------------
%%% functions to process arrived numbers
%%%-----------------------------------------------------------------
-module(store_num).
-export([store_num/2]).
-include("nums.hrl").
%-------------------------------------------------------------------
% @doc either adds arrived number into current sequence
% or updates storage with current sequence and starts new sequence
-spec store_num(#nums{}, non_neg_integer()) -> #nums{}.

store_num(St, N) ->
	C = St#nums.cur,
	case C#cur.beg of
		Beg when is_integer(Beg), N == Beg + C#cur.len ->
			New_cur = C#cur{len = C#cur.len + 1},
			p_debug:p("~p:store_num:~p new cur ~p~n", [?MODULE, ?LINE, New_cur], St#nums.debug, run, 4),
			New_st = St#nums{cur = New_cur};
		Beg when is_integer(Beg) ->
			New_st = update_seqs(St, N);
		_ -> % at the very beginning
			p_debug:p("~p:store_num:~p init cur~n", [?MODULE, ?LINE], St#nums.debug, run, 4),
			New_cur = #cur{beg=N, len=1},
			New_st = St#nums{cur = New_cur, amount = 1}
	end,
	New_st
.
%-------------------------------------------------------------------
% @doc it's called when old sequence stops and new sequence begins
-spec update_seqs(#nums{}, non_neg_integer()) -> #nums{}.

update_seqs(#nums{amount = Amount, n = Limit} = St, N) when Amount < Limit+1 ->
	update_cur(St, N)
;
update_seqs(St, N) ->
	log_storage_info(St, 'before_add'),
	Upd = update_cur(St, N),
	log_storage_info(St, 'after_add'),
	Rst = squeeze_storage(Upd),
	log_storage_info(St, 'after_squeeze'),
	Rst
.
%-------------------------------------------------------------------
% @doc adds current sequence into storage, makes new current sequence
-spec update_cur(#nums{}, non_neg_integer()) -> #nums{}.
update_cur(St, N) ->
	Ast = add_item(St),
	New_cur = #cur{beg=N, len=1},
	Ast#nums{cur = New_cur}
.
%-------------------------------------------------------------------
% @doc creates key for storage based on length of sequence
-spec make_key(#cur{}) -> non_neg_integer().

make_key(#cur{len = Len}) ->
	Len
.
%-------------------------------------------------------------------
% @doc adds sequence into storage
-spec add_item(#nums{}) -> #nums{}.

add_item(St) ->
	Cur = St#nums.cur,
	p_debug:p("~p:add_item:~p cur ~p~n", [?MODULE, ?LINE, Cur], St#nums.debug, run, 4),
	Key = make_key(Cur),
	case ets:lookup(?TAB, Key) of
		[#data{data = List} | _] ->
			Item = #data{key=Key, data=[Cur | List]};
		_ ->
			Item = #data{key=Key, data=[Cur]}
	end,
	p_debug:p("~p:add_item:~p item ~p~n", [?MODULE, ?LINE, Item], St#nums.debug, run, 5),
	insert_item(Item),
	New_amount = St#nums.amount + 1,
	p_debug:p("~p:add_item:~p new amount=~p~n", [?MODULE, ?LINE, New_amount], St#nums.debug, run, 4),
	update_min_len(St#nums{amount = New_amount})
.
%-------------------------------------------------------------------
% @doc removes unnecessary sequences from storage, starting with
% shortest ones.
-spec squeeze_storage(#nums{}) -> #nums{}.

squeeze_storage(#nums{amount = Amount, n = Limit} = St) when Amount < Limit+2 ->
	p_debug:p("~p:squeeze_storage:~p am (~p) < lim (~p)+2~n", [?MODULE, ?LINE, Amount, Limit], St#nums.debug, run, 5),
	St
;
squeeze_storage(#nums{amount = Amount, n = Limit} = St) ->
	Key0 = ets:first(?TAB),
	p_debug:p("~p:squeeze_storage:~p am=~p, lim=~p, key=~p~n", [?MODULE, ?LINE, Amount, Limit, Key0], St#nums.debug, run, 5),
	New_amount = squeeze(St, Amount, Limit, Key0),
	St#nums{amount = New_amount}
.
%-------------------------------------------------------------------
% @doc iterates over storage, removing shortest sequences. Keeps Limit+1
% sequences, bearing in mind that adding 'cur' leads to Limit+2.
-spec squeeze(#nums{}, non_neg_integer(), non_neg_integer(), atom()) ->
	non_neg_integer().

squeeze(St, Amount, _, '$end_of_table') ->
	p_debug:p("~p:squeeze:~p end_of_table, am=~p~n", [?MODULE, ?LINE, Amount], St#nums.debug, run, 5),
	Amount
;
squeeze(St, Amount, Limit, _) when Amount < Limit+2 ->
	p_debug:p("~p:squeeze:~p amount (~p) < limit (~p)+2~n", [?MODULE, ?LINE, Amount, Limit], St#nums.debug, run, 5),
	Amount
;
squeeze(St, Amount, Limit, Key) ->
	Delta = Amount - (Limit + 1),
	List = fetch_data_list(Key),
	p_debug:p("~p:squeeze:~p am=~p, lim=~p, d=~p, list~n~p~n", [?MODULE, ?LINE, Amount, Limit, Delta, List], St#nums.debug, run, 6),
	case List of
		_ when Delta == 0 ->
			p_debug:p("~p:squeeze:~p delta 0~n", [?MODULE, ?LINE], St#nums.debug, run, 7),
			New = List,
			New_amount = Amount;
		[_ | T] when Delta == 1 ->
			p_debug:p("~p:squeeze:~p delta 1~n", [?MODULE, ?LINE], St#nums.debug, run, 7),
			New = T,
			New_amount = Amount - 1;
		[_ | _] when Delta =< length(List) ->
			p_debug:p("~p:squeeze:~p 1, d=~p, len=~p~n", [?MODULE, ?LINE, Delta, length(List)], St#nums.debug, run, 7),
			New = lists:nthtail(Delta, List),
			New_amount = Amount - Delta;
		[_ | _] ->
			Len = length(List),
			p_debug:p("~p:squeeze:~p 2, d=~p, len=~p~n", [?MODULE, ?LINE, Delta, Len], St#nums.debug, run, 7),
			New = [],
			New_amount = Amount - Len;
		_ ->
			p_debug:p("~p:squeeze:~p 3~n", [?MODULE, ?LINE], St#nums.debug, run, 7),
			New = [],
			New_amount = Amount
	end,
	Item = #data{key=Key, data=New},
	p_debug:p("~p:squeeze:~p am=~p, item ~p~n", [?MODULE, ?LINE, New_amount, Item], St#nums.debug, run, 5),
	insert_item(Item),
	New_key = ets:next(?TAB, Key),
	squeeze(St, New_amount, Limit, New_key)
.
%-------------------------------------------------------------------
% @doc inserts bunch of sequences into storage. Clears storage place if
% bunch is empty.
-spec insert_item(#data{}) -> true.

insert_item(#data{key=Key, data=[]}) ->
	ets:delete(?TAB, Key)
;
insert_item(Item) ->
	ets:insert(?TAB, Item)
.
%-------------------------------------------------------------------
% @doc extracts bunch of sequences with same length from storage
-spec fetch_data_list(any()) -> list().

fetch_data_list(Key) ->
	case ets:lookup(?TAB, Key) of
		[H | _] ->
			H#data.data;
		_ ->
			[]
	end
.
%-------------------------------------------------------------------
% @doc prints general storage info to log
-spec log_storage_info(#nums{}, atom()) -> any().

log_storage_info(C, Tag) ->
	Info = ets:info(?TAB),
	p_debug:p("~p:log_storage_info:~p storage info ~p~n~p~n", [?MODULE, ?LINE, Tag, Info], C#nums.debug, store, 3),
	p_debug:p_ets("~p:log_storage_info:~p storage info ~p~n", [?MODULE, ?LINE, Tag], C#nums.debug, ets, 3, ?TAB)
.
%-------------------------------------------------------------------
% @doc gets minimal length of sequence
-spec update_min_len(#nums{}) -> #nums{}.

update_min_len(St) ->
	Len = ets:first(?TAB),
	St#nums{min = Len}
.
%-------------------------------------------------------------------
