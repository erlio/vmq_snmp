%% Copyright 2014 Erlio GmbH Basel Switzerland (http://erl.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(vmq_snmp).
-export([start/0,
         stop/0,
         change_config/1]).

-define(REPORTER, vmq_report_snmp).

start() ->
    SNMPConfig0 = application:get_all_env(vmq_snmp),
    SNMPConfig = fix_paths(SNMPConfig0),
    MIBTemplate = proplists:get_value(snmp_mib_template_file, SNMPConfig),
    MIBDir = proplists:get_value(snmp_mib_dir, SNMPConfig),
    start_snmp(SNMPConfig),
    application:ensure_all_started(vmq_snmp),
    Opts = [{mib_template, MIBTemplate},
            {mib_dir, MIBDir}],
    start_reporter(Opts).


stop() ->
    application:stop(vmq_snmp),
    application:stop(snmp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hooks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
change_config(Config) ->
    case lists:keyfind(vmq_snmp, 1, application:which_applications()) of
        false ->
            %% vmq_snmp app is loaded but not started
            ok;
        _ ->
            %% vmq_snmp app is started
            {vmq_snmp, SNMPConfig0} = lists:keyfind(vmq_snmp, 1, Config),
            SNMPConfig = fix_paths(SNMPConfig0),
            MIBTemplate = proplists:get_value(snmp_mib_template_file, SNMPConfig),
            MIBDir = proplists:get_value(snmp_mib_dir, SNMPConfig),
            start_snmp(SNMPConfig),
            Opts = [{mib_template, MIBTemplate},
                    {mib_dir, MIBDir}],
            start_reporter(Opts)
    end.

start_snmp(SNMPConfig0) ->
    application:load(snmp),
    SNMPConfig = fix_paths(SNMPConfig0),
    AgentConf = proplists:get_value(snmp_agent, SNMPConfig),
    %% ensure snmp is stopped
    application:stop(snmp),
    application:set_env(snmp, agent, AgentConf),
    application:ensure_all_started(snmp).

start_reporter(Opts) ->
    exometer_report:disable_reporter(?REPORTER),
    exometer_report:remove_reporter(?REPORTER),
    exometer_report:add_reporter(?REPORTER, Opts).


fix_paths(Conf) ->
    fix_paths(Conf, []).
fix_paths([{Key, [{A, _}|_] = SubConf}|Rest], Acc) when is_atom(A) ->
    fix_paths(Rest, [{Key, fix_paths(SubConf, [])}|Acc]);
fix_paths([{Key, Val} = Item|Rest], Acc) ->
    LKey = atom_to_list(Key),
    case {lists:suffix("dir", LKey),
          lists:suffix("file", LKey)}
    of
        {true, false} ->
            %% check if the dir exists
            fix_paths(Rest, [{Key, fix_path(Val)}|Acc]);
        {false, true} ->
            fix_paths(Rest, [{Key, fix_file(Val)}|Acc]);
        {false, false} ->
            fix_paths(Rest, [Item|Acc])
    end;

fix_paths([], Acc) -> Acc.

fix_path(Path) ->
    fix_path(Path, false).
fix_path(Path, Retry) ->
    case filelib:is_dir(Path) of
        true ->
            Path;
        false when Retry ->
            Path;
        false ->
            MyPath = code:lib_dir(?MODULE),
            NewPath = filename:join(MyPath, Path),
            fix_path(NewPath, true)
    end.

fix_file(File) ->
    fix_file(File, false).
fix_file(File, Retry) ->
    case filelib:is_file(File) of
        true ->
            File;
        false when Retry ->
            File;
        false ->
            MyPath = code:lib_dir(?MODULE),
            NewFile = filename:join(MyPath, File),
            fix_file(NewFile, true)
    end.



