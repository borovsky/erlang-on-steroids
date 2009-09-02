%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc  
%%% ErlyDTL template compiler
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-12-16 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_compiler).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').
-author('alex.borovsky@gmail.com').

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-export([compile/2, compile/3]).

-record(dtl_context, {
          local_scopes = [], 
          auto_escape = off, 
          parse_trail = [],
          debug = false,
          reader = {file, read_file},
          module = [],
          renderer_module = erlydtl_renderer,
          renderer_params,
          compiler_options = [verbose, report_errors],
          write_erl_to = none}).

compile(Binary, Module) when is_binary(Binary) ->
    compile(Binary, Module, []);

compile(File, Module) ->
    compile(File, Module, []).

compile(Binary, Module, Options) when is_binary(Binary) ->
    File = "",
    case parse(Binary) of
        {ok, DjangoParseTree} ->
            case compile_to_binary(File, DjangoParseTree, 
                    init_dtl_context(File, Module, Options)) of
                {ok, Module1, _} ->
                    {ok, Module1};
                Err ->
                    Err
            end;
        Err ->
            Err
    end;
    
compile(File, Module, Options) ->  
    Context = init_dtl_context(File, Module, Options),
    case parse(File, Context) of  
        {ok, DjangoParseTree} ->
            case compile_to_binary(File, DjangoParseTree, Context) of
                {ok, Module1, Bin} ->
                    case proplists:get_value(out_dir, Options) of
                        undefined ->
                            ok;
                        OutDir ->
                            BeamFile = filename:join([OutDir, atom_to_list(Module1) ++ ".beam"]),
                            case file:write_file(BeamFile, Bin) of
                                ok ->
                                    ok;
                                {error, Reason} ->
                                    {error, lists:concat(["beam generation failed (", Reason, "): ", BeamFile])}
                            end
                    end;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.
    

%%====================================================================
%% Internal functions
%%====================================================================

compile_to_binary(File, DjangoParseTree, Context) ->
    try body_ast(DjangoParseTree, Context) of
        Ast ->
            %io:format("Ast for ~p: ~p~n", [Context#dtl_context.module, Ast]),
            Forms = forms(Context#dtl_context.module, Ast),
            case Context#dtl_context.write_erl_to of
                none -> none;
                ErlFile -> 
                    file:write_file(ErlFile ++ ".dump", io_lib:format("~p~n", [Forms])),
                    Res = lists:map(fun(Par) -> [erl_prettypr:format(Par), "\n"] end, Forms),
                    file:write_file(ErlFile, Res)
            end,
            case compile:forms(Forms, 
                    Context#dtl_context.compiler_options) of
                {ok, Module1, Bin} -> 
                    code:purge(Module1),
                    case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
                        {module, _} -> {ok, Module1, Bin};
                        _ -> {error, lists:concat(["code reload failed: ", Module1])}
                    end;
                error ->
                    {error, lists:concat(["compilation failed: ", File])};
                OtherError ->
                    OtherError
            end
    catch 
        throw:Error -> Error
    end.
                
init_dtl_context(File, Module, Options) when is_list(Module) ->
    init_dtl_context(File, list_to_atom(Module), Options);
init_dtl_context(File, Module, Options) ->
    Ctx = #dtl_context{},
    #dtl_context{
        parse_trail = [File], 
        module = Module,
        debug = proplists:get_value(debug, Options, Ctx#dtl_context.debug), 
        reader = proplists:get_value(reader, Options, Ctx#dtl_context.reader),
        compiler_options = proplists:get_value(compiler_options, Options, Ctx#dtl_context.compiler_options),
        renderer_params = proplists:get_value(renderer_params, Options, none), 
        renderer_module = proplists:get_value(renderer_module, Options, erlydtl_renderer), 
        write_erl_to = proplists:get_value(write_erl_to, Options, Ctx#dtl_context.write_erl_to)}.


parse(File, Context) ->  
    {M, F} = Context#dtl_context.reader,
    case catch M:F(File) of
        {ok, Data} ->
            case parse(Data) of
                {error, Msg} when is_list(Msg) ->
                    {error, File ++ ": " ++ Msg};
                Result ->
                    Result
            end;
        _ ->
            {error, "reading " ++ File ++ " failed "}
    end.
        
parse(Data) ->
    case erlydtl_scanner:scan(binary_to_list(Data)) of
        {ok, Tokens} ->
            erlydtl_parser:parse(Tokens);
        Err ->
            Err
    end.        
  
forms(Module, BodyAst) ->
    Render0FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([], none, [erl_syntax:application(none, 
                        erl_syntax:atom(render), [erl_syntax:list([])])])]),
    Function2 = erl_syntax:application(none, erl_syntax:atom(render2), 
        [erl_syntax:variable("Variables")]),
    ClauseOk = erl_syntax:clause([erl_syntax:variable("Val")], none,
        [erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable("Val")])]),     
    ClauseCatch = erl_syntax:clause([erl_syntax:variable("Err")], none,
        [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable("Err")])]),            
    Render1FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([erl_syntax:variable("Variables")], none, 
            [erl_syntax:try_expr([Function2], [ClauseOk], [ClauseCatch])])]),  

    RenderInternalFunctionAst = erl_syntax:function(
        erl_syntax:atom(render2), 
            [             
             erl_syntax:clause([erl_syntax:variable("Variables")], none, 
                [BodyAst])]),   
    
    ModuleAst  = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(1))])]),


    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, Render0FunctionAst,
            Render1FunctionAst, RenderInternalFunctionAst]].    

        
% child templates should only consist of blocks at the top level
body_ast([{extends, File} | ThisParseTree], Context) ->
    AddBlocksAsts = lists:foldl(
                      fun
                          ({block, {identifier, _, BlockName}, Contents}, Ast) ->
                              erl_syntax:application(erl_syntax:atom(Context#dtl_context.renderer_module),
                                           erl_syntax:atom(set_block),
                                                      [erl_syntax:string(BlockName), 
                                           body_ast(Contents, Context), Ast]);
                          (_, Ast) ->
                              Ast
                      end, erl_syntax:variable("Variables"), ThisParseTree),
    
    FileAst = resolve_variable_or_value_ast(File, Context),
    include_ast(FileAst, AddBlocksAsts, Context);
 
    
body_ast(DjangoParseTree, Context) ->
    AstInfoList = lists:map(
        fun
            ({'block', {identifier, _, Name}, Contents}) ->
                block_ast(Name, Contents, Context);
            ({'comment', _Contents}) ->
                empty_ast();
            ({'date', 'now', {string_literal, _Pos, FormatString}}) ->
                now_ast(FormatString, Context);
            ({'autoescape', {identifier, _, OnOrOff}, Contents}) ->
                body_ast(Contents, Context#dtl_context{auto_escape = list_to_atom(OnOrOff)});
            ({'text', _Pos, String}) -> 
                string_ast(String, Context);
            ({'string_literal', _Pos, String}) ->
                auto_escape(erl_syntax:string(unescape_string_literal(String)), Context);
            ({'number_literal', _Pos, Number}) ->
                string_ast(Number, Context);
            ({'attribute', _} = Variable) ->
                {Ast, _VarName} = resolve_variable_ast(Variable, Context),
                format(Ast, Context);
            ({'variable', _} = Variable) ->
                {Ast, _VarName} = resolve_variable_ast(Variable, Context),
                format(Ast, Context);              
            ({'include', Path}) ->
                PathAst = resolve_variable_or_value_ast(Path, Context),
                include_ast(PathAst, Context);
            ({'if', {'not', Variable}, Contents}) ->
                IfAstInfo = empty_ast(),
                ElseAstInfo = body_ast(Contents, Context),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context);
            ({'if', Variable, Contents}) ->
                IfAstInfo = body_ast(Contents, Context),
                ElseAstInfo = empty_ast(),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context);
            ({'ifelse', {'not', Variable}, IfContents, ElseContents}) ->
                IfAstInfo = body_ast(ElseContents, Context),
                ElseAstInfo = body_ast(IfContents, Context),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context);                  
            ({'ifelse', Variable, IfContents, ElseContents}) ->
                IfAstInfo = body_ast(IfContents, Context),
                ElseAstInfo = body_ast(ElseContents, Context),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context);
            ({'ifequal', Args, Contents}) ->
                IfAstInfo = body_ast(Contents, Context),
                ElseAstInfo = empty_ast(),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context);
            ({'ifequalelse', Args, IfContents, ElseContents}) ->
                IfAstInfo = body_ast(IfContents, Context), 
                ElseAstInfo = body_ast(ElseContents, Context),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context);                
            ({'ifnotequal', Args, Contents}) ->
                IfAstInfo = empty_ast(),
                ElseAstInfo = body_ast(Contents, Context),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context);
            ({'ifnotequalelse', Args, IfContents, ElseContents}) ->
                IfAstInfo = body_ast(ElseContents, Context),
                ElseAstInfo = body_ast(IfContents, Context),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context);
            ({'apply_filter', Variable, Filter}) ->
                filter_ast(Variable, Filter, Context);
            ({'for', {'in', IteratorList, Variable}, Contents}) ->
                for_loop_ast(IteratorList, Variable, Contents, Context);
            ({'cycle', Names}) ->
                cycle_ast(Names, Context);
            ({'cycle_compat', Names}) ->
                cycle_compat_ast(Names, Context)
        end, DjangoParseTree),   
    erl_syntax:list(AstInfoList).

block_ast(Name, Contents, Context) ->
    NameAst = erl_syntax:string(Name),
    ContextAst = erl_syntax:variable("Variables"),
    RenderModuleAst = erl_syntax:atom(Context#dtl_context.renderer_module),

    BlockCheckAst = erl_syntax:application(RenderModuleAst,
                                           erl_syntax:atom(have_child_block), 
                                           [NameAst, ContextAst]),
    TrueAst = erl_syntax:clause([erl_syntax:atom(true)], none, 
                                [erl_syntax:application(
                                   RenderModuleAst,
                                   erl_syntax:atom(render_block), 
                                   [NameAst, ContextAst])]),

    NewContextAst = erl_syntax:variable("NewContext"),
    FalseCaseAst = [erl_syntax:match_expr(NewContextAst,
                    erl_syntax:application(RenderModuleAst,
                                           erl_syntax:atom(set_block),
                                           [NameAst, 
                                            body_ast(Contents, Context), ContextAst])),
                    erl_syntax:application(
                      RenderModuleAst,
                      erl_syntax:atom(render_block), 
                      [NameAst, NewContextAst])
                   ],
    FalseAst = erl_syntax:clause([erl_syntax:atom(false)], none, FalseCaseAst),
    erl_syntax:case_expr(BlockCheckAst, [TrueAst, FalseAst]).

resolve_variable_or_value_ast({string_literal, _, Literal}, _Context) ->
    erl_syntax:string(unescape_string_literal(Literal));
resolve_variable_or_value_ast({number_literal, _, Literal}, _Context) ->
    erl_syntax:integer(list_to_integer(Literal));
resolve_variable_or_value_ast(Variable, Context) ->
    {Ast, _VarName} = resolve_variable_ast(Variable, Context),
    Ast.


empty_ast() ->
    erl_syntax:list([]).


string_ast(String, Context) ->
    case Context#dtl_context.debug of
        true ->
            erl_syntax:string(String); %% less verbose AST, better for development and debugging
        _ ->
            erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String])
    end.

filter_ast(Variable, Filter, Context) ->
    % the escape filter is special; it is always applied last, so we have to go digging for it

    % AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
    % so don't do any more escaping
    UnescapedAst = filter_ast_noescape(Variable, Filter, 
        Context#dtl_context{auto_escape = did}),
    case search_for_escape_filter(Variable, Filter, Context) of
        on ->
            erl_syntax:application(
                    erl_syntax:atom(erlydtl_filters), 
                    erl_syntax:atom(force_escape), 
                    [UnescapedAst]);
        _ ->
            UnescapedAst
    end.

filter_ast_noescape(Variable, [{identifier, _, "escape"}], Context) ->
    body_ast([Variable], Context);
filter_ast_noescape(Variable, Filter, Context) ->
    VariableAst = body_ast([Variable], Context),
    VarValue = filter_ast1(Filter, VariableAst),
    VarValue.

filter_ast1([{identifier, _, Name} | Arg], VariableAst) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(Name), 
        [VariableAst | case Arg of 
                [{string_literal, _, ArgName}] ->
                    [erl_syntax:string(unescape_string_literal(ArgName))];
                [{number_literal, _, ArgName}] ->
                    [erl_syntax:integer(list_to_integer(ArgName))];
                _ ->
                    []
            end]).
 
search_for_escape_filter(_, _, #dtl_context{auto_escape = on}) ->
    on;
search_for_escape_filter(_, _, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter(Variable, Filter, _) ->
    search_for_escape_filter(Variable, Filter).

search_for_escape_filter(_, [{identifier, _, "escape"}]) ->
    on;
search_for_escape_filter({apply_filter, Variable, Filter}, _) ->
    search_for_escape_filter(Variable, Filter);
search_for_escape_filter(_Variable, _Filter) ->
    off.



resolve_variable_ast(VarTuple, Context) ->
    resolve_variable_ast(VarTuple, Context, 'fetch_value').
 
resolve_ifvariable_ast(VarTuple, Context) ->
    resolve_variable_ast(VarTuple, Context, 'find_value').
           
resolve_variable_ast({attribute, {{identifier, _, AttrName}, Variable}}, Context, FinderFunction) ->
    {VarAst, VarName} = resolve_variable_ast(Variable, Context, FinderFunction),
    {erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(FinderFunction),
                    [erl_syntax:atom(AttrName), VarAst]), VarName};

resolve_variable_ast({variable, {identifier, _, VarName}}, Context, FinderFunction) ->
    VarValue = case resolve_scoped_variable_ast(VarName, Context) of
        undefined ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(FinderFunction),
                [erl_syntax:atom(VarName), erl_syntax:variable("Variables")]);
        Val ->
            Val
    end,
    {VarValue, VarName};

resolve_variable_ast({apply_filter, Variable, Filter}, Context, FinderFunction) ->
    {VarAst, VarName} = resolve_variable_ast(Variable, Context, FinderFunction),
    VarValue = filter_ast1(Filter, erl_syntax:list([VarAst])),
    {VarValue, VarName};

resolve_variable_ast(What, _Context, _FinderFunction) ->
   error_logger:error_msg("~p:resolve_variable_ast unhandled: ~p~n", [?MODULE, What]).

resolve_scoped_variable_ast(VarName, Context) ->
    lists:foldl(fun(Scope, Value) ->
                case Value of
                    undefined -> proplists:get_value(list_to_atom(VarName), Scope);
                    _ -> Value
                end
        end, undefined, Context#dtl_context.local_scopes).

format(Ast, Context) ->
    auto_escape(format_number_ast(Ast), Context).


format_number_ast(Ast) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(format_number),
        [Ast]).


auto_escape(Value, Context) ->
    case Context#dtl_context.auto_escape of
        on ->
            erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(force_escape),
                [Value]);
        _ ->
            Value
    end.


ifelse_ast(Variable, IfContentsAst, ElseContentsAst, Context) ->
    {Ast, _VarName} = resolve_ifvariable_ast(Variable, Context),
    erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(is_false), [Ast]),
        [erl_syntax:clause([erl_syntax:atom(true)], none, 
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none,
                [IfContentsAst])
        ]).

        
ifequalelse_ast(Args, IfContentsAst, ElseContentsAst, Context) ->
    [Arg1Ast, Arg2Ast] = lists:foldl(fun
            (X, Asts) ->
                case X of
                    {string_literal, _, Literal} ->
                        [erl_syntax:string(unescape_string_literal(Literal)) | Asts];
                    {number_literal, _, Literal} ->
                        [erl_syntax:integer(list_to_integer(Literal)) | Asts];
                    Variable ->
                        {Ast, _VarName} = resolve_ifvariable_ast(Variable, Context),
                        [Ast | Asts]
                end                
        end,
        [],
        Args),
    erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(are_equal),
            [Arg1Ast, Arg2Ast]),
        [
            erl_syntax:clause([erl_syntax:atom(true)], none, [IfContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none, [ElseContentsAst])
        ]).


for_loop_ast(IteratorList, Variable, Contents, Context) ->
    Vars = lists:map(fun({identifier, _, Iterator}) -> 
                    erl_syntax:variable("Var_" ++ Iterator) 
            end, IteratorList),
    InnerAst = body_ast(Contents,
        Context#dtl_context{local_scopes = [
                [{'forloop', erl_syntax:variable("Counters")} | lists:map(
                    fun({identifier, _, Iterator}) ->
                            {list_to_atom(Iterator), erl_syntax:variable("Var_" ++ Iterator)} 
                    end, IteratorList)] | Context#dtl_context.local_scopes]}),
    CounterAst = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
        erl_syntax:atom(increment_counter_stats), [erl_syntax:variable("Counters")]),
    {ListAst, _VarName} = resolve_variable_ast(Variable, Context),
    CounterVars0 = case resolve_scoped_variable_ast("forloop", Context) of
        undefined ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [ListAst]);
        Value ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [ListAst, Value])
    end,
    erl_syntax:application(
            erl_syntax:atom('erlang'), erl_syntax:atom('element'),
            [erl_syntax:integer(1), erl_syntax:application(
                    erl_syntax:atom('lists'), erl_syntax:atom('mapfoldl'),
                    [erl_syntax:fun_expr([
                                erl_syntax:clause([erl_syntax:tuple(Vars), erl_syntax:variable("Counters")], none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])]),
                                erl_syntax:clause(case Vars of [H] -> [H, erl_syntax:variable("Counters")];
                                        _ -> [erl_syntax:list(Vars), erl_syntax:variable("Counters")] end, none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])])
                            ]),
                        CounterVars0, ListAst])]).

cycle_ast(Names, Context) ->
    NamesTuple = lists:map(fun({string_literal, _, Str}) ->
                                   erl_syntax:string(unescape_string_literal(Str));
                              ({variable, _}=Var) ->
                                   {V, _} = resolve_variable_ast(Var, Context),
                                   V;
                              ({number_literal, _, Num}) ->
                                   format(erl_syntax:integer(Num), Context);
                              (_) ->
                                   []
                           end, Names),
    erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters")]).

%% Older Django templates treat cycle with comma-delimited elements as strings
cycle_compat_ast(Names, _Context) ->
    NamesTuple = [erl_syntax:string(X) || {identifier, _, X} <- Names],
    erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters")]).

now_ast(FormatString, _Context) ->
    % Note: we can't use unescape_string_literal here
    % because we want to allow escaping in the format string.
    % We only want to remove the surrounding escapes,
    % i.e. \"foo\" becomes "foo"
    UnescapeOuter = string:strip(FormatString, both, 34),
    erl_syntax:application(
        erl_syntax:atom(erlydtl_dateformat),
        erl_syntax:atom(format),
        [erl_syntax:string(UnescapeOuter)]).

unescape_string_literal(String) ->
    unescape_string_literal(string:strip(String, both, 34), [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal("n" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\n | Acc], noslash);
unescape_string_literal("r" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\r | Acc], noslash);
unescape_string_literal("t" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\t | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).

include_ast(Path, Context) ->
    Extension = 
        lists:foldl(fun(Scope, AccScope) ->
                            lists:foldl(fun({Key, Val}, Acc) ->
                                            [erl_syntax:tuple([erl_syntax:atom(Key), Val])| Acc]
                                        end, AccScope, Scope)
                    end, [], Context#dtl_context.local_scopes), 
    NewContextAst = erl_syntax:application(erl_syntax:atom(erlydtl_runtime),
                                           erl_syntax:atom(extend_dict),
                                           [erl_syntax:variable("Variables"), 
                                            erl_syntax:list(Extension)]),
    include_ast(Path, NewContextAst, Context).

include_ast(Path, NewContextAst, Context) ->
    AppAst = erl_syntax:application(
               erl_syntax:atom(Context#dtl_context.renderer_module),
               erl_syntax:atom(render),
               [Path, NewContextAst, erl_syntax:abstract(Context#dtl_context.renderer_params)]),
    Ext = case erlang:now() of
              {MegaSec, Sec, MicroSec} -> 
                   "_" ++integer_to_list(MegaSec) ++
                   "_" ++integer_to_list(Sec) ++
                   "_" ++integer_to_list(MicroSec)
          end,
    RenderedAst = erl_syntax:variable("Rendered" ++ Ext),
    OkAst = erl_syntax:clause(
              [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
              none,
              [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason" ++ Ext),
    ErrStrAst = erl_syntax:application(
                  erl_syntax:atom(io_lib),
                  erl_syntax:atom(format),
                  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
                 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
                 none,
                 [ErrStrAst]),
    erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]).
