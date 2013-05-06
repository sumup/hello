-module(hello_validate_SUITE).
-compile([export_all]).

-include("hello_ct_helpers.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

% ---------------------------------------------------------------------
% -- test cases
validate_type_fuzz(_Config) ->
    ?proper_qc(prop_validate_type(), [{numtests, 1000}]).

prop_validate_type() ->
    ?FORALL({Spec, Value}, gen_value_and_spec(),
        hello_validate:type(Spec, Value)).

% ---------------------------------------------------------------------
% -- proper generators
gen_value_and_spec() ->
    oneof([gen_integer_and_spec(),
           gen_float_and_spec(),
           gen_bstring_and_spec(),
           gen_list_and_spec(),
           gen_object_and_spec()]).

gen_integer_and_spec() ->
    {integer, integer()}.

gen_float_and_spec() ->
    {float, float()}.

gen_bstring_and_spec() ->
    {string, ?LET(String, string(), unicode:characters_to_binary(String))}.

gen_non_empty_bstring() ->
    ?LET(String, non_empty(string()), unicode:characters_to_binary(String)).

-define(DEFAULT_COMPLEXITY, 5).
-define(complexity, parameter(complexity, ?DEFAULT_COMPLEXITY)).
-define(dec_complexity(Body), with_parameter(complexity, ?complexity - 1, ?LAZY(Body))).

gen_list_and_spec() ->
    {oneof([list, array]), ?dec_complexity(gen_maybe_empty_list())}.

gen_maybe_empty_list() ->
    frequency([{?DEFAULT_COMPLEXITY,
                    exactly([])},
               {?complexity,
                    ?LET(SpecsAndValues, list(gen_value_and_spec()),
                            lists:map(fun ({_Spec, Value}) -> Value end, SpecsAndValues))}]).

gen_object_and_spec() ->
    ?dec_complexity(gen_maybe_empty_object()).

gen_maybe_empty_object() ->
    frequency([{?DEFAULT_COMPLEXITY, exactly({object, {[]}})},
               {?complexity,         gen_complex_object_and_spec()}]).

gen_complex_object_and_spec() ->
    ?LET(Elts, list(?LAZY(gen_object_field_and_spec())),
        begin
            {Names, SpecsAndValues} = lists:unzip(Elts),
            {Specs, Values} = lists:unzip(SpecsAndValues),
            _FieldSpecs = lists:zip(Names, Specs),
            Fields = lists:zip(Names, Values),
            {object, {Fields}}
        end).

gen_object_field_and_spec() ->
    {gen_non_empty_bstring(), gen_value_and_spec()}.

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [validate_type_fuzz].
