#gen_io

gen_io is an erlang behaviour that aims to separate side effects from the code we write, so that more code can be pure functions, and so more easily testable.  gen_io modules must implement all functions required by gen_server, plus optionally implement the process_io/2 function.

In erlang, side effects **can** be handled in the following manner:

```erlang
handle_cast({send, Data}, State) ->
    Result = handle_send(Data),
    process_io(Result),
    {noreply, State}.

handle_send(Data0) ->
    Data1 = ...                     % do some work on the data
    {forward, Data1}.               % return data that can be interpreted as a command

process_io({forward, Data}) ->
    other_process:forward(Data).
```

Even when code is written like this, only two of the six lines of code (not including function headers) is pure and can be easily tested using eunit.  gen_io allows a new return type `{ok, {io, [SideEffect :: tuple()]}, State}`, the gen_io behaviour will handle the list of side effects by calling `process_io` with the side effect tuple as an argument.  What's more, gen_io provides many common implementations for side effects, so you might not need to code it at all!

So lets see what this example might look like using gen_io.

```erlang
handle_cast({send, Data}, State) ->
    Result = handle_send(Data),
    {noreply, {io, [Result]} State}.

handle_send(Data0) ->
    Data1 = ...                                 % do some work on the data
    {func, other_process, forward, [Data1]}.    % return data that can be interpreted as a command
```

There is much less boiler plate here and both functions are now purely functions, so its very easy to test.  gen_io will execute the tuple tagged as `func` by executing it as a function.

## Current State

gen_io is an a very experimental state although the prototype is fully working.

## About

This project was inspired by the [Purely Functional I/O](http://www.infoq.com/presentations/io-functional-side-effects?utm_source=infoq&utm_medium=videos_homepage&utm_campaign=videos_row3) presentation, it is well worth watching to get a quick start on IO in functional programming by [Rúnar Óli](https://twitter.com/runarorama).