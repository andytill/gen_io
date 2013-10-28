

-record(gen_io_spec, {
	
    module              :: atom(), 

    spec_id             :: atom(),

    args = []           :: [],

    shutdown = 5000     :: integer(),

    registered_name     :: undefined | {local, atom()} | {global, atom()}
}).