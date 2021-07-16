@symbol("abort") fn c_abort() void;
export @symbol("rt.abort") fn abort_() void = c_abort();
