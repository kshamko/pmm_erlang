REBAR = `which rebar`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	@( erl -mnesia dir '"db/"' -pa ebin deps/*/ebin -s task3)

.PHONY: all deps compile clean run
