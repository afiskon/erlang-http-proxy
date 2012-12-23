deps:
	./rebar get-deps
build: deps
	./rebar compile
run: build
	./start.sh
test: build
	./rebar ct skip_deps=true
test_full: build
	./rebar eunit
	./rebar ct
clean:
	./rebar clean
