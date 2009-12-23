
all:
	./rebar compile

edoc:
	erl -noshell -pa ebin \
	    -eval "edoc:application(mochiweb), \".\", [{dir, \"doc\"}])" \
	    -s init stop

test:
	erl -noshell -pa ebin -s mochiweb test -s init stop

clean:
	./rebar clean
