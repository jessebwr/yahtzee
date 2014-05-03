all: compile

compile:
	erlc -o ../ebin/ ../src/*.erl 
	clang++ -undefined dynamic_lookup -dynamiclib -std=c++11 -stdlib=libc++ -Wall -fPIC -O3 -I /opt/local/lib/erlang/erts-5.10.4/include -shared yahtzee_chooser.cpp -o ../ebin/yahtzee_chooser.so
clean:
	rm ../ebin/*.beam
	rm ../ebin/*.so
