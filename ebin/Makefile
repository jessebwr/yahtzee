all: compile

compile:
	erlc -o . ../src/*.erl
	clang++ -undefined dynamic_lookup -dynamiclib -std=c++11 -stdlib=libc++ -Wall -fPIC -O3 -I /opt/local/lib/erlang/erts-5.10.4/include -shared ../c_src/yahtzee_chooser.cpp -o ./yahtzee_chooser.so
clean:
	rm *.beam
	rm *.so
