.PHONY: xic dune ir lex zip clean

xic:
	cd src && dune build xic.exe
	cp src/_build/default/xic.exe xic

clean:
	rm -rf xic *.lexed *.parsed *.typed *.ir *.mir *.s *.o *.out 
	cd runtime && make clean && cd ..
	cd src && dune clean 

zip:
	zip -r submission.zip Makefile lib/ runtime/ src/ tests/ benchmarks/ xic-build
