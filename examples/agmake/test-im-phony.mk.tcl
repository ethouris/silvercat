

phony all answer


rule answer objects {
	g++ -o $@ file1.o file2.o
}

phony objects file1.o file2.o

rule *.o *.cc {
	g++ -c $<
}


# vim: ft=tclmake
