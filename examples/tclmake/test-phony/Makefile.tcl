
rule d b c {
	echo LOLD
	echo D >d
}

rule a {
	echo LOLA
	echo A >a
}

phony a

rule b a {
	echo LOLB
	echo B >b
}

rule c a {
	echo LOLC
	echo C >c
}

