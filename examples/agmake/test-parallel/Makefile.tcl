
phony all total

rule x {
	sleep 0.25
	echo "X" >x
}

rule a x {
	sleep 0.5
	echo "A" >a
	cat x >>a
}

rule b x {
	sleep 0.6
	echo "B" >b
	cat x >>b
}

rule c x {
	sleep 0.7
	echo "C" >c
	cat x >>c
}

rule d x {
	sleep 0.8
	echo "D" >d
	cat x >>d
}

rule total a b c d {
	sleep 1
	cat a b c d >>total
}

rule clean {
	%autoclean all
}
