    namespace eval google {
        namespace eval protobuf {
            # Simplified version: find first available
            set protoc [auto_execok protoc]
            set outdir . ;# no shadow build support so far

            proc compile-pre {target sources} {
                variable protoc
                variable outdir

                set ipath [::ag $target ?incdir]
                if { $ipath != "" } {
                    set ipath "-I$ipath"
                }

                set rules ""
                set extra_headers ""
                set out_sources ""  ;# will be rewritten
                foreach f $sources {
                    if { ![string match *.proto $f] } {
                        lappend out_sources $f ;# leave it in -sources
                        continue
                    }
                    set name [file rootname $f]

                    dict set rules $outdir/${name}.pb.h "$f {\n\t$protoc $ipath --cpp_out=$outdir $f\n}"
                    lappend extra_headers $outdir/${name}.pb.h

                    # let *.cc rule simply link to the *.h rule
                    dict set rules $outdir/${name}.pb.cc "$f {\n\r!link $outdir/${name}.pb.h\n}"
                    lappend out_sources $outdir/${name}.pb.cc
                }

                # Ok, now add the rules to the target's rules and
                # let the normal processing facility create rules for *.cc files.
                ::ag $target -rules $rules -sources {= $out_sources} -headers $extra_headers

                # Additionally set pkg-config package automatically
                ::ag $target -packages protobuf
            }
        }
    }

