(stack build sdf && stack exec sdf) 2>&1 | tee out
#(stack build --library-profiling --executable-profiling --profile autobeat) 2>&1 | tee out
