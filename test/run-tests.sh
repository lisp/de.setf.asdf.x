#!/bin/sh

# do_tests {lisp invocation} {scripts-regex}
# - read lisp forms one at a time from standard input
# - quit with exit status 0 on getting eof
# - quit with exit status >0 if an unhandled error occurs

export CL_SOURCE_REGISTRY="$PWD"

if [ x"$1" = "xhelp" ]; then
    echo "$0 [lisp invocation] [scripts-regex]"
    echo " - read lisp forms one at a time from matching scripts"
    echo " - quit with exit status 0 on getting eof"
    echo " - quit with exit status >0 if an unhandled error occurs"
    echo " you need to supply the .script in the second argument"
    echo " lisps include sbcl, clisp, allegro and allegromodern"
    exit -1
fi

if [ -z "$2" ]; then
    scripts="*.script"
else
    scripts="$2"
fi

sok=1

do_tests() {
rm -f *.$2 || true
( cd .. && echo '(load "test/compile-asdf.lisp")' | $1  )
if [ $? -eq 0 ] ; then
    test_count=0
    test_pass=0
    test_fail=0
    failed_list=""
    for i in $scripts ; 
    do 
      echo "Testing: $i" >&2
      test_count=`expr "$test_count" + 1`
      rm -f *.$2 || true
      if  $1 < $i ; then
        echo "Using $1, $i passed" >&2
	test_pass=`expr "$test_pass" + 1`
      else
        echo "Using $1, $i failed" >&2
	test_fail=`expr "$test_fail" + 1`
	failed_list="$failed_list $i"
        sok=0
      fi
    done
    echo >&2
    echo "-#---------------------------------------" >&2
    echo "Using $1" >&2
    echo "Ran $test_count tests: " >&2
    echo "  $test_pass passing and $test_fail failing" >&2
    if [ $test_fail -eq 0 ] ; then
	echo "all tests apparently successful" >&2
    else
	echo "failing test(s): $failed_list" >&2
    fi
    echo "-#---------------------------------------" >&2
    echo >&2
fi
}

# terminate on error
set -e

lisp=$1
if [ -z $1 ] ; then
    lisp="sbcl"
fi

case "$lisp" in
  sbcl)
    if type sbcl ; then
      fasl_ext="fasl"
      command="sbcl --userinit /dev/null --sysinit /dev/null --noinform --noprogrammer"
    fi ;;
  clisp)
    if type clisp ; then
	fasl_ext="fas"
	command=`which clisp`
	command="$command -norc -ansi -I - "
    fi ;;
  allegro)
    if type alisp ; then
	fasl_ext="fasl"
	command="alisp -q -batch "
    fi ;;
  allegromodern)
    if type mlisp ; then
	fasl_ext="fasl"
	command="mlisp -q -batch "
    fi ;;
  ccl)
    if type ccl ; then
        case `uname -s` in
          Linux) fasl_os=lx ;;
          Darwin) fasl_os=dx ;;
        esac
        case `uname -m` in
          x86_64|ppc64) fasl_bits=64 ;;
          i?86|ppc) fasl_bits=32 ;;
        esac
        fasl_ext="${fasl_os}${fasl_bits}fsl"
	command="ccl --no-init --quiet --batch "
    fi ;;
  cmucl)
    if type lisp ; then
	fasl_ext="x86f"
	command="lisp -batch -noinit"
    fi ;;
esac

create_asds () {
    mkdir -p {conf.d,dir1,dir2/{dir3,dir4}}
    for i in dir1 dir2; do touch "$i"/test.asd; done
    for i in dir3 dir4; do (cd dir2/$i; touch test.asd); done
}

clean_up () {
    rm -rf {conf.d,dir?}
}


if [ -z "$command" ] ; then
    echo "Error: cannot find or do not know how to run Lisp named $lisp"
else
    create_asds
    mkdir -p results
    echo $command
    thedate=`date "+%Y-%m-%d"`
    do_tests "$command" $fasl_ext 2>&1 | tee "results/${lisp}.text" "results/${lisp}-${thedate}.save"
    clean_up
fi
 
