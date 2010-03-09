#! /bin/bash

export PATH=/development/bin:$PATH
export JAVA_HOME=/usr/bin
export LD_LIBRARY_PATH=/development/lib

export TIMESTAMP=`date +'%G%m%dT%H%M%S'`
export ROOT=`pwd`

export ABCL=`which abcl`
export ALISP=`which alisp`
export CCL=`which ccl`
export CLISP=`which clisp`
export CMUCL=`which cmucl`
export ECL=`which ecl`
export LW=`which lispworks-personal-5-1-1-x86-linux`
export SBCL=/usr/local/bin/sbcl

## initialized as
## git clone http://common-lisp.net/project/asdf/asdf.git .
echo git pull
( cd asdf; git pull )

mkdir $TIMESTAMP
cd $TIMESTAMP
echo "cd $TIMESTAMP"

if [[ "NO" == "NO${ABCL}" ]]
then
  echo "NO ABCL"
else
  mkdir abcl
  echo $ABCL --load ${ROOT}/test-init.lisp --load "${ROOT}/asdf-pathname-test.lisp" --eval "(cl-user::quit)" 
  (cd abcl; $ABCL <<EOF
  (load "${ROOT}/test-init.lisp")
  (load "${ROOT}/asdf-pathname-test.lisp")
  (cl-user::quit)
EOF
 )
fi

if [[ "NO" == "NO${ALISP}" ]]
then
  echo "NO ALISP"
else
  mkdir alisp
  echo $ALISP -L ${ROOT}/test-init.lisp -L "${ROOT}/asdf-pathname-test.lisp" -kill
  (cd alisp; $ALISP -L ${ROOT}/test-init.lisp -L "${ROOT}/asdf-pathname-test.lisp" -kill )
fi

if [[ "NO" == "NO${CCL}" ]]
then
  echo "NO CCL"
else
  mkdir ccl
  echo $CCL --batch --no-init --load ${ROOT}/test-init.lisp --eval "(load \"${ROOT}/asdf-pathname-test.lisp\")" --eval "(quit)"
  (cd ccl; $CCL --batch --no-init --load ${ROOT}/test-init.lisp --eval "(load \"${ROOT}/asdf-pathname-test.lisp\")" --eval "(quit)")
fi

if [[ "NO" == "NO${CLISP}" ]]
then
  echo "NO CLISP"
else
  mkdir clisp
  echo $CLISP -x "(load \"${ROOT}/asdf/asdf.lisp\")" -x "(load \"${ROOT}/asdf-pathname-test.lisp\")"
  (cd clisp; $CLISP -x "(load \"${ROOT}/asdf/asdf.lisp\")" -x "(load \"${ROOT}/asdf-pathname-test.lisp\")")
fi

if [[ "NO" == "NO${CMUCL}" ]]
then
  echo "NO CMUCL"
else
  mkdir cmucl
  echo $CMUCL -batch -load "${ROOT}/test-init.lisp" -load "${ROOT}/asdf-pathname-test.lisp" -eval "(quit)"
  (cd cmucl; $CMUCL -batch -load "${ROOT}/test-init.lisp" -load "${ROOT}/asdf-pathname-test.lisp" -eval "(quit)" )
fi

if [[ "NO" == "NO${ECL}" ]]
then
  echo "NO ECL"
else
  mkdir ecl
  echo $ECL -load "${ROOT}/asdf/asdf.lisp" -load "${ROOT}/asdf-pathname-test.lisp" -eval "(quit)"
  (cd ecl; $ECL -load "${ROOT}/asdf/asdf.lisp" -load "${ROOT}/asdf-pathname-test.lisp" -eval "(quit)")
fi

if [[ "NO" == "NO${LW}" ]]
then
  echo "NO LW"
else
  mkdir lw
  echo $LW -init ${ROOT}/test-init.lisp --init "${ROOT}/asdf-pathname-test.lisp"
  (cd lw; $LW <<EOF
  (load "${ROOT}/test-init.lisp")
  (load "${ROOT}/asdf-pathname-test.lisp")
  (quit)
EOF
 )
fi

if [[ "NO" == "NO${SBCL}" ]]
then
  echo "NO SBCL"
else
  mkdir sbcl
  echo $SBCL --userinit ${ROOT}/test-init.lisp --eval "(load \"${ROOT}/asdf-pathname-test.lisp\")" --eval "(quit)"
  (cd sbcl; $SBCL --userinit ${ROOT}/test-init.lisp --eval "(load \"${ROOT}/asdf-pathname-test.lisp\")" --eval "(quit)")
fi

echo "DONE."
