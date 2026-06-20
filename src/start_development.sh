#!/bin/bash
#Use frequently issued commands here
ARCH=$(uname -m)
export BUILD_HOME=/home/skoranne/GITHUB/VLSI_LAYOUT/src
if [ "$ARCH" = "x86_64" ]; then
    export buildMakeFile="Makefile.ifx"
else
    export buildMakeFile="Makefile.nvf"
fi
echo "Setting build file to $buildMakeFile"

bc() {
    cd $BUILD_HOME
    make -f $buildMakeFile clean  
}
bx() {
    cd $BUILD_HOME
    make -f $buildMakeFile DEBUG=0 all
}
bd() {
    cd $BUILD_HOME
    make -f $buildMakeFile DEBUG=1 all
}
fm() {
    git status | grep -i modified
}
