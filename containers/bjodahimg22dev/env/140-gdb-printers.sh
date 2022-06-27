#!/bin/bash -e

PRINTERS_ROOT=${1:-/opt}
cd $PRINTERS_ROOT
git clone --branch indicate-dynamic https://github.com/bjodah/eigengdb
#( cd eigengdb; python3 setup.py install && python3 bin/eigengdb_register_printers )

git clone --branch relax-upper-version-limit-of-boost https://github.com/bjodah/Boost-Pretty-Printer
#echo -e "python\nimport sys; sys.path.insert(1, '/opt/Boost-Pretty-Printer'); __import__('boost').register_printers(boost_version=(1, 77, 0))\nend" >>/root/.gdbinit

git clone https://github.com/koutheir/libcxx-pretty-printers

