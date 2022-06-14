#!/bin/bash -xe
# usage: pass --user flag if running on workstation, and not in docker build.
${PYTHON:-python3} -m pip install $@ --upgrade pip
${PYTHON:-python3} -m pip install $@ --upgrade-strategy=eager --upgrade setuptools wheel
${PYTHON:-python3} -m pip install $@ \
        appdirs argcomplete argh black bottle check-manifest CherryPy \
        conan future git-archive-all mako "pygments>=2.4.1" pylatex pyparsing \
	pytest pyyaml tqdm wheel wurlitzer sqlalchemy

#${PYTHON:-python3} -m pip install --no-use-pep517 sqlalchemy
