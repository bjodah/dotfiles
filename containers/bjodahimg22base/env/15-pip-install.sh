#!/bin/bash -xe
# usage: pass --user flag if running on workstation, and not in docker build.
${PYTHON:-python3} -m pip install $@ --upgrade pip
${PYTHON:-python3} -m pip install $@ --upgrade-strategy=eager --upgrade setuptools wheel
${PYTHON:-python3} -m pip install $@ \
        appdirs argcomplete argh \
        future git-archive-all mako pygments pyparsing \
	pytest pyyaml tqdm wheel wurlitzer

#${PYTHON:-python3} -m pip install --no-use-pep517 sqlalchemy
