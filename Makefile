#!/usr/bin/make -f

package = $(shell grep ^Name: *.spec | awk '{print $$2}')
version = $(shell grep ^Version: *.spec | awk '{print $$2}')

all:

dist:
	git clone --depth=1 "file://$${PWD}/.git" ${package}-${version}
	rm -rf ${package}-${version}/.git
	tar -cvpzf ${package}-${version}.tar.gz ${package}-${version}
	rm -rf ${package}-${version}

# EOF
