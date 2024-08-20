# ravanan --- High-reproducibility CWL runner powered by Guix
# Copyright © 2024 Arun Isaac <arunisaac@systemreboot.net>
#
# This file is part of ravanan.
#
# ravanan is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ravanan is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ravanan.  If not, see <https://www.gnu.org/licenses/>.

project = ravanan

GUILD ?= guild
NODE ?= node
SED ?= sed

prefix ?= /usr/local
bindir ?= $(prefix)/bin
datarootdir ?= $(prefix)/share
libdir ?= $(prefix)/lib

guile_effective_version = 3.0

top_level_module_dir = $(project)
config_file = $(top_level_module_dir)/config.scm
sources = $(wildcard $(top_level_module_dir)/*.scm) \
          $(wildcard $(top_level_module_dir)/work/*.scm) \
          $(config_file)
objects = $(sources:.scm=.go)
scripts = $(wildcard bin/*)

scmdir = $(datarootdir)/guile/site/$(guile_effective_version)
godir = $(libdir)/guile/$(guile_effective_version)/site-ccache

.PHONY: all clean install

all: $(objects) $(config_file)

%.scm: %.scm.in
	$(SED) 's|@NODE@|$(NODE)|' $< > $@

%.go: %.scm $(config_file)
	GUILE_AUTO_COMPILE=0 $(GUILD) compile -L . -o $@ $<

install: $(sources) $(objects) $(scripts)
	install -D $(scripts) --target-directory $(bindir)
	for source in $(sources); do \
		install -D $$source $(scmdir)/$$source; \
	done
	for object in $(objects); do \
		install -D $$object $(godir)/$$object; \
	done

clean:
	rm -f $(objects) $(config_file)
