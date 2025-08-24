# ravanan --- High-reproducibility CWL runner powered by Guix
# Copyright © 2025 Arun Isaac <arunisaac@systemreboot.net>
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

CCWL ?= ccwl
CWLTEST ?= cwltest

tools_directory = tools
tool_ccwl = $(wildcard $(tools_directory)/*.scm)
tool_cwl = $(tool_ccwl:.scm=.cwl)

all: $(tool_cwl)

%.cwl: %.scm
	$(CCWL) compile $^ > $@

.PHONY: check clean
check:
	mkdir -p /tmp/cwltest-tmpdir
	TMPDIR=/tmp/cwltest-tmpdir ../pre-inst-env $(CWLTEST) --test tests.yaml --tool ravanan -- --store=store --guix-manifest=manifest.scm

clean:
	rm -f $(tool_cwl)
