### ravanan --- High-reproducibility CWL runner powered by Guix
### Copyright © 2025 Arun Isaac <arunisaac@systemreboot.net>
###
### This file is part of ravanan.
###
### ravanan is free software: you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.
###
### ravanan is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with ravanan.  If not, see <https://www.gnu.org/licenses/>.

import json
from pathlib import Path
from pybadges import badge
import sys

match sys.argv:
    case [_, cwltest_badgedir, cwl_icon_file, output_directory]:
        for summary_file in Path(cwltest_badgedir).glob("*.json"):
            with open(summary_file) as file:
                summary = json.load(file)
            with (Path(output_directory) / (summary_file.stem + ".svg")).open("w") as file:
                file.write(badge(left_text=summary["subject"],
                                 right_text=summary["status"],
                                 right_color=summary["color"],
                                 logo=cwl_icon_file,
                                 embed_logo=True))
    case [program, *_]:
        print(f"Usage: {program} CWLTEST_BADGEDIR CWL_ICON_FILE OUTPUT_DIRECTORY")
        sys.exit(1)
