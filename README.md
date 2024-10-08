[![Laminar](https://ci.systemreboot.net/badge/ravanan.svg)](https://ci.systemreboot.net/jobs/ravanan)

ravanan is a [Common Workflow Language (CWL)](https://www.commonwl.org/) implementation that is powered by [GNU Guix](https://guix.gnu.org/) and provides strong reproducibility guarantees. ravanan provides strong foolproof caching ([work reuse](https://www.commonwl.org/v1.2/CommandLineTool.html#WorkReuse)) so you never run the same steps of your workflow twice, nor do you have to keep track of which steps were run and with what parameters: ravanan remembers everything for you. ravanan captures logs from every step of your workflow so you can always trace back in case of job failures.

ravanan currently runs on single machines and on slurm via its [API](https://slurm.schedmd.com/rest_api.html). Other HPC backends will be implemented in the future based on demand.

# How to use

TODO

# License

ravanan is free software released under the terms of the [GNU General Public License](https://www.gnu.org/licenses/gpl.html), either version 3 of the License, or (at your option) any later version.

# The Name

ravanan is named after the [mythic learned ten-headed king of the island of Lanka](https://en.wikipedia.org/wiki/Ravana).
