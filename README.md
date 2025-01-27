[![Laminar](https://ci.systemreboot.net/badge/ravanan.svg)](https://ci.systemreboot.net/jobs/ravanan)

ravanan (pronounced rah-vun-un, IPA: rɑːvʌnʌn, Shavian: 𐑮𐑭𐑝𐑳𐑯𐑳𐑯) is a [Common Workflow Language (CWL)](https://www.commonwl.org/) implementation that is powered by [GNU Guix](https://guix.gnu.org/) and provides strong reproducibility guarantees. ravanan provides strong bullet-proof caching ([work reuse](https://www.commonwl.org/v1.2/CommandLineTool.html#WorkReuse)) so you never run the same step of your workflow twice, nor do you have to keep track of which steps were run and with what parameters: ravanan remembers everything for you. ravanan captures logs from every step of your workflow so you can always trace back in case of job failures.

Salient features of ravanan include:
- Bullet-proof caching; never run the same computation again and never fear that the cache is stale (no kidding, we're serious!)
- Each step in your CWL corresponds to exactly one job on the batch system.
- Clear logging; you never have to hunt for log files in obscure directories or binary databases.
- Jobs do not directly write to the shared network filesystem; keeps performance good and your fellow HPC users happy
- Jobs never write to `/tmp`; keeps your HPC admin happy

ravanan currently runs on single machines and on slurm via its [API](https://slurm.schedmd.com/rest_api.html). Other HPC backends will be implemented in the future based on demand.

# Building from source

ravanan depends on
- [GNU Guile](https://www.gnu.org/software/guile/)
- [GNU Guix](https://guix.gnu.org/)
- [guile-filesystem](https://gitlab.com/lilyp/guile-filesystem)
- [guile-gcrypt](https://notabug.org/cwebber/guile-gcrypt) (for SHA1 hash computation)
- [guile-json](https://github.com/aconchillo/guile-json)
- [guile-libyaml](https://github.com/mwette/guile-libyaml)
- [node](https://nodejs.org/) (for javascript execution)
The easiest way to get these dependencies is using Guix. You can do so by running
```
guix shell -Df guix.scm
```
Build using
```
make NODE=/usr/bin/node prefix=/usr/local
make install
```
`NODE` is the path to the node executable. `prefix` is the installation prefix to install ravanan to.

Optionally, run tests using
```
make check
```

# How to use
## Getting started

ravanan requires a running Guix daemon. ravanan maintains its own *store* (this is distinct from Guix's store) to cache job outputs.

You can run a workflow `hello-world.cwl` with inputs in `hello-world-inputs.json` using
```
ravanan hello-world.cwl hello-world-inputs.json --store=store --guix-manifest=manifest.scm
```
`--store` specifies a directory to use as ravanan's store. `--guix-manifest` specifies a Guix manifest file that lists the dependencies required for the workflow. An example `hello-world.cwl` and `hello-world-inputs.json` are provided in the `doc` directory of this repository.

## On HPC using slurm

ravanan can be used to run CWL workflows on a HPC cluster running the slurm batch scheduler. ravanan communicates with slurm using the [slurm API](https://slurm.schedmd.com/rest.html) and therefore requires a running slurmrestd. To authenticate to slurmrestd, you need a [JWT token](https://slurm.schedmd.com/jwt.html). Policy on your HPC cluster may vary, but you typically obtain a JWT token using scontrol like so:
```
scontrol token lifespan=86400 > jwt
```

Jobs run by ravanan do not directly write to the store. Instead, they operate on a separate *scratch* path. The store path must be on a shared filesystem that is visible to all compute nodes. The scratch path is usually on a fast filesystem that is local to each compute node. You must specify the scratch path using `--scratch`. Jobs writing directly to the shared store directory could overload the network and disrupt other cluster users; this design prevents that.

Putting it all together, a typical ravanan invocation on a slurm HPC might look like
```
ravanan hello-world.cwl hello-world-inputs.json --store=store --guix-manifest=manifest.scm --batch-system=slurm-api --scratch=/scratch --slurm-jwt=jwt
```

## Using a specific version of Guix

By default, ravanan builds packages using the version of Guix that it was compiled with. To explicitly specify the version of Guix to use, you can provide a channels file using `--guix-channels`.

You may also wish to specify a different manifest for each step in a workflow. This is especially convenient when developing the workflow, to avoid one little change to the manifest triggering a re-run of all steps. You can do this by listing packages (and optionally, their versions) using the CWL standard [SoftwareRequirement](https://www.commonwl.org/v1.2/CommandLineTool.html#SoftwareRequirement) specification. This even frees you from having to write the Guix manifest yourself!

But sometimes, this approach is not powerful enough; you may actually need the full expressive power of a Guix manifest. You can specify such a manifest file using a `manifest` field in the `SoftwareRequirement` requirement. This is a ravanan extension, and is not part of the CWL standard.
```yaml
SoftwareRequirement:
    manifest: my-manifest.scm
```

## Referencing local scripts

The CWL user guide recommends two different ways to [reference local scripts](https://www.commonwl.org/user_guide/faq.html#how-do-i-reference-a-local-script):
1. Making the script discoverable through the `PATH` environment variable
2. Passing the script into the workflow as a `File` type input

Method 1 is bad for reproducibility and ravanan does not allow it. Method 2 works but is still semantically awkward (a script isn't really a workflow input now, is it?).

Thanks to Guix, a better way is possible. Guix, in its magnificent elegance, does not distinguish between local user-defined packages and distribution-provided packages. So, you can simply package up your local script as a local Guix package and put that it into a manifest. Here's an example of such a manifest file. With this manifest file, you can simply invoke `my-script` from your CWL workflow as though it were any other command. And, all the reproducibility and bullet-proof caching properties of ravanan just work as usual!
```scheme
(use-modules ((gnu packages python) #:select (python))
             ((gnu packages python-xyz) #:select (python-numpy))
             (guix build-system trivial)
             (guix gexp)
             (guix packages))

(define my-script-gexp
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 match))

        ;; Set up any dependencies required by the script.
        (setenv "PYTHONPATH"
                #$(file-append (profile
                                (content (packages->manifest
                                          (list python python-numpy)))
                                (allow-collisions? #t))
                               "/lib/python3.10/site-packages"))
        (match (program-arguments)
          ((_ args ...)
           (apply invoke
                  #$(file-append python "/bin/python3")
                  #$(local-file "my-script")
                  args))))))

(define my-script
  (package
    (name "my-script")
    (version "0.1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:builder
           (with-imported-modules '((guix build utils))
             #~(begin
                 (use-modules (guix build utils))

                 (let ((bin (string-append #$output "/bin")))
                   (mkdir-p bin)
                   (symlink #$(program-file "my-script"
                                            my-script-gexp)
                            (string-append bin "/my-script")))))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(packages->manifest
 (list my-script))
```

# License

ravanan is free software released under the terms of the [GNU General Public License](https://www.gnu.org/licenses/gpl.html), either version 3 of the License, or (at your option) any later version.

# The Name

ravanan is named after the [mythic learned ten-headed king of the island of Lanka](https://en.wikipedia.org/wiki/Ravana).
