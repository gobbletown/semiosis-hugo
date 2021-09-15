+++
title = "Hydra - CI and release management for NixOS"
author = ["Shane Mulligan"]
date = 2020-01-09T00:00:00+13:00
keywords = ["nix", "ci", "release", "testing", "hydra"]
draft = false
+++

Original article
: <https://nixos.org/hydra/manual/>

<!--listend-->

{{< highlight text "linenos=table, linenostart=1" >}}
big bang integration
    The phenomenon where components are only
    tested together near the end of the
    development process.
{{< /highlight >}}


## Background {#background}


### Continuous Integration {#continuous-integration}

An automated system continuously or
periodically:

-   check out the source code of a project,
-   build it,
-   run tests, and
-   produce reports for the developers.


### CI Tools enable {#ci-tools-enable}


#### Lenthy tests {#lenthy-tests}

Many projects have very large test sets (e.g.,
regression tests in a compiler, or stress
tests in a DBMS) that can take hours or days
to run to completion.


#### Tests may include many types of analysis {#tests-may-include-many-types-of-analysis}

Many kinds of static and dynamic analyses can
be performed as part of the tests, such as
code coverage runs and static analyses.


#### Build many variants {#build-many-variants}

It may also be necessary to build many
different variants of the software.

For instance, it may be necessary to verify
that the component builds with various
versions of a compiler.


#### Deterministic incremental building {#deterministic-incremental-building}

Developers typically use incremental building
to test their changes (since a full build may
take too long), but this is unreliable with
many build management tools (such as Make),
i.e., the result of the incremental build
might differ from a full build.


#### Assurance for building from VCS {#assurance-for-building-from-vcs}

It ensures that the software can be built from
the sources under revision control.

Users of version management systems such as
CVS and Subversion often forget to place
source files under revision control.


#### Clean builds {#clean-builds}

The machines on which the CI system runs
ideally provides a clean, well-defined build
environment.

If this environment is administered through
proper SCM techniques, then builds produced by
the system can be reproduced.

In contrast, developer work environments are
typically not under any kind of SCM control.


#### Test combinations as soon as possible {#test-combinations-as-soon-as-possible}

In large projects, developers often work on a
particular component of the project, and do
not build and test the composition of those
components (again since this is likely to take
too long).

To prevent "big bang integration", it is
important to test components together as soon
as possible (hence CI).


#### Automatic software release {#automatic-software-release}

It allows software to be released by
automatically creating packages that users can
download and install.

To do this manually represents an often
prohibitive amount of work, as one may want to
produce releases for many different platforms:
e.g., installers for Windows and Mac OS X, RPM
or Debian packages for certain Linux
distributions, and so on.


## Limitations of traditional CI tools {#limitations-of-traditional-ci-tools}

Examples of CI tools include:

-   Jenkins,
-   CruiseControl Tinderbox,
-   Sisyphus,
-   Anthill and
-   BuildBot.

These tools have the following limitations.:


### They do not manage the build environment {#they-do-not-manage-the-build-environment}

The build environment consists of the
dependencies necessary to perform a build
action, e.g., compilers, libraries, etc.

Setting up the environment is typically done
manually, and without proper SCM control (so
it may be hard to reproduce a build at a later
time).

Manual management of the environment scales
poorly in the number of configurations that
must be supported.

For instance, suppose that we want to build a
component that requires a certain compiler X.

We then have to go to each machine and install
X.

If we later need a newer version of X, the
process must be repeated all over again.

An ever worse problem occurs if there are
conflicting, mutually exclusive versions of
the dependencies.

Thus, simply installing the latest version is
not an option.

Of course, we can install these components in
different directories and manually pass the
appropriate paths to the build processes of
the various components.

But this is a rather tiresome and error-prone
process.


### They do not easily support variability in software systems {#they-do-not-easily-support-variability-in-software-systems}

A system may have a great deal of build-time
variability: optional functionality, whether
to build a debug or production version,
different versions of dependencies, and so on.

(For instance, the Linux kernel now has over
2,600 build-time configuration switches.)

It is therefore important that a CI tool can
easily select and test different instances
from the configuration space of the system to
reveal problems, such as erroneous
interactions between features.

In a CI setting, it is also useful to test
different combinations of versions of
subsystems, e.g., the head revision of a
component against stable releases of its
dependencies, and vice versa, as this can
reveal various integration problems.


## Hydra {#hydra}

A CI tool that solves the above problems.

It is built on the `Nix` <span class="underline">**package manager**</span>.

-   The build environment for projects is produced automatically and
    deterministically, and
-   variability in components to be expressed naturally using functions;
    and as such is an ideal fit for a continuous build system.


## Glossary {#glossary}

{{< highlight text "linenos=table, linenostart=1" >}}
Portability testing
    Software may need to be built and tested
    on many different platforms.

    It is infeasible for each developer to do
    this before every commit.

Hydra
    [CI testing and software release tool]

    [#Nix]
    [#Nix language]
    [#CI]

    A Nix-based continuous build system,
    released under the terms of the GNU GPLv3
    or (at your option) any later version.

    It continuously checks out sources of
    software projects from version management
    systems to build, test and release them.

    Uses a purely functional language to
    describe build jobs and their
    dependencies.

    The build tasks are described using Nix
    expressions.

    This allows a Hydra build task to specify
    all the dependencies needed to build or
    test a project.

    It supports a number of operating systems,
    such as various GNU/Linux flavours, Mac OS
    X, and Windows.

    Example:
        At Delft University of Technology we
        run a Hydra-based build farm to build
        and test the NixOS GNU/Linux
        distribution, the Nix Packages
        collection, the Stratego/XT program
        transformation system, and various
        other open source projects.
{{< /highlight >}}