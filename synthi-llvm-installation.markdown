---
title: Installations for the synthesizer-llvm tutorial
---

The following instructions might appear to be lengthy and cumbersome,
but they boil essentially down to:
Install LLVM and SoX, create and install a custom `llvm.pc` file
and then install `synthesizer-llvm` via the common Cabal workflow.


## Installing LLVM

The tutorial relies on the [LLVM](http://llvm.org/) compiler framework.
The `synthesizer-llvm` package uses it
like a machine-independent assembly language.
Currently we support LLVM-3.8 optimally.
This was the current version until 2016-09-02.
In principle we support LLVM versions back to 3.4,
but there were some changes in the types of so called intrinsics
that are used to address processor specific machine instructions.
If your LLVM version is too old,
some `synthesizer-llvm` examples will fail with errors like this one:

~~~~
Intrinsic has incorrect argument type!
<4 x float> (<4 x float>, <4 x float>, i8)* @llvm.x86.sse41.dpps
LLVM ERROR: Broken function found, compilation aborted!
~~~~


If you cannot manage to install LLVM-3.8,
you may ask me and we will find fixes to support older versions of LLVM.
However, LLVM-3.6 had a serious problem with callback functions
that I could not work around.

On Debian and Ubuntu you can install appropriate LLVM-3.8 packages
from the LLVM [package repositories](http://apt.llvm.org/).

On Windows 64 bit you may download my pre-built binaries
either as big
[ZIP archive](http://code.haskell.org/~thielema/llvm-ffi/archive/llvm-3.8-windows-7-64bit.zip)
or as small
[Tar XZ archive](http://code.haskell.org/~thielema/llvm-ffi/archive/llvm-3.8-windows-7-64bit.tar.xz).
They are built with MSYS2 and must be unpacked under `/usr/local`
such that the complete installation path is `/usr/local/llvm-3.8`.
I assume that an MSYS2 installation is not even necessary,
but I have not tested it.

I do not have an idea of the installation procedure on Mac.
However, LLVM is developed by Apple
thus installation should not be overly complicated.

You may also build LLVM yourself from sources.
I suggest that you configure the build with

~~~~
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=/usr/local/llvm-3.8 -DLLVM_BUILD_LLVM_DYLIB=ON -DLLVM_LINK_LLVM_DYLIB=ON -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=ON /your/path/to/llvm-3.8.1.src/
~~~~

The default configuration is a Debug build
that gnerates more than 6 GB of data.
By switching to Release build with asserts and dynamic linking as above,
it will only build about 600 MB of data.
I think that for our needs you even only need
the content of the include directory
and the `lib/libLLVM-3.8.so` on Linux
or the `bin/LLVM.dll` on Windows.


## Installing `llvm-ffi`

The first step in the setup of Haskell packages should be

~~~~
cabal update
~~~~

in order to fetch the list of all recently updated packages.

The good news is:
My packages work with any version of GHC from 7.4 to 8.0.
The bad news is:
The low-level bindings to LLVM, namely the package `llvm-ffi`,
require extra steps for installation.
The ancestor package `llvm-base`
tried to do the configuration automatically using `autoconf` and
some `Setup.hs` stuff that depended on a particular Cabal version.
I replaced this by `pkg-config` configuration
because Cabal supports that optimally.
However, in order to work nicely it would require that LLVM comes
with a PkgConfig file, which it does not.

It is pretty simple to create such a config file, though.
You may install and run my program
[llvm-pkg-config](https://hackage.haskell.org/package/llvm-pkg-config).
It should create a configuration like this one:

~~~~
Name: LLVM
Description: Low-level Virtual Machine compiler framework
Version: 3.8
URL: http://www.llvm.org/
Requires:
Conflicts:
Libs: -L/usr/lib/llvm-3.8/lib -lLLVM-3.8
Cflags: -I/usr/lib/llvm-3.8/include -std=c++11
~~~~

You may just adapt the above content to your needs
and write it to `/usr/local/lib/pkgconfig/llvm-3.8.pc`
or to any other location you find appropriate.
Then adapt the environment variable `PKG_CONFIG_PATH` appropriately.

You may now install `llvm-ffi`:

~~~~
cabal install -fbuildExamples llvm-ffi
~~~~

If you run `llvm-ffi-example` it should emit:

~~~~
[-2.0,-1.0,0.0,1.0,2.0,3.0,4.0,5.0]
~~~~

At least, it should not crash.


## Installation of `synthesizer-llvm`

Now installation of the main package should no longer be difficult.

~~~~
cabal install -fbuildExamples synthesizer-llvm
~~~~

This should build all necessary packages and some example executables.
If you have [SoX](http://sox.sourceforge.net/) installed,
you may play a MIDI file using

~~~~
synthi-llvm-render your-music.mid
~~~~

where `your-music.mid` is a MIDI file of your choice.
The instruments won't match the ones you expect,
but there should be some musical noise.
(And due to a SoX bug, playback will silence after about 2 minutes.)

You may also try

~~~~
synthi-llvm-example
~~~~

This should generate a big file called `speedtest.f32`
containing a filter effect.
You can play it via SoX' `play` command:

~~~~
play -r 44100 speedtest.f32
~~~~

We will use SoX throughout the tutorial, so please install that, too.

If you are on Linux
you should also compile support for Linux' audio system ALSA.

~~~~
cabal install -fbuildExamples synthesizer-alsa synthesizer-llvm
~~~~

This will support live processing of MIDI commands.

On any platform you may also install JACK

~~~~
cabal install -fbuildExamples -fjack jack synthesizer-llvm
~~~~

for support of live processing.


## Final words

If you have trouble with installation,
please contact me via <<haskell@henning-thielemann.de>>.
