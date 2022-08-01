# Creating a Haskell Project with Cabal
**Note:** you need to have `git` installed before proceeding further. To check if git is installed, open a terminal window and run the command:
```
git --version
```
If a version displays, proceed to the next section. Otherwise follow the installation instructions for your operating system here:
https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

We won't cover the fundamentals of using Git and Github in this walkthrough. If you're new to version control, you can follow this tutorial from freeCodeCamp:

https://www.freecodecamp.org/news/git-and-github-for-beginners/

## I. Create Github Account & Create a New Repository
1. Go to https://github.com and create an account if you don't already have one. Otherwise log into your account.
2. Click the `+` icon in the top right corner and select `New Repository`.
3. Give your repository a name (the name of your project) and a short description (if desired).
4. Leave `Public` selected so your project can be reviewed by EMURGO faculty.
5. In the **Add .gitignore** section, select `Haskell`.
6. Optionally **Add a README file** as desired.
7. In the **Choose a license** section, leave `None` selected (we will create the license file when we initialize our Haskell project).
8. Click `Create repository` and you'll be taken to the repository page.

## II. Clone Repository
On the repository page, click the green `Code` button and copy the repository url under `HTTPS`.

In a terminal window, navigate to whichever directory you'd like your project directory to be placed, and enter the following command, pasting in the repository URL:

```
git clone https://github.com/<your username>/<your repository>.git
```

**Note:** VS Code frequently shows "module not found" errors in the `import` lines of project files when you have another folder (*i.e. a parent directory*) open in the Explorer instead of the project's root directory.

After cloning your project repository, use `File > Open Folder` and select the project root directory to open it in the Explorer. Do the same each time you open VS Code to work on your project.

Open a terminal window (`Ctrl` + `~` in VS Code) and verify that the path displayed in the terminal matches the project root directory before proceeding to the next section.

## III. Creating a Haskell Project with Cabal

*Reference: https://cabal.readthedocs.io/en/3.6/*

Cabal (Common Architecture for Building Applications and Libraries) is the standard package system for Haskell software.

If you installed Haskell using GHCUp, you should already have Cabal on your machine. To confirm, run the following command in a terminal window:

```
cabal -V
```

In your terminal window, make sure you are in the project root directory or else navigate there using the `cd` command.

Run the following command to download the latest package data from Hackage:

```
cabal update
```


Run the following command to start the interactive `cabal-install` tool:

```
cabal init -i
```

A series of interactive prompts will begin. Follow the responses listed after each prompt below:

```
Should I generate a simple project with sensible defaults? [default: y]
```

Type `n` and `Enter`.

```
What does the package build:
  1) Executable
  2) Library
  3) Library and Executable
```
Type `3` and `Enter`.

```
What is the main module of the executable:
 * 1) Main.hs (does not yet exist, but will be created)
   2) Main.lhs (does not yet exist, but will be created)
   3) Other (specify)
Your choice? [default: Main.hs ist, but will be created)]
```

Hit `Enter` to accept the default.

```
Please choose version of the Cabal specification to use:
   1) 1.10   (legacy)
   2) 2.0    (+ support for Backpack, internal sub-libs, '^>=' operator)
   3) 2.2    (+ support for 'common', 'elif', redundant commas, SPDX)
 * 4) 2.4    (+ support for '**' globbing)
   5) 3.0    (+ set notation for ==, common stanzas in ifs, more redundant commas, better pkgconfig-depends)
Your choice? [default: 2.4    (+ support for '**' globbing)]
```

Type `5` and `Enter`.

```
Package name? [default: ...]
```

Type `Enter` to accept the default.

```
Package version? [default: 0.1.0.0]
```

Type `Enter` to accept the default.

```
Please choose a license:
 * 1) NONE
   2) BSD-2-Clause
   3) BSD-3-Clause
   4) Apache-2.0
   5) MIT
   6) MPL-2.0
   7) ISC
   8) GPL-2.0-only
   9) GPL-3.0-only
  10) LGPL-2.1-only
  11) LGPL-3.0-only
  12) AGPL-3.0-only
  13) GPL-2.0-or-later
  14) GPL-3.0-or-later
  15) LGPL-2.1-or-later
  16) LGPL-3.0-or-later
  17) AGPL-3.0-or-later
  18) Other (specify)
Your choice? [default: NONE]
```

Type the number corresponding to the license of your choice. We will use `3` (`BSD-3-Clause`) in this walkthrough.

```
Author name? [default: ...]
```

The name associated with your Github account should display as the default. Type `Enter` to accept.

```
Maintainer email? [default: ...]
```

The email associated with your Github account should display as the default. Type `Enter` to accept.

```
Project homepage URL?
```

Hit `Enter` to leave blank, or add the URL to the project repository on Github.

```
Project synopsis?
```

Hit `Enter` to leave blank, or type a short, one-line description of your project.

```
Project category:
```

Hit `Enter` for `(none)`, or choose the number associated with the most relevant category.

```
Application (Main.hs) directory:
 * 1) app
   2) src-exe
   3) (none)
   4) Other (specify)
Your choice? [default: app]
```

Hit `Enter` to accept the default.

```
Library source directory:
 * 1) src
   2) lib
   3) src-lib
   4) (none)
   5) Other (specify)
Your choice? [default: src]
```

Hit `Enter` to accept the default.

```
Should I generate a test suite for the library? [default: y]
```

Type `n` and `Enter` (we will not be covering testing in this course).

```
What base language is the package written in:
 * 1) Haskell2010
   2) Haskell98
   3) Other (specify)
Your choice? [default: Haskell2010]
```

Hit `Enter` to accept the default.

```
Add informative comments to each field in the cabal file (y/n)? [default: n]
```

Type `y` and `Enter`.

## IV. Interactive Development with GHCi
To open a GHCi REPL, enter the following command in the terminal (make sure you are in the project root directory):

```
cabal repl
```

Only the `MyLib` module and its imports will be loaded.

To test the `Main` module (and its imports) with GHCi, use this command instead:

```
cabal repl exe:<project name>
```

**Note:** if you make changes to your library code, those changes will not be reflected in your `Main` GHCi session, even if you use the `:r` command. You need to quit the GHCi session (`:q`) and run the `cabal repl exe:` command again to start a fresh session.

## V. Adding Dependencies to .cabal File
1. Search for the library you wish to use in your project on https://hackage.haskell.org/
2. Add the package name to the `build-depends` line of the `library` and/or `executable` stanza of your project `.cabal` file.
3. Import the module name(s) at the top of your code file. If the associated package has been added correctly you should not see any error displayed.

**Note:** many modules that are part of Base Haskell and can be imported without error in a vanilla `.hs` file will give `Could not load module` errors in a Cabal project.

This is because when Cabal asks GHC to build your package, it tells it to ignore all available packages except those explicitly listed in the `.cabal` file. The terminology GHC uses for this is "hidden".

Hidden packages need to be added to the relevant `build-depends` line of the `.cabal` file. If your editor is configured with the relevant extensions, the error message should indicate the name of the library to add.
