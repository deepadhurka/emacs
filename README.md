# Emacs Configuration

> **Please note**: I no longer use emacs regularly and am unable to support issues on this configuration. I will leave it up for people in case it is useful, but I have disabled issues on the repository and will not respond to emails asking for help -- I receive too many emails as it is, and unfortunately I cannot respond to every one.

**Tested on versions:**
* GNU Emacs 24.3.1 (x86_64-apple-darwin13.1.0, NS apple-appkit-1265.19) of 2014-04-06
* GNU Emacs 24.3.1 (x86_64-pc-linux-gnu, GTK+ Version 3.4.2) of 2014-02-22

## Installing

Clone this repository, and then run the `bootstrap.sh` script. This
will copy all of the necessary files to `~/.emacs` and
`~/.emacs.d`. Note that if these files exist for you already, **this
will overwrite those files**.

Once you have run the bootstrap script, start Emacs (make sure you are
connected to the internet when you do this for the first time). It
will install [`el-get`](https://github.com/dimitri/el-get) and all of
the other plugins listed in the next section. This initial
installation may take a while, so be patient.

## Dependencies

For some of the plugins to work, you will need to have some external
dependencies installed, such as Python, IPython, git, etc. If the
installation gives you an error, it might mean you are missing a
required dependency that el-get doesn't install.

## Emacs plugins

This configuration installs several plugins using
[`el-get`](https://github.com/dimitri/el-get). These plugins are
specified in `.emacs.d/settings/el-get-settings.el`, and are also
listed below:

* `auctex` -- LaTeX plugin
* `ein` -- [IPython notebook](http://ipython.org/notebook) plugin
* `jedi` -- general Python support
* `pydoc-info` -- Python documentation
* `auto-complete` -- auto completion
* `popup` -- visual popup (e.g., for auto completion)
* `color-theme-solarized` -- the [solarized](http://ethanschoonover.com/solarized) color theme
* `magit` -- git plugin
* `markdown-mode` -- support for [Markdown](http://daringfireball.net/projects/markdown/) files
* `matlab-mode` -- support Matlab files
* `nxhtml` (MuMaMo)
* `scss-mode` -- support for [SCSS](http://sass-lang.com/) files
* `nyan-mode` -- silly mode that renders a nyan cat to display how far
  you are through a file
* `helm` -- [completion and selection](https://github.com/emacs-helm/helm) narrowing framework
* `helm-descbinds` -- describe keybindings using helm
* `yaml-mode` -- support [YAML](https://github.com/yoshiki/yaml-mode) files

## Gotchas

Here are some issues I or others have run into when installing this configuration.

### Version control systems

To install all the plugins above, you need to have several different version control systems installed, including `hg`, `git`, `bzr`, and `cvs`.

### Trouble building AUCTeX

If you get the following error:

`error: el-get: ./autogen.sh el-get could not build auctex [./autogen.sh]`

There are a few possible causes. Try these steps:

1. Make sure you have `automake` and `texlive-full` installed (if you are on Ubuntu) or MacTeX (if you are on Mac).
2. Try running emacs from the command line (it could be an issue with not finding the right path).
3. If that doesn't work, run emacs from the command line with the `--debug-init` flag. This will give you more information about the error, and possibly point you towards the solution.

### No such file or directory: pycheckers

`pycheckers` is a little script to check that your Python code
conforms to the
[PEP8 style guide](http://legacy.python.org/dev/peps/pep-0008/) using
the [pep8](https://pypi.python.org/pypi/pep8) and
[pyflakes](https://pypi.python.org/pypi/pyflakes/0.8.1) Python
packages.

If you do not want this functionality, you can comment out the block
of code in `python-settings.el` that starts with "pyflakes flymake
integration". Otherwise, read on.

1. In your `~/.bashrc`, add `$HOME/bin` to your `$PATH` environment variable like so:
  
  ```
  export PATH="$HOME/bin:$PATH"
  ```

2. Create a file in `~/bin/pycheckers` with the following contents:

  ```
  #!/bin/bash
  
  pyflakes "$1"
  pep8 --ignore=E261 --repeat "$1"
  true
  ```

3. Make it executable with `chmod +x ~/bin/pycheckers`.

4. Make sure you have `pep8` and `pyflakes` installed (run `pip
   install pep8 pyflakes`).

5. Now it should work! If not, please submit a bug report and let me
   know.

### Tramp is timing out

If you get the error `tramp ssh: connect to host c port 22: Operation timed out` and you are running OS X Mavericks with Emacs installed using Homebrew, then this is probably due to the Mavericks upgrade. Try reinstalling Emacs through Homebrew and remove the folder `~/.emacs.d/el-get` (note: this will remove all your el-get plugins, and they will need to be reinstalled).

### Auto-complete disappears from python mode
The python mode needs to have the `jedi`, `epc`, and `pylint` packages added to your Python installation. Run this command:

  ```
  pip install jedi epc pylint
  ```

Hint provided by Andrea Crotti's EuroPython 2013 Conference talk, [Emacs and shell as your best friends](https://www.youtube.com/watch?v=0cZ7szFuz18), and the [minimal Emacs configuration](https://github.com/AndreaCrotti/minimal-emacs-configuration) used in the talk.


=================================
Full Dev Environment Setup

Requirements:
Ubuntu 14.04
Emacs 24.4 or higher. Check with emacs --version

git clone https://github.com/deepadhurka/emacs
git checkout -b dev --track dev
cd $HOME/emacs
./bootstrap.sh  #installs the .emacs, .emacs.d, .bashrc, .bash_aliases, requirements.txt files
#installs pychecker in $HOME/bin/
#source .bashrc

sudo apt-get install bash-completion
sudo apt-get install bzr
sudo apt-get install cvs
sudo apt-get install mercurial #hg
sudo apt-get install automake
sudo apt-get install texlive
sudo apt-get install texinfo
sudo apt-get install python-pip
sudo pip install virtualenv

cd $HOME/emacs
virtualenv venv
emenv #source venv
#install requirements in venv
pip install -r requirements.txt

NOW run bootstrap

TODO: 
Merge my C emacs settings into this git repo emacs el files and configs.
Merge the MyEnv changes to the python emacs dev branch in git.
Merge the golang changes too
Create requirements.txt and place in emacs git repo

