[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/tonini/karma.el.png)](https://travis-ci.org/tonini/karma.el)
[![MELPA](http://melpa.org/packages/karma-badge.svg)](http://melpa.org/#/karma)
[![MELPA Stable](http://stable.melpa.org/packages/karma-badge.svg)](http://stable.melpa.org/#/karma)

# karma.el

> Karma Test Runner Emacs Integration

***

- [Installation](#installation)
  - [ELPA](#installation-via-packageel)
  - [Via el-get](#via-el-get)
  - [Manual](#manual)
- [Usage](#usage)
  - [Basic setup](#basic-setup)
  - [Interactice commands](#interactive-commands)
- [Contributing](#contributing)
- [License](#license)

## Installation

### Installation via package.el

`package.el` is the built-in package manager in Emacs.

Karma.el is available on the three major community maintained repositories -
[MELPA STABLE](melpa-stable.milkbox.net), [MELPA](http://melpa.milkbox.net) and [Marmalade](https://marmalade-repo.org/).

You can install `Karma` with the following commnad:

<kbd>M-x package-install [RET] karma [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'karma)
  (package-install 'karma))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is the recommended way of
obtaining Karma, as the `master` branch is normally quite stable and
"stable" (tagged) builds are released somewhat infrequently.

With the most recent builds of Emacs, you can pin Karma to always
use MELPA Stable by adding this to your Emacs initialization:

```el
(add-to-list 'package-pinned-packages '(karma . "melpa-stable") t)
```

### Via el-get

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs. If you're an el-get
user just do <kbd>M-x el-get-install [RET] karma [RET]</kbd>.

### Manual

You can install Karma manually by placing it on your `load-path` and
`require` ing it. Many people favour the folder `~/.emacs.d/vendor`.

```el
(add-to-list 'load-path "~/.emacs.d/vendor/karma.el/")
(require 'karma)
```
## Usage

### Basic setup

You need to create an `.karma` file inside your project directory to inform
Karma.el where to get the Karma config file and the Karma executable.

```json
{
  "config-file": "karma.coffee",
  "karma-command": "node_modules/karma/bin/karma"
}
```

The `config-file` and the `karma-command` paths need to be relative or absoulte
to the your project directory.

### Interactive Commands

Keybinding           | Description
---------------------|---------------
<kbd>C-c , t</kbd>   | Runs `karma-start` in the root directory of the project.
<kbd>C-c , s s</kbd> | Runs `karma-start-single-run` in the root directory of the project.
<kbd>C-c , n s</kbd> | Runs `karma-start-no-single-run` in the root directory of the project.
<kbd>C-c , r</kbd>   | Runs `karma-run` in the root directory of the project.
<kbd>C-c , p</kbd>   | Pop to the `*Karma start*` buffer if exists.
<kbd>C-c , c</kbd>   | Runs `karma-test-file-current-buffer`.

## Contributing

Contributions are very welcome!

1. Fork karma.el
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!

## License

Copyright © 2014 Samuel Tonini and
[contributors](https://github.com/tonini/karma.el/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
