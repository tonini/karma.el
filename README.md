[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/tonini/karma.el.png)](https://travis-ci.org/tonini/karma.el)

# karma.el

> Karma Test Runner Emacs Integration

## Installation

### ELPA

Karma.el is available on community maintained repository - [MELPA](http://melpa.milkbox.net/)

Just run `M-x package-install [RET] karma [RET]` inside your emacs and you're ready to go.

If you're not already using ELPA, check the [emacswiki](http://www.emacswiki.org/emacs/ELPA) page to get
familiar with it.

### Manual

```lisp
(add-to-list 'load-path "/path/to/karma.el/")
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
