# erc-matterircd

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/erc-matterircd-badge.svg)](http://melpa.org/#/erc-matterircd)
![CI](https://github.com/alexmurray/erc-matterircd/workflows/CI/badge.svg)

Integrate [erc](https://www.gnu.org/software/erc/) with
[matterircd](https://github.com/42wim/matterircd) for a more seamless way
to use [Mattermost](https://mattermost.org/) via
[Emacs](https://www.gnu.org/software/emacs/).

## Features

- Connect automatically to Mattermost once the connection to `matterircd`
  has been established
- Prepend `@` to IRC nicks when completing nicks on `matterircd` servers so
  that notifications to other users work correctly
- Display `**bold text**` correctly as **bold text**
- Display `_italic text_` correctly as _italic text_
- Display `[link text](url)` as [link text](url)
- Cleanup display of `/gif` to turn:
```
<foo>  */gif [name](URL)*
<foo> ![GIF for 'name'](URL)
```

Into:
```
<foo> /gif name
<foo> URL
```

When used with [`erc-image`](https://github.com/kidd/erc-image.el) this
ensures GIFs are only displayed once, and that they are hyperlinked via
just their `name` for easy display as well.

## Installation

### MELPA (coming soon...)

The preferred way to install `erc-matterircd` is via
[MELPA](http://melpa.org) - then you can just <kbd>M-x package-install RET
erc-matterircd RET</kbd>

To enable then simply add the following to your init file:

```emacs-lisp
(require 'erc-matterircd)
```

Or is you use [`use-package`](https://github.com/jwiegley/use-package) this
can be simplified to:

```emacs-lisp
(use-package erc-matterircd
  :ensure t)
```

### Manual

If you would like to install the package manually, download or clone it and
place within Emacs' `load-path`, then you can require it in your init file
like this:

```emacs-lisp
(require 'erc-matterircd)
```

## Usage

```emacs-lisp
(require 'erc-matterircd)
(setq erc-matterircd-server "mattermost.server")
(setq erc-matterircd-team "mytest")
(setq erc-matterircd-password "password")
(add-to-list 'erc-modules 'matterircd)
(erc-update-modules)
```

```emacs-lisp
;; Then connect to matterircd via erc as normal - assuming it is running on
;; localhost, port 6667 and using mynick as the mattermost username
(erc :server "localhost" :port "6667" :nick "mynick")
```

## Development

### Unit tests

Run the ERT unit test suite:

```bash
emacs --batch --no-init-file -L . -l test/erc-matterircd-test.el \
  --eval "(ert-run-tests-batch-and-exit)" 2>&1
```

### Integration tests

The integration tests exercise the mode against a real matterircd and
Mattermost instance.  They require Docker (with the Compose plugin) and a
working Emacs installation. They can be run as follows:

```bash
bash test/integration/run.sh
```

The test script will then:

1. Starts a throwaway Mattermost instance via Docker Compose
2. Provisions a test team, user, and channel via the Mattermost REST API
3. Downloads matterircd (if not already cached in `test/integration/.bin/`)
4. Connects ERC to matterircd in Emacs batch mode and runs the test suite
5. Tears everything down on exit

**Environment variables** — all have sensible defaults; override as needed:

| Variable | Default | Description |
|---|---|---|
| `MM_URL` | `http://localhost:8065` | Mattermost base URL |
| `IRC_HOST` | `127.0.0.1` | matterircd bind address |
| `IRC_PORT` | `6667` | matterircd IRC port |
| `MM_TEAM` | `testteam` | Mattermost team name |
| `MM_NICK` / `MM_PASSWORD` | `testuser` / `TestUser1!` | ERC credentials |
| `MM_CHANNEL` | `#testchannel` | Channel to test in |
| `MATTERIRCD_VERSION` | `0.30.0` | matterircd release to download |
| `KEEP_SERVICES` | `0` | Set to `1` to leave Mattermost running after the run |

To point the tests at an already-running Mattermost instance instead of
starting one via Docker, set `MM_URL` (and the credential variables) and
export `MM_ADMIN_TOKEN` and `MM_CHANNEL_ID` yourself before calling
`run.sh`.

## License

Copyright © 2020 Alex Murray

Distributed under GNU GPL, version 3.
