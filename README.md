# erc-matterircd

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

## License

Copyright © 2020 Alex Murray

Distributed under GNU GPL, version 3.
