miso-from-html
===================

Convert HTML into [miso](https://github.com/dmjio/miso) `View` syntax.

[![asciicast](https://asciinema.org/a/MHZ5r4muWitBshkXhjtLHUBQQ.png)](https://asciinema.org/a/MHZ5r4muWitBshkXhjtLHUBQQ)

### Features
 - Strips comments
 - Pretty prints style tags as a Haskell `Map` from `Data.Map`

### Usage

Given some HTML

```html
<nav class="navbar" role="navigation">
  <div class="navbar-brand">
    <a class="navbar-item" href="https://bulma.io">
      <img src="https://bulma.io/images/bulma-logo.png" width="112" height="28">
      <a>ok<p>hey</p></a>
    </a>
  </div>
</nav>
```

Convert it to [miso](https://github.com/dmjio/miso) `View` syntax.

```bash
$ cabal run miso-from-html < index.html
```

Result

```haskell
nav_
    [ class_ "navbar"
    , role_ "navigation"
    ]
    [ div_ [ class_ "navbar-brand" ]
	[ a_
	    [ class_ "navbar-item"
	    , href_ "https://bulma.io"
	    ]
	    [ img_
		[ src_ "https://bulma.io/images/bulma-logo.png"
		, width_ "112"
		, height_ "28"
		]
	    , a_ []
		[ "ok"
		, p_ [][ "hey" ]
		]
	    ]
	]
    ]
```

### Test

```bash
$ nix-shell --run 'runghc Main.hs < index.html'
```