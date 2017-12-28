The canonical way to install CIDER is via `package.el` (Emacs's built-in package
manager), but it can be installed manually or via alternative package managers such
as `el-get`.

## Prerequisites

You'll need to have Emacs installed (preferably the latest stable
release). If you're new to Emacs you might want to go through
[the guided tour of Emacs](https://www.gnu.org/software/emacs/tour/index.html)
and the built-in tutorial (just press <kbd>C-h t</kbd>).

CIDER officially supports Emacs 24.4+, Java 7+ and Clojure(Script) 1.7+.
CIDER 0.10 was the final release which supported Java 6 and Clojure 1.5 and 1.6.

You'll also need a recent version of your favorite build tool (Leiningen, Boot
or Gradle) to be able to start CIDER via `cider-jack-in`. Generally it's a good
idea to use their latest stable versions.

**CIDER does not support ClojureCLR.**

## Installation via package.el

CIDER is available on the two major `package.el` community
maintained repos -
[MELPA Stable](http://stable.melpa.org)
and [MELPA](http://melpa.org).

You can install CIDER with the following command:

<kbd>M-x package-install [RET] cider [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'cider)
  (package-install 'cider))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is a reasonable way of
obtaining CIDER, as the `master` branch is normally quite stable
and serious regressions there are usually fixed pretty quickly.

Generally, users of the non-adventurous kind are advised to stick
with the stable releases, available from MELPA Stable.
In Emacs 24.4+, you can pin CIDER to always use MELPA
Stable by adding this to your Emacs initialization:

```el
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
```

**CIDER has dependencies (e.g. `queue` & `seq`) that are only available in the
  [GNU ELPA repository](https://elpa.gnu.org/). It's the only package repository
  enabled by default in Emacs and you should not disable it!**

## Installation via use-package

`use-package` can be used to install CIDER via the `package.el`'s repositories
[MELPA Stable](http://stable.melpa.org) and [MELPA](http://melpa.org).

If you wanted to install the version of CIDER which is what is to be found in
the `master` branch, declare the following in your Emacs initialization file
(`.emacs` or `init.el`):

```el
(use-package cider
  :ensure t)
```

However, if you wanted to be a bit more conservative and only use the stable
releases of CIDER, you'd declare the following:

```el
(use-package cider
  :ensure t
  :pin melpa-stable)
```

After placing one of the above s-expressions, evaluate it, for it to take effect
by entering: <kbd>C-x C-e</kbd>.

For further configuration options with `use-package`, consult the
official [use-package repository](https://github.com/jwiegley/use-package).


## Installation via el-get

CIDER is also available for installation from
the [el-get](https://github.com/dimitri/el-get) package manager.

Provided you've already installed `el-get` you can install CIDER with the
following command:

<kbd>M-x el-get-install [RET] cider [RET]</kbd>

## Manual installation

Alternatively you can simply load CIDER in your Emacs straight from its source
repo. Assuming you've already obtained the code you should add something like
this to your Emacs configuration:

```el
;; load CIDER from its source code
(add-to-list 'load-path "~/projects/cider")
(require 'cider)
```

Just keep in mind that you'll have to manually install all the packages CIDER
depends on in advance.

## CIDER's nREPL middleware

Much of CIDER's functionality depends on the presence of CIDER's
own [nREPL middleware](https://github.com/clojure-emacs/cider-nrepl). Starting
with version 0.11, When `cider-jack-in` (<kbd>C-c M-j</kbd>) is used, CIDER
takes care of injecting it and its other dependencies.

**`profiles.clj` or `profile.boot` don't need to be modified anymore for the above use case!**

If you don't want `cider-jack-in` to inject dependencies automatically, set
`cider-inject-dependencies-at-jack-in` to `nil`. Note that you'll have to setup
the dependencies yourself (see the section below), just as in CIDER 0.10 and older.

CIDER can also inject a Clojure dependency into your project, which is useful,
for example, if your project defaults to an older version of Clojure than that
supported by the CIDER middleware. Set `cider-jack-in-auto-inject-clojure`
appropriately to enable this.

If a standalone REPL is preferred, you need to invoke `cider-connect` (instead
of `cider-jack-in`) and you'll need to manually add the dependencies to your
Clojure project (explained in the following section).

### Setting up a standalone REPL

#### Using Leiningen

Use the convenient plugin for defaults, either in your project's
`project.clj` file or in the :repl profile in `~/.lein/profiles.clj`.

```clojure
:plugins [[cider/cider-nrepl "x.y.z"]]
```

A minimal `profiles.clj` for CIDER would be:

```clojure
{:repl {:plugins [[cider/cider-nrepl "0.16.0"]]}}
```

**Be careful not to place this in the `:user` profile, as this way CIDER's
middleware will always get loaded, causing `lein` to start slower.  You really
need it just for `lein repl` and this is what the `:repl` profile is for.**

#### Using Boot

Boot users can configure the tool to include the middleware automatically in
all of their projects using a `~/.boot/profile.boot` file like so:

```clojure
(require 'boot.repl)

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.16.0"]])

(swap! boot.repl/*default-middleware*
       conj 'cider.nrepl/cider-middleware)
```

For more information visit [boot-clj wiki](https://github.com/boot-clj/boot/wiki/Cider-REPL).

### Using embedded nREPL server

If you're embedding nREPL in your application you'll have to start the
server with CIDER's own nREPL handler.

```clojure
(ns my-app
  (:require [clojure.tools.nrepl.server :as nrepl-server]
            [cider.nrepl :refer (cider-nrepl-handler)]))

(defn -main
  []
  (nrepl-server/start-server :port 7888 :handler cider-nrepl-handler))
```

It goes without saying that your project should depend on `cider-nrepl`.

***

`x.y.z` should match the version of CIDER you're currently using (say `0.16.0`).
For snapshot releases of CIDER you should use the snapshot of the plugin as well
(say `0.16.0-SNAPSHOT`).
