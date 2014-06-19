ditaa-img
=========

Convert Markdown with Ditaa plain-text diagrams to Markdown with images.

Installation
------------

### Older way

Run `cabal install --global` or `cabal install --user` in the source
directory. This will install an executable `ditaa-img` to your

### Safer way

    cabal sandbox init
    cabal install --only-dep
    cabal build
    # now copy ./dist/build/ditaa-img/ditaa-img somewher in your $PATH

Usage
-----

Ditaa diagram format in github-flavored markdown:

<pre><code>
```ditaa
+-----+    +------+
|hello|--->|world!|
+-----+    +------+
```
</code></pre>

DITAA_CMD environment variable defines how Ditaa should be called.
INPUT and OUTPUT should be used as placeholders for the file names.
If not set, the default value is `ditaa INPUT OUTPUT`.

Run `ditaa-cmd -h` to see command line options.

See also
--------

  * [Ditaa](http://ditaa.sourceforge.net/)
