- [Abstract](#org84772bd)
- [Dependencies](#org135c376)
- [Getting Started](#org0c65c20)
- [Preferred (by me) keybindings](#orgb08fad9)



<a id="org84772bd"></a>

# Abstract

So I **do** love Emacs and it **is** my favorutive ~~editor~~ lisp-interpreter but I also know tiny bit of vim key-bindings and holy cow those can do a lot of things in very few keystrokes. The thing that blew me away in vim was `di(` ( **d\*elete \*i\*nside \*(** ) which deletes the thing contents in the surrounding **(*content*)**. Oh, and by the way `yi(` (**y\*ank \*i\*nside \*(** ) copies the contents. You can pretty much chose the actions to operate on *something*, a few strokes and **<span class="underline">bham</span>**! You&rsquo;ve done it. Emacs is not really like that. Emacs doesn&rsquo;t have those little targeted text editing operations. I&rsquo;ve written some simple functions for saving, marking and killing /sexp/s that kinda imitates the vim way of doing things. Basically - I am ripping it off but shut up. The functions come in handy a lot of times in my day to day life.


<a id="org135c376"></a>

# Dependencies

As you&rsquo;ve probably noticed `C-M` in like kind of a prefix for all *sexp*-operations The code uses some functions from [smartparens](https://github.com/Fuco1/smartparens) so you will have to have it in your load-path.


<a id="org0c65c20"></a>

# Getting Started

I use GNU Emacs version 25 and everything is ok but the package should run with no problems on anything newer than GNU Emacs 24. The package is not yet on MELPA so you will have to loaded manually. Clone the repository:

> git clone <https://github.com/palikar/arnaud-sexp>

and load the *arnaud-sexp.el* file

```emacs-lisp
(load "~/path/to/arnaud-sexp")
```

If the file is in your load-path you can require it with:

```emacs-lisp
(require 'arnaud-sexp)
```


<a id="orgb08fad9"></a>

# Preferred (by me) keybindings

On requiring the package does not sets any key bindings so feel free to add bindings for the functions either globally of to your personal key-map. I personally use these:

| Keystroke   | Description                                              |
|----------- |-------------------------------------------------------- |
| `C-M-k`     | Kill erverything inside the current *sexp*               |
| `C-M-K`     | Kill the current *sexp* and the                          |
| `C-M-SPC`   | Mark erverything inside the current *sexp*               |
| `C-M-S-SPC` | Mar the current *sexp*                                   |
| `C-M-w`     | Save everything inside the current *sexp* into kill ring |
| `C-M-W`     | Save the current *sexp* into kill ring                   |
