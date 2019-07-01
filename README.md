- [Abstract](#org6053871)
- [Dependencies](#orgceed028)
- [Getting Started](#orgf05f829)
- [Preferred (by me) keybindings](#orgd4e2d82)



<a id="org6053871"></a>

# Abstract

So I do love Emacs and it is my favorite ~~editor~~ lisp-interpreter but I also know tiny bit of vim key-bindings and holy cow those can do a lot of things in very few keystrokes. The thing that blew me away in vim was `di(` (*delete inside (* ) which deletes the contents in the surrounding braces. Oh, and by the way `yi(` (*yank inside(* ) copies the contents. You can pretty much chose the actions to operate on *something*, a few strokes and bham! You&rsquo;ve done it. Emacs is not really like that. Emacs doesn&rsquo;t have those little targeted text editing operations. I&rsquo;ve written some simple functions for saving, marking and killing /sexp/s that kinda imitates the vim way of doing things. Basically - I am ripping it off but oh well. The functions come in handy a lot of times in my day to day life.


<a id="orgceed028"></a>

# Dependencies

The code uses some functions from [smartparens](https://github.com/Fuco1/smartparens) so you will have to have it in your load-path.


<a id="orgf05f829"></a>

# Getting Started

I use GNU Emacs version 25 and everything is ok but the package should run with no problems on anything newer than GNU Emacs 24. The package is not yet on MELPA so you will have to loaded manually. Clone the repository:

> git clone <https://github.com/palikar/arnaud-sexp>

and load the *vsexp.el* file

```emacs-lisp
(load "~/path/to/vsexp")
```

If the file is in your load-path you can require it with:

```emacs-lisp
(require 'vsexp)
```


<a id="orgd4e2d82"></a>

# Preferred (by me) keybindings

Requiring the package does not set any key bindings so feel free to add bindings for the functions either globally or to your personal key-map. I personally use these:

| Keystroke   | Description                                               |
| `C-M-k`     | Kill erverything inside the current *sexp*                |
| `C-M-K`     | Kill the current *sexp* and the braces themselves         |
| `C-M-SPC`   | Mark erverything inside the current *sexp*                |
| `C-M-S-SPC` | Mark erverything outside the current *sexp*               |
| `C-M-w`     | Save everything inside the current *sexp* into kill ring  |
| `C-M-W`     | Save everything outside the current *sexp* into kill ring |
| `C-c w i`   | Mark inside thing (with prompt)                           |
| `C-c w o`   | Mark outside thing (with prompt)                          |

As you&rsquo;ve probably noticed `C-M` in like kind of a prefix for all *sexp*-operations. The last two commands will first prompt you for character and will then mark the right region closed in the symbol. If the entered symbol is a bracket, the marked region will be between the corresponding brackets (just like in Vim).



And my key-bindings configuration is:

```emacs-lisp
(define-key my-keys-mode-map (kbd "C-M-SPC") 'arnaud-mark-sexp)
(define-key my-keys-mode-map (kbd "C-M-k") 'arnaud-kill-sexp)
(define-key my-keys-mode-map (kbd "C-M-S-SPC") 'arnaud-mark-sexp-whole)
(define-key my-keys-mode-map (kbd "C-M-S-k") 'arnaud-kill-sexp-whole)
(define-key my-keys-mode-map (kbd "C-M-w") 'arnaud-kill-save-sexp)
(define-key my-keys-mode-map (kbd "C-M-S-w") 'arnaud-kill-save-sexp-whole)

(define-key my-keys-mode-map (kbd "C-c w i") 'vsexp-mark-inside)
(define-key my-keys-mode-map (kbd "C-c w o") 'vsexp-mark-outside)

```
