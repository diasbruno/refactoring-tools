# refactoring-tools

Tool designed to assist refactoring.

## install

### from source

Get `refactoring-tools` and add to load paths with:

```emacs-lisp
(load-to-list 'load-path "/PATH/TO/refactoring-tools")
```

## usage

Setup your refactoring-tools bindings like:

```emacs-lisp
(global-set-key (kbd "C-c C-r") 'refactoring-tools-run)
```

or...

```emacs-lisp
(global-set-key (kbd "S-r") 'refactoring-tools-run)
```
