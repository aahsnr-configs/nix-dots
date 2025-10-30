### **Expanded Prompt**

Whenever the doom emacs configures a package using the following format, it gives off the error `Wrong type argument: proper-list-p`

```
(use-package! eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))
```

The above code is an example. The error is universal for any emacs package configured this way.
