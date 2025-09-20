# Table of Contents

1.  [Demo](#orgb0ead41)
2.  [Quick Start](#org819648e)
3.  [Issues and Limitations](#orgaa5d9aa)

This package allows ServiceNow developers edit their Script Includes, UI Scripts, and other artifacts, directly in Emacs. It provides a configurable way to synchronize ServiceNow records with local files and buffers.


<a id="orgb0ead41"></a>

# Demo

For a quick demo, see: <https://youtu.be/Boo0oO2fqhA>


<a id="org819648e"></a>

# Quick Start

This package depends on [ServiceNow.el](https://github.com/julian-hoch/ServiceNow.el), so please install and configure that first. Make sure you can log in to the instance from Emacs. Then, install this package. If you use straight, you can use the following snippet. Otherwise adapt as needed.

```emacs-lisp
(use-package snsync
  :straight (:host github :repo "julian-hoch/snsync"))
```

To download a Script Include or UI Script in a (temporary) buffer, use command `snsync-download-buffer`. To save your changes back to the instance, use `snsync-upload-buffer`. To update the buffer with the latest data from the instance, use `snsync-reload-buffer`.

**NOTE**: These actions do NOT check for potential conflicts and will happily overwrite any local changes you made.

If you want to work with persistent files, you can use the command `snsync-get-file` instead. This will save the data to a file in a local directory, and set some file-local variables so you can later continue working with it.

There is also a “do what I mean” command, `snsync-dwim`, which will try to guess what you want to do based on the current context. It will either upload your changes, if the source is not modified in the instance, or download the latest data from the instance, if it is only modified there. If both are modified, it will ask you what to do, and allow you to resolve the conflict (using `ediff`).


<a id="orgaa5d9aa"></a>

# Issues and Limitations

-   Content is trimmed of leading and trailing whitespace. This could be improved. It helps with narrowing and content hashing, but it also means that adding space at the start or end of a record will not be considered a meaningful change, and whitespace will be removed on the next sync.

-   Currently, we can only retrieve or update existing records. Creating new records directly from Emacs is not yet implemented.
