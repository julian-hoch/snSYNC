
# Table of Contents

1.  [Quick Start](#orgc9111a8)

This package allows ServiceNow developers edit their Script Includes, UI Scripts (and other artifacts) directly in Emacs.  It provides a configurable way to synchronize ServiceNow records with local files.


<a id="orgc9111a8"></a>

# Quick Start

This package depends on [ServiceNow.el](https://github.com/julian-hoch/ServiceNow.el), so please install and configure that first.  Then, install this package, e.g. with:

    (use-package snsync
      :straight (:host github :repo "julian-hoch/snsync"))

To download a Script Include or UI Script in a (temporary) buffer, use command `snsync-download-record`.
To save your changes back to the instance, use `snsync-upload-buffer`.  To update the buffer with the latest data from the instance, use `snsync-reload-buffer`.

**NOTE**: These actions do NOT check for potential conflicts and will happily overwrite any local changes you made.

If you want to work with persistent files, you can use the command `snsync-get-file` instead.  This will save the data to a file in a local directory, and set some file-local variables so you can later continue working with it.

