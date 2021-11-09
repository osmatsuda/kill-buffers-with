# kill-buffers-with

## Usage

Allows killing some buffers at one time like `ag-kill-buffers` or `tramp-cleanup-all-buffers`, but you can select buffers with a glob pattern and a search object.

A glob pattern is translated to a regexp and search for matched buffers from `(buffer-list)`. The following terms you can choose for a search object type:

* `buffer`
   * `(buffer-name BUFFER)`
* `file`
   * `(buffer-file-name BUFFER)`
   * or `(buffer-local-value 'list-buffers-directory BUFFER)`
* `mode`
   * `(buffer-local-value 'mode-name BUFFER)`

### Example

When you want to kill all buffers under a directry not only file-buffers but like dired and magit buffers.

```
---------- Buffer: Minibuffer ----------
M-x kill-buffers-with <RET>

Pattern: */path-to-kill/* <RET>

Select name type: file <RET>
---------- Buffer: Minibuffer ----------

---------- Echo Area ----------
Killed buffers ("__init__.py<foo>" "__init__.py<bar>" "path-to-kill" "magit: path-to-kill" "magit-process: kill-to-kill")
---------- Echo Area ----------
```

See [test.el](test.el) how a glob pattern translate to a regexp.

## Installation

Put [kill-buffers-with.el](kill-buffers-with.el) in your `load-path` directory and execute `(autoload 'kill-buffers-with "kill-buffers-with" nil t)`.


