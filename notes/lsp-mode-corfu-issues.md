# Document Title

## ISSUES
### Navigating the Rapids: Current Challenges with lsp-mode and Corfu Completion in Emacs

**While `lsp-mode` and `corfu` offer a powerful and streamlined approach to code completion in Emacs, users may encounter a handful of persistent and emerging issues that can disrupt a smooth workflow. Recent discussions and bug reports from late 2024 and early 2025 highlight challenges ranging from inconsistent completion candidates and performance lag to tricky configurations, particularly when integrating the `orderless` completion style.**

One of the most prominent issues reported by users is the inconsistency of completion candidates provided by `lsp-mode` when using `corfu`. A notable example is a bug report in the Doom Emacs repository where `corfu` fails to consistently offer the expected completion candidates. This can manifest as a completion popup that either omits the desired candidate or presents a less relevant one, only to behave correctly on a subsequent, faster attempt. This suggests a potential race condition or a timing issue in how `lsp-mode` and `corfu` interact.

Performance issues, specifically lag, also continue to be a point of friction for some users. While not a new problem, the overhead of the Language Server Protocol (LSP) combined with the real-time nature of `corfu`'s completion popup can lead to noticeable delays, especially in larger projects or with more resource-intensive language servers. This can disrupt the fluidity of typing and code editing.

Configuration complexities, particularly when trying to achieve a specific completion behavior with `orderless`, are another recurring theme. `lsp-mode` has its own way of handling completion categories, which can sometimes clash with the desired `orderless` configuration. This often requires users to delve into custom hooks and configurations to ensure that `orderless`'s flexible matching is applied correctly to the candidates supplied by the LSP server. Without proper setup, users might find that the powerful filtering capabilities of `orderless` are not being utilized, reverting to a more basic prefix-based completion.

Furthermore, some users have reported issues with the stability of the completion popup itself. There have been instances of the `corfu` popup disappearing unexpectedly or behaving erratically, such as selections vanishing while navigating through the completion candidates. These glitches, while seemingly minor, can be disruptive to the user experience.

It's also important to note that the quality and nature of the completion candidates are heavily influenced by the specific language server being used. Sometimes, what appears to be an issue with `lsp-mode` or `corfu` can actually stem from the language server's implementation of the completion feature. This can include the server providing a limited or incomplete set of candidates in certain contexts.

In summary, while the combination of `lsp-mode` and `corfu` provides a modern and effective completion experience in Emacs, users should be aware of the following potential hurdles:

* **Inconsistent and unpredictable completion candidates.**
* **Performance lag, especially in larger projects.**
* **Configuration challenges, particularly with `orderless` integration.**
* **Occasional instability of the `corfu` completion popup.**
* **Server-side limitations affecting the quality of completions.**

Community forums and bug trackers remain valuable resources for troubleshooting these issues, with users often sharing workarounds and configuration snippets to alleviate these problems. As both `lsp-mode` and `corfu` are under active development, it is expected that some of these issues will be addressed in future updates.

### Solutions
## Taming the Beast: Practical Solutions for lsp-mode and Corfu Issues in Emacs

Navigating the intricacies of `lsp-mode` and `corfu` can sometimes feel like a white-water rafting expedition. While the combination is powerful, users can encounter turbulence in the form of inconsistent completions, performance drag, and configuration puzzles. Fortunately, the Emacs community has developed a range of effective strategies to mitigate these common issues, transforming a potentially frustrating experience into a smooth and productive one.

Here are some practical ways to address the current challenges faced when integrating `lsp-mode` with `corfu` for code completion.

### 1\. Tackling Inconsistent Completion Candidates

One of the most jarring issues is when the completion popup flickers with different results for the same input. This often points to a timing or caching problem in how completion functions (CAPFs) are handled.

**Solution:**
A highly effective solution, particularly for users of Doom Emacs and adaptable elsewhere, involves using the `cape-capf-buster` function from the `cape` package. This function helps to ensure that the list of completion candidates from `lsp-mode` is consistently fresh.

```emacs-lisp
(use-package lsp-mode
  :hook (lsp-completion-mode . (lambda ()
                                 (setq-local completion-at-point-functions
                                             (list (cape-capf-buster #'lsp-completion-at-point)
                                                   #'cape-file
                                                   #'cape-dabbrev)))))
```

This configuration, placed within a relevant hook, wraps the standard `lsp-completion-at-point` function, which can resolve inconsistencies observed during rapid typing.

### 2\. Fine-Tuning Performance and Reducing Lag

Performance issues with `lsp-mode` can often be traced back to Emacs's garbage collection, JSON processing, or an overly "chatty" language server.

**Solutions:**

  * **Adjust Garbage Collection:** `lsp-mode` generates a lot of transient data. Increasing the garbage collection threshold can prevent stuttering during completion.

    ```emacs-lisp
    (setq gc-cons-threshold (* 100 1024 1024)) ;; 100 MB
    (setq read-process-output-max (* 1024 1024)) ;; 1 MB
    ```

  * **Enable Native JSON Parsing:** Ensure your Emacs build has native JSON support (`libjansson`). This dramatically speeds up the communication with the language server. You can check this with `(functionp 'json-serialize)`.

  * **Tune Corfu's Behavior:** Adjusting when and how quickly Corfu appears can make the experience feel more responsive.

    ```emacs-lisp
    (setq corfu-auto t)
    (setq corfu-auto-delay 0.2) ; Adjust delay as needed
    ```

  * **Optimize `lsp-mode` Settings:** The official `lsp-mode` documentation provides a wealth of performance tips, such as disabling unnecessary UI features (`lsp-ui-sideline`, `lsp-ui-doc`) if they are not needed.

  * **Consider `lsp-bridge`:** For users who prioritize performance above all else and are willing to try a different architecture, `lsp-bridge` offers a compelling alternative. It runs the LSP communication in a separate asynchronous Python process, effectively preventing the language server from ever blocking the Emacs UI. Note that this replaces `corfu` with its own completion UI.

### 3\. Mastering `orderless` Integration

A common frustration is `lsp-mode` seemingly ignoring a carefully crafted `orderless` configuration. This happens because `lsp-mode` sets its own completion style for the `lsp-capf` category, overriding the global default.

**Solution:**
The fix is to re-assert your desired completion style within a hook that runs after `lsp-mode`'s completion setup is complete.

```emacs-lisp
(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion)))

(defun my-lsp-mode-setup-completion ()
  "Set completion styles for lsp-mode buffers."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless partial-completion)))

(add-hook 'lsp-completion-mode-hook #'my-lsp-mode-setup-completion)
```

This ensures that your `orderless` configuration is respected, providing powerful, out-of-order candidate filtering for LSP-provided completions.

### 4\. Stabilizing the Corfu Popup

If the `corfu` popup behaves erratically or disappears, it could be due to aggressive interruption of the completion process. Some `lsp-mode` backends may not handle this gracefully.

**Solution:**
The `cape` package provides another useful utility, `cape-wrap-noninterruptible`, which can be advised onto the `lsp-completion-at-point` function. This can help prevent race conditions where Corfu quits before the language server has had a chance to respond fully.

```emacs-lisp
(advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
```

*Note: This is a workaround for more persistent instability and may not be necessary for all users.*

### General Troubleshooting and Diagnostics

When in doubt, it's crucial to determine where the problem lies: `corfu`, `lsp-mode`, or the language server itself.

  * **Check the LSP Logs:** Use the command `M-x lsp-describe-session` and inspect the logs. The `*lsp-log*` buffer shows the raw JSON communication between Emacs and the server. If you see that the server is sending incorrect or no completion items, the issue is with the server, not your Emacs setup.
  * **Isolate `corfu`:** Temporarily disable `global-corfu-mode` and try to complete using the default `M-x completion-at-point`. If the problem persists, it's unlikely to be a `corfu`-specific issue.
  * **Start with a Minimal Configuration:** If all else fails, testing with a minimal `init.el` containing only `lsp-mode` and `corfu` can help identify if the problem stems from an interaction with another package.

By applying these targeted configurations and troubleshooting steps, you can significantly improve the stability, performance, and predictability of your `lsp-mode` and `corfu` setup, leading to a more powerful and enjoyable coding experience in Emacs.
