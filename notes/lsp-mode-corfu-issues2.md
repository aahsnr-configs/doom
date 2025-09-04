# Issues

Let me search for current information about issues with these Emacs packages, particularly in the context of Doom Emacs.Let me get more specific information about the recent issue mentioned in the search results.Let me also check the blog post about orderless completion in lsp-mode for more technical details.Based on my research, here are the current issues that lsp-mode faces with corfu, orderless, and cape packages, especially in Doom Emacs:

## Major Issues

### 1. **Orderless Completion Style Conflicts**

The most significant issue is that lsp-mode interferes with orderless completion styles by modifying `completion-category-defaults` and adding specific settings for 'lsp-capf'. When you enter lsp-mode, you lose the powerful orderless matching capabilities that many users rely on.

**Solution**: Users need to add a hook to restore orderless behavior:

```elisp
(add-hook 'lsp-completion-mode-hook
  (lambda ()
    (setq-local completion-category-defaults
                (assoc-delete-all 'lsp-capf completion-category-defaults))))
```

### 2. **Inconsistent Completion Candidates in Doom Emacs**

There's a recent issue (reported just a week ago) where corfu with lsp-mode doesn't provide consistent completion candidates in Doom Emacs. This suggests ongoing stability problems with the integration.

### 3. **Filtering Problems**

LSP Mode completions aren't filtered properly when using corfu, though this is an older issue from 2021 that may have been addressed.

## Configuration Challenges

### 4. **Complex Setup Requirements**

To properly integrate these packages, users need specific configuration:

```elisp
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; disable lsp's own completion
  :init
  (defun setup-lsp-completion ()
    (setf (alist-get 'styles
                     (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (lsp-completion-mode . setup-lsp-completion))
```

### 5. **Doom Emacs Module Status**

While Doom Emacs has experimental corfu module support, it's still being developed and may not have full feature parity with the traditional company-mode setup. The integration is relatively new and still evolving.

## Recommended Approaches

1. **Disable LSP's built-in completion** and rely entirely on corfu + cape + orderless
2. **Use hooks to restore orderless behavior** after lsp-mode initializes
3. **Configure completion categories explicitly** to ensure proper interaction between all packages
4. **Stay updated** with Doom Emacs developments as the corfu module continues to mature

The main challenge is that lsp-mode was originally designed to work with company-mode, so using it with the newer corfu ecosystem requires careful configuration to prevent conflicts, especially around completion filtering and styling.

# Possible Solutions

Based on the issues I identified, here are comprehensive solutions to mitigate the problems with lsp-mode, corfu, orderless, and cape integration:

## 1. **Fix Orderless Completion Style Conflicts**

### Primary Solution - Hook-based Fix:

```elisp
(add-hook 'lsp-completion-mode-hook
  (lambda ()
    (setq-local completion-category-defaults
                (assoc-delete-all 'lsp-capf completion-category-defaults))))
```

### Alternative - Direct Category Override:

```elisp
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ; Let corfu handle completion
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless basic)))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))
```

## 2. **Proper Corfu Configuration for LSP**

### Basic Corfu Setup:

```elisp
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s)
  :init
  (global-corfu-mode)
  :config
  ;; Enable corfu in minibuffer
  (defun corfu-enable-in-minibuffer ()
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))
```

### LSP-specific Corfu Configuration:

```elisp
(use-package corfu
  :hook
  (lsp-completion-mode . (lambda ()
    ;; Adjust corfu settings for LSP
    (setq-local corfu-auto-delay 0.1)
    (setq-local corfu-auto-prefix 1))))
```

## 3. **Cape Integration for Enhanced Completion**

```elisp
(use-package cape
  :init
  ;; Add useful completion functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)

  ;; For programming modes, prioritize LSP but add cape functions
  :hook
  (prog-mode . (lambda ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'lsp-completion-at-point
                                       #'cape-dabbrev
                                       #'cape-file))))))
```

## 4. **Doom Emacs Specific Solutions**

### In `~/.doom.d/config.el`:

```elisp
;; Force orderless for LSP completions
(after! lsp-mode
  (add-hook 'lsp-completion-mode-hook
    (lambda ()
      (setq-local completion-category-defaults
                  (assoc-delete-all 'lsp-capf completion-category-defaults)))))

;; Ensure corfu works well with LSP
(after! corfu
  (setq lsp-completion-provider :none) ; Use corfu instead

  ;; Better LSP completion experience
  (add-hook 'lsp-mode-hook
    (lambda ()
      (setq-local corfu-auto-delay 0.1)
      (setq-local corfu-auto-prefix 1))))
```

### Module Configuration in `~/.doom.d/init.el`:

```elisp
:completion
(corfu +orderless)    ; Use corfu with orderless
;;company           ; Comment out company if using corfu

:tools
(lsp +peek)          ; LSP with peek functionality
```

## 5. **Advanced Filtering Solutions**

### Custom Completion Category Setup:

```elisp
(setq completion-category-overrides
      '((file (styles partial-completion))
        (command (styles orderless))
        (variable (styles orderless))
        (symbol (styles orderless))
        (lsp-capf (styles orderless))))
```

### Fine-tuned Orderless Configuration:

```elisp
(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (orderless-matching-styles '(orderless-literal
                               orderless-prefixes
                               orderless-initialism
                               orderless-regexp))
  ;; LSP-specific adjustments
  :config
  (defun orderless-lsp-dispatch (word _index _total)
    (cond
     ((string-prefix-p "." word) `(orderless-prefixes . ,(substring word 1)))
     ((string-prefix-p "#" word) `(orderless-initialism . ,(substring word 1)))
     (t word)))

  (add-hook 'lsp-completion-mode-hook
    (lambda ()
      (setq-local orderless-style-dispatchers '(orderless-lsp-dispatch)))))
```

## 6. **Performance Optimizations**

```elisp
;; Reduce LSP completion noise
(setq lsp-completion-show-detail nil
      lsp-completion-show-kind t
      lsp-completion-filter-on-incomplete t)

;; Optimize corfu for LSP
(setq corfu-min-width 80
      corfu-max-width 120
      corfu-count 14
      corfu-scroll-margin 4)
```

## 7. **Troubleshooting Configuration**

### Debug Completion Issues:

```elisp
;; Add to check what completion functions are active
(defun my/debug-completion ()
  "Debug completion setup"
  (interactive)
  (message "completion-at-point-functions: %s" completion-at-point-functions)
  (message "completion-styles: %s" completion-styles)
  (message "completion-category-defaults: %s" completion-category-defaults))
```

### Fallback for Problematic Modes:

```elisp
;; If certain modes have issues, configure them specifically
(add-hook 'python-mode-hook
  (lambda ()
    (when (featurep 'lsp-mode)
      (setq-local completion-at-point-functions
                  (list #'lsp-completion-at-point
                        #'cape-dabbrev
                        #'cape-file)))))
```

## 8. **Complete Working Example for Doom Emacs**

Here's a complete configuration that addresses most issues:

```elisp
;; In ~/.doom.d/config.el
(after! lsp-mode
  ;; Disable lsp's completion in favor of corfu
  (setq lsp-completion-provider :none)

  ;; Fix orderless integration
  (add-hook 'lsp-completion-mode-hook
    (lambda ()
      (setq-local completion-category-defaults
                  (assoc-delete-all 'lsp-capf completion-category-defaults))
      (setq-local completion-styles '(orderless basic)))))

(after! corfu
  ;; Optimize for LSP
  (setq corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-cycle t)

  ;; Better integration with LSP
  (add-hook 'lsp-mode-hook
    (lambda ()
      (setq-local corfu-auto t))))

(after! cape
  ;; Enhanced completion with cape
  (add-hook 'prog-mode-hook
    (lambda ()
      (when (bound-and-true-p lsp-mode)
        (setq-local completion-at-point-functions
                    (list (cape-capf-super #'lsp-completion-at-point
                                           #'cape-dabbrev)))))))
```

These solutions should resolve most of the integration issues between lsp-mode, corfu, orderless, and cape, providing a smooth and powerful completion experience in both vanilla Emacs and Doom Emacs configurations.
