Excellent question. Managing multiple snippet files is key to keeping your configuration organized as it grows.

In Doom Emacs, you have two primary, excellent ways to load snippets. The best method depends on how your snippets are structured.

### Method 1: The Directory-Based Approach (Recommended for most cases)

This is the most common and idiomatic way to manage snippets in Doom Emacs. The `:editor snippets` module is pre-configured to automatically find and load any snippets you place in the correct directory structure. You don't need to add any `load!` calls to your `config.el` for this to work.

**How it works:** You create subdirectories within `~/.config/doom/snippets/` that match the name of the major-mode you want the snippets to be active in (e.g., `latex-mode`, `python-mode`).

#### Step-by-Step Guide:

1.  **Organize Your Snippets:** Create a directory for each major mode inside `~/.config/doom/snippets/`.

2.  **Create `.yas-snippet` Files:** Inside each mode directory, each individual snippet is its own file ending in `.yas-snippet`.

Here is the recommended directory structure:

```
~/.config/doom/
└── snippets/
    ├── latex-mode/
    │   ├── article.yas-snippet
    │   ├── equation.yas-snippet
    │   ├── figure.yas-snippet
    │   └── ... (your other LaTeX snippets)
    │
    ├── org-mode/
    │   ├── todo.yas-snippet
    │   └── src_block.yas-snippet
    │
    └── python-mode/
        ├── class.yas-snippet
        └── main.yas-snippet
```

3.  **Format Your Snippet Files:** Each `.yas-snippet` file has a simple header and a body. For example, the contents of `latex-mode/article.yas-snippet` would look like this:

    ```yasnippet
    # -*- mode: snippet -*-
    # name: article
    # key: article
    # group: Templates
    # --
    \documentclass[11pt,a4paper]{article}
    \usepackage[utf8]{inputenc}
    \title{${1:Title}}
    \author{${2:Author}}
    \date{\today}

    \begin{document}

    \maketitle

    $0

    \end{end{document}
    ```

**Advantage:** This is fully automatic. Just add or remove `.yas-snippet` files, and Doom's YASnippet integration handles the rest. **No changes are needed in `config.el`**.
