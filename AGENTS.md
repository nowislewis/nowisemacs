# `~/.emacs.d` AI 协作与代码规范 (AGENTS.md)

## 1. 架构与职责边界（最重要）

- **`init.org` 是配置的唯一真源（Single Source of Truth）**：
  - `init.el` 由 `init.org` 通过 `make init-build` 自动 tangle 生成，**严禁只改 `init.el` 而不改 `init.org`**。
  - `init.org` 只负责**声明式组装**：`(setup ...)` 块、变量设定（`setopt` / `setq`）、按键绑定（`keymap-set`）、hook 挂载。
- **`lisp/` 存放功能实现**：
  - 任何非平凡的函数、自定义 minor-mode、异步工作流或独立特性，必须放到 `lisp/nowis-<feature>.el`（或对应模块文件）中，不要直接塞进 `init.org`。
  - 对外提供的交互命令或入口函数标注 `;;;###autoload`，由 Capsule 自动提取到 `lisp/lisp-autoloads.el`（严禁手改 `lisp-autoloads.el`）。
- **`lib/` 为 Git Submodule 管理的第三方包**：
  - 由 `lisp/capsule.el` + `.gitmodules` + `Makefile` 管理，除非用户明确要求给上游打补丁，否则不要直接修改 `lib/` 下的代码。

## 2. 修改后的标准构建与验证流程

每次修改 `init.org` 或 `lisp/*.el` 后，必须在终端运行对应命令完成同步与自检：

- 修改了 `lisp/*.el` 的 autoload 入口或实现 + 修改了 `init.org`：
  ```sh
  make -C ~/.emacs.d compile-lisp init-build
  ```
- 仅更新 `lisp/lisp-autoloads.el`（不重编）：
  ```sh
  make -C ~/.emacs.d autoloads
  ```
- 仅修改了 `init.org`：
  ```sh
  make -C ~/.emacs.d init-build
  ```
- 修改了 `lisp/capsule.el` 或构建逻辑时，运行回归测试：
  ```sh
  make -C ~/.emacs.d test
  ```

## 3. 长期主义与工程审美（核心准则）

1. **优先复用 Emacs 内置能力与现有基础设施**：
   - 写任何新代码前，先查 Emacs Built-in（如 `project.el`、`vc`、`subr-x`、`seq`、`cl-lib`、`thingatpt`、`comint`）及已安装库是否已有现成函数或变量（如直接配 `git-commit-usage-message` 而非写 hook 弹 message）。
   - 能组合内置原语解决的，绝不重新造轮子或引入新外部依赖。
2. **代码少而简明（Less is More）**：
   - 拒绝过度设计、防御性废话和投机性配置项（YAGNI）。
   - 用最少的代码表达最清晰的意图；如果一个功能 30 行能写清楚，绝不写 100 行。
3. **模块化与高内聚、可复用**：
   - 将【数据获取/纯逻辑转换】与【Buffer UI 副作用/交互命令】拆分为职责单一的小函数，方便在其他命令或测试中直接复用。
   - **提炼领域原语与谓词**：凡在多处重复出现的状态判定、元数据读写或格式归一化逻辑，必须收拢为单一自解释的内部原语（如 `--buffer-attached-p`、`--read-meta`），消除散落的低层模板代码。
   - 优先使用稳定的公开契约（CLI、公开 hook、原生 `defcustom`），避免对第三方包内部私有函数（`--`）做脆弱的 advice。
   - 异步请求（如 `gptel-request`）必须显式隔离上下文（绑定 `gptel-use-tools`、`gptel-use-context`、`:transforms`），并在失败/取消时保护或恢复用户的 buffer 草稿。
4. **单向数据流与禁止脆弱 Hack（Zero Fragile Hacks）**：
   - **单一通信通道**：跨进程/终端协同（如 Emacs 与 Pi）坚持单一真源与单向流式协议（如 `OSC 7501` 透传），保持本地与远端 TRAMP 行为天然同构，拒绝混用多层启发式猜路径或让子进程反向操纵宿主。
   - **严禁模拟按键注入改状态**：绝不通过向活动终端粘贴命令并发送回车（如 `paste "/cmd" + RET`）来同步内部状态，防止污染会话历史或误触发用户未写完的草稿。
5. **就地按键可发现性（In-situ Keybinding Hints）**：
   - 新增专用模式、临时编辑工作台（Workbench/Capture）或低频模态状态时，必须在进入时给出简短的按键提示，避免用户靠死记硬背。
   - 严禁硬编码按键字符串（如 `"[C-c C-c]"` 或 `"M-x ..."`），必须统一使用内置 `(substitute-command-keys "... \\[command] ...")`，兼具自动同步键位与原生 `help-key-binding` 高亮。
   - 注意与全局 `breadcrumb-mode` 共存：凡是关联磁盘文件的 buffer（`buffer-file-name` 非空），`header-line-format` 已被 `breadcrumb-mode` 占用，提示一律走 minibuffer `message`（或内置 usage 变量如 `git-commit-usage-message`）；仅无磁盘文件的纯临时 buffer 才可使用 `header-line-format`。

## 4. Elisp 编码规范

1. **文件头与词法作用域**：所有 `.el` 文件首行必须启用 `;;; ... -*- lexical-binding: t; -*-`，结尾包含 `(provide '...)`。
2. **命名空间约定**：
   - 模块公共函数/变量：`nowis-<module>-<name>`
   - 模块内部私有函数/变量：`nowis-<module>--<name>`
3. **零字节编译警告**：
   - Docstring 首行必须是完整句子，且每行宽度不超过 80 字符。
   - 使用 Emacs 30/31 现代 API（如使用 `when-let*` / `and-let*` 而非已废弃的 `when-let`，使用 `keymap-set` / `keymap-global-set`）。
