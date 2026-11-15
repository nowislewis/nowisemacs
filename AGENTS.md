# AI 协作与长期工程规范 (AGENTS.md)

> **Part I** 定义跨项目通用的软件工程与重构准则。**Part II** 定义 `~/.emacs.d` 的架构边界与构建定制。

---

## Part I: 通用长期工程与重构准则 (Universal Engineering Principles)

### 1. 底座优先与语义压缩（Built-ins First & Semantic Compression）
1. **标准底座优先（Built-ins First）**：
   - 写新代码前，先检索语言标准库与项目现有模块是否提供现成原语或配置项。
   - 优先组合内置原语解决问题，不重复造轮子，不轻易引入外部依赖。
2. **暴露假设与二次设计（Surface Assumptions & Design It Twice）**：
   - 需求存在歧义或有更简方案时，先列出假设与取舍并请用户确认，不静默猜测。
   - 设计核心模块接口时，至少构思两种不同方案，按调用杠杆（Leverage）与局部性（Locality）择优。
3. **具象先行，语义压缩（Semantic Compression / YAGNI）**：
   - 先写直接解决当前问题的具象代码，只用 30 行能写清的功能绝不写 100 行。
   - 至少出现两个真实适配器（Adapter）时才提取接缝（Seam），不为单一实现预造抽象层。
   - **提炼领域原语**：同一状态判定、元数据读写或归一化出现 ≥2 处时，自底向上压缩为单一内部原语。前提是能用一句话说出它维护的不变量。说不出的重复留在调用点，按第 2 条第 1 款的删除测试法判断。
   - **内联退化的原语**：共享原语开始按调用方加参数和条件分支时，把它内联回各调用点。删除每处用不到的分支，再重新判断是否合并。
   - **收敛数据泥团（No Data Clumps）**：多处结伴传递 ≥3 个参数时，收拢为单一结构体或 plist。

### 2. 深模块与正交分层（Deep Modules & Functional Core）
1. **窄接口、深实现（Deep Modules）**：
   - 模块（Module）对外暴露最窄的接口（Interface），将复杂性向下拉入实现内部（Pull Complexity Downwards）。
   - 保持线性流程在单个深函数内完成，不把清晰逻辑拆碎成十几个透传上下文的浅函数。
   - **删除测试法**：若删除某辅助函数后整体复杂度消失，说明它是多余中间人，直接内联删除。
2. **纯逻辑内核与薄副作用外壳（Functional Core, Imperative Shell）**：
   - 将纯数据转换与状态推导写成无副作用的纯函数，方便独立测试与多处调用。
   - 将 I/O、UI 渲染与进程通信收拢在最外层的薄命令外壳中。
3. **在接缝处解析，消解异常（Parse, Don't Validate & Define Errors Out of Existence）**：
   - 在模块接缝入口处将外部输入一次性解析为满足不变量的内部结构，不在内部散落防御性判空。
   - 优先设计幂等操作语义，在原语内部自然消化边界情况，减轻调用方的异常处理负担。

### 3. 单一真源与架构卫生（Single Source of Truth & Clean Contracts）
1. **单一真源与非法状态不可表示（SSOT & State Invariants）**：
   - 每份事实在系统中只保留一个权威真源。
   - 派生状态通过实时计算或单向投影获得，不将可派生数据持久化为第二真源。
   - 区分空值语义，不用同一个 `nil`/`null` 混表“未初始化”与“查询结果为空”。
2. **单向数据流与公开契约（Unidirectional Flow & Public Contracts）**：
   - 跨模块、跨进程或本地与远端协同坚持单一通信通道与单向流式协议。
   - 优先调用公开契约（CLI、公开 Hook、标准配置项），不耦合第三方库的私有实现。
   - 异步或并发任务显式隔离上下文，在失败或取消时恢复用户草稿与既有状态。

### 4. 重构、测试与排障纪律（Tidy First, Seam Testing & Tight Loops）
1. **外科手术式修改与结构先行（Surgical Changes & Tidy First）**：
   - 每一行修改直接服务于当前目标，不顺手重排无关代码或格式。
   - 旧结构阻碍新功能时，先做保行为的结构整理（Tidy），验证通过后再修改运行时的行为。
2. **破旧立新，彻底清理（Replace, Don't Layer）**：
   - 引入新原语或修复根因时，同步迁移所有调用点。
   - 立即删除废弃参数、旧分支、死代码与失效测试，不让新旧两套机制并存。
3. **接口即测试面与垂直切片（Interface as Test Surface & Vertical Slices）**：
   - 按“一个接缝测试 → 最小实现 → 验证通过”的垂直切片（Tracer Bullets）推进。
   - 测试只跨公共接口验证可观察行为，不耦合内部私有结构，确保内部重构不破坏测试。
   - 断言期望值必须取自独立真源（字面量或规格样例），不在测试中重抄实现逻辑（No Tautological Tests）。
   - 新增测试必须接入统一构建入口（如 `make test`），每次修改后运行编译与测试验证。
4. **紧反馈环排障（Tight Red-Capable Loop）**：
   - 排查复杂 Bug 或性能回退时，先构造一条稳定复现（变红）、秒级运行的单命令反馈环，并最小化输入。
   - 未拿到红灯复现命令前，不凭空猜测原因或散弹式修改代码。
   - 临时调试探针统一带 `[DEBUG-xxx]` 前缀，修复后通过 `grep` 彻底删除。

---

## Part II: `~/.emacs.d` 专属定制 (Project Customization)

### 1. 仓库架构与职责边界（最重要）
- **`init.org` 是配置的唯一真源**：
  - `init.el` 由 `init.org` 通过 `make init-build` 自动生成，只修改 `init.org`，不直接手改 `init.el`。
  - `init.org` 只放声明式组装：`(setup ...)` 块、变量配置（`setopt` / `setq`）、按键绑定（`keymap-set`）、hook 挂载。
- **`lisp/` 存放功能实现**：
  - 非平凡函数、自定义 minor-mode、异步工作流与独立特性放入 `lisp/nowis-<feature>.el`，不塞进 `init.org`。
  - 对外交互命令或入口函数标注 `;;;###autoload`，由 Capsule 自动提取，不手改 `lisp/lisp-autoloads.el`。
- **`lib/` 为 Git Submodule 管理的第三方包**：
  - 由 `lisp/capsule.el`、`.gitmodules` 与 `Makefile` 统一管理，除用户明确要求打补丁外，不修改 `lib/` 下代码。

### 2. 标准构建与验证命令
修改 `init.org` 或 `lisp/*.el` 后，在终端运行对应命令完成同步与自检：
- 修改 `lisp/*.el` 实现或 autoload 且修改 `init.org`：`make -C ~/.emacs.d compile-lisp init-build`
- 仅更新 `lisp/lisp-autoloads.el`（不重编）：`make -C ~/.emacs.d autoloads`
- 仅修改 `init.org`：`make -C ~/.emacs.d init-build`
- 修改 `lisp/*.el` 核心模块或构建逻辑时运行回归测试：`make -C ~/.emacs.d test`
- `init-build` 末尾自动运行 `init-check`，它真正加载一次 `init.el`。只 tangle 不加载，看不出运行时才报的错（例如读取尚未加载的包所绑定的变量）。这一步失败就不算改完。

### 3. Emacs 专属工程约束
1. **内置底座与公开契约**：
   - 优先复用 Emacs Built-in（`project.el`、`vc`、`subr-x`、`seq`、`cl-lib`、`thingatpt`、`comint`）及已安装包的原生配置项（如 `git-commit-usage-message`）。
   - 优先使用公开 Hook 与配置变量，不对第三方包私有函数（`--`）做脆弱的 `advice-add`。
2. **终端协同与异步隔离**：
   - Emacs 与外部终端（如 Pi）协同采用 `OSC 7501` 单向透传协议，保持本地与远端 TRAMP 行为同构。
   - 通过流式协议同步状态，不向活动终端粘贴命令并发送回车（如 `paste "/cmd" + RET`）。
   - 异步请求（如 `gptel-request`）显式绑定隔离 `gptel-use-tools`、`gptel-use-context` 与 `:transforms`。
3. **就地按键可发现性（In-situ Keybinding Hints）**：
   - 新增专用模式、临时编辑工作台或低频模态状态时，在进入时给出简短按键提示。
   - 按键提示统一使用 `(substitute-command-keys "... \\[command] ...")`，不硬编码按键字符串（如 `"[C-c C-c]"`）。
   - **与 `breadcrumb-mode` 共存**：关联磁盘文件的 buffer（`buffer-file-name` 非空）由 `breadcrumb-mode` 占用顶部栏，提示走 `message` 或内置 usage 变量。
   - 仅无磁盘文件的纯临时 buffer 可使用 `header-line-format`。
4. **Elisp 编码与零编译警告**：
   - 文件首行启用 `;;; ... -*- lexical-binding: t; -*-`，文件结尾包含 `(provide '...)`。
   - 命名空间：公共接口命名为 `nowis-<module>-<name>`，私有实现命名为 `nowis-<module>--<name>`。
   - Docstring 首行写完整句子，单行宽度 ≤ 80 字符。
   - 统一使用 Emacs 30/31 现代 API（`when-let*`、`and-let*`、`keymap-set`、`keymap-global-set`）。
