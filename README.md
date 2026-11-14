# haskell_projects

Haskell 与 Agda 的课程学习仓库，包含课程讲义、课后作业、练习程序和几个可独立构建的 Stack 项目。

## 目录

| 目录 | 内容 |
| --- | --- |
| [FPslides/](FPslides/) | 课程讲义。`2025/` 为往年资料，`2026/` 为当前资料；`2026/work/` 是生成讲义的 Python 脚本，`2026/output/` 是生成的 PDF，`2026/tmp/` 是中间产物。 |
| [Homework/](Homework/) | 按 Lesson 编号的课后作业。`HWn.hs` 是正式作业，`NN-M.hs` 是课堂或课后练习。 |
| [ProblemsByGPT/](ProblemsByGPT/) | 额外的 GPT 出题练习，文件名使用月日（如 `1023.hs`）。 |
| [TestPrograms/](TestPrograms/) | 课堂演示和实验代码，例如 `Countdown.hs`、`MonadicParser/` 以及 Functor/Applicative/Monad 相关示例。 |
| [stackwork/](stackwork/) | 可独立构建的 Stack 项目：`guess`、`qsort`、`random-num`。 |
| [examples/](examples/) | 最小示范文件：`main.hs` 和 `Hello.agda`，不属于作业或练习。 |

根目录只放仓库级文件（README、忽略规则等），新的程序放进对应目录。

## 运行方式

### 单个 Haskell 文件

只有模块、没有 `main` 的文件用 GHCi 加载：

```bash
ghci Homework/Lesson16/HW11.hs
```

带 `main` 的程序可以直接运行：

```bash
runghc examples/main.hs
```

只做类型检查、不生成 `.hi`/`.o`：

```bash
ghc -fno-code -fno-write-interface Homework/Lesson16/HW11.hs
```

### Stack 项目

三个项目各自包含 `stack.yaml`，都使用 lts-24.59 快照和 USTC 镜像：

```bash
cd stackwork/guess
stack build
stack exec guess-exe
```

其他可执行文件分别是 `qsort-exe` 和 `random-num-exe`。

### Agda

```bash
agda Homework/Lesson17/HW11.agda
```

需要 Agda 2.8.0.2 和 agda-stdlib 2.4。stdlib 通过 `%APPDATA%\agda\libraries` 注册；换机器时需要重新配置。

### 讲义生成脚本

脚本用 `reportlab` 生成 PDF，建议用 Python 3.12 环境运行：

```bash
uv run --python 3.12 --with reportlab python FPslides/2026/work/build_haskell_reference.py
uv run --python 3.12 --with reportlab python FPslides/2026/work/build_haskell_one_page.py
```

输出在 [FPslides/2026/output/pdf/](FPslides/2026/output/pdf/)；脚本内部按自身位置解析路径，因此从仓库根目录运行即可。

## 环境版本

| 工具 | 版本 |
| --- | --- |
| GHC | 9.10.3 |
| Stack | 3.11.1 |
| Stackage 快照 | lts-24.59 |
| Agda | 2.8.0.2 |
| agda-stdlib | 2.4 |
| Python（讲义脚本） | 3.12 + reportlab |

## 约定

- 作业放在 `Homework/LessonNN/`：正式作业用 `HWn.hs`，练习用 `NN-M.hs`。
- 新增练习/实验放 `TestPrograms/` 或 `gpt-problems/`；最小示范放 `examples/`。
- 不提交生成物：`*.agdai`、`__pycache__/`、`.stack-work/`、`*.hi`、`*.o` 等已在 `.gitignore` 中忽略。
