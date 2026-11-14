# Examples

最小示范文件，不属于作业或练习：

- `main.hs` — 最小的 Haskell `IO` 程序：打印 `Hello`，计算 `2 + 3`。
- `Hello.agda` — 最小的 Agda 模块：自定义 `Bool` 与 `not`。

运行：

```bash
runghc examples/main.hs
agda --no-libraries examples/Hello.agda
```
