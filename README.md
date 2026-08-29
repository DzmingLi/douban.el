# douban.el

在 Emacs 里用 Org 编写并发布豆瓣长评、日记、读书笔记和普通广播。

> [!WARNING]
> 这是实验性软件，依赖豆瓣未公开的网页接口。接口随时可能变化。当前仅支持 GNU/Linux，其中 Firefox 组合测试最充分。



## 依赖

- GNU/Linux；
- Emacs 31.1，并带 SQLite、libxml 与 GnuTLS 支持;
- [`browser-cookies.el`](https://github.com/DzmingLi/browser-cookies.el) 0.1.0 或更新版本，用于读取浏览器登录态；
- [`plz`](https://github.com/alphapapa/plz.el) 0.10-pre 或更新版本，用于所有 HTTP 请求；
- `curl` 可执行程序，作为 `plz` 的底层 HTTP 传输；
- Emacs 内置的 Org 与 `ox-html`，用于编辑和转换源稿；
- 已在受支持的浏览器 profile 中登录豆瓣。


## Cookie 配置

浏览器和 profile 都由用户显式指定。底层读取由 `browser-cookies.el` 提供，
不会扫描、猜测或自动选择 profile：

`douban-cookie-browser` 可设为：

- `firefox`
- `chromium`
- `chrome`

`douban-cookie-profile-directory` 必须指向所选 profile：

- Firefox：包含 `cookies.sqlite`；
- Chromium、Chrome：包含 `Network/Cookies`。

Chromium 系浏览器的 Cookie 可能需要从 Linux 桌面的 Secret Service 或 KWallet 取得解密密钥。

Firefox Container 还应显式设置对应的 `originAttributes`，普通非 Container profile 使用空字符串。


## 使用

### 新建长评源稿

`M-x douban-new-review` 先选择品类，再通过名称搜索或标准条目 URL 选择评论
对象，最后创建 `.org` 源稿。文件选择器默认从
`douban-review-directory` 开始；该目录和源稿的父目录不存在时都会递归
创建，目标文件已存在则直接报错。创建成功后，源稿会在一个新 tab 中打开并
启用 `douban-mode`。本命令只生成本地模板，不打开网页编辑器。

### 编辑 metadata

读书笔记不提供专门的新建命令。在 Org 文件中直接写出必填的
`#+DOUBAN_ANNOTATION_SUBJECT_ID:`，再通过当前补全前端补全
`#+DOUBAN_ANNOTATION_PRIVACY:` 等字段。
新日记和新广播分别写出空的 `#+DOUBAN_NOTE_ID:` 或
`#+DOUBAN_STATUS_ID:`；空 ID 表示尚未发布，成功后会自动写回。然后执行
`M-x douban-publish`。

读书笔记只支持当前“新笔记”协议，不导入已有笔记，也不兼容旧的
`/annotation/ID/` 写接口。

Org 直接从完整关键字前缀推断内容类型，例如
`#+DOUBAN_REVIEW_SUBJECT_ID:` 和 `#+DOUBAN_NOTE_PRIVACY:`，不要求额外的
`#+DOUBAN_REVIEW:` 容器行，也不读取这种空容器关键字。每份源稿至少需要一个
属于自身类型的实际字段；混用不同类型的字段会直接报错。

`douban-mode` 不会扫描或自动识别普通 Org 文件。
`douban-new-review` 创建的长评源稿会显式启用它；手工创建或重新打开其它
豆瓣源稿时，按需运行：

```text
M-x douban-mode
```


下列字段表省略 Org 关键字中相应的 `DOUBAN_REVIEW_`、`DOUBAN_NOTE_`、
`DOUBAN_ANNOTATION_` 或 `DOUBAN_STATUS_` 前缀。

`review` 字段：

| 字段 | 含义 |
| --- | --- |
| `id` | 初次发布时可省略或留空；创建成功后写回，非空时更新这篇长评 |
| `subject-id` | 必填，豆瓣条目 ID；新长评可在值槽按名称动态补全 |
| `subject-type` | 必填，必须是 `book`、`movie`、`tv`、`music` 或 `game` |
| `introduction` | 可选导语，最多 140 个 UTF-16 code unit |
| `rating` | 可选，整数 1–5 |
| `spoiler` / `donate` | 可选布尔值 |
| `explanation-types` | 可选的单项内容说明，值与普通广播相同 |
| `rtype` / `platforms` | 仅用于游戏评论；`platforms` 保存一个或多个平台 ID |

游戏长评的 `rtype` 只接受 `review`（评测）或 `guide`（攻略）。必须在 metadata 中明确填写 `rtype`

游戏评论已经具有合法 `subject-id` 时，`platforms` 值槽会通过该条目的匿名
详情接口取得当前可用平台。候选显示平台名称、缩写和 ID，完成补全后只保存
平台 ID。多个平台在同一个 Org 关键字值中使用逗号分隔：

```org
#+DOUBAN_REVIEW_SUBJECT_ID: 36932396
#+DOUBAN_REVIEW_SUBJECT_TYPE: game
#+DOUBAN_REVIEW_PLATFORMS: 1,2
```

`note` 字段：

| 字段 | 含义 |
| --- | --- |
| `id` | 初次发布时留空；新建页预分配后会在上传或发布前写回 |
| `privacy` | 可选：`public`（所有人可见）或 `friends`（仅朋友可见） |
| `cannot-reply` | 是否禁止回复 |
| `author-tags` | 标签列表 |

没有 `id` 时读取创建页并绑定其预分配 ID；有 `id` 时读取对应编辑页，
由页面的 `action` 判断状态：`new` 表示恢复同一次首次发布，其他受支持的
提交模式表示更新已发布日记。源稿不另存 URL。

`annotation` 字段：

| 字段 | 含义 |
| --- | --- |
| `id` | 初次发布时可省略或留空；创建成功后写回 topic ID，非空时更新这篇笔记 |
| `subject-id` | 必填，笔记所属的豆瓣图书 ID；新稿可按书名动态补全 |
| `privacy` | 可选：`public`（公开）或 `private`（仅自己可见） |
| `explanation-types` | 可选的单项内容说明，值与普通广播相同 |

读书笔记标题最多 70 个 UTF-16 code unit。新建默认公开，其回复范围由
`douban-default-reply-limit` 决定；私密笔记始终禁止回复。更新时保留编辑页
中的现有回复范围，只有从私密切回公开时重新使用该全局配置。省略其他可选
字段同样会保留编辑页中的现有设置。源稿只保存 topic ID，规范公开地址为
`https://www.douban.com/topic/ID/`。

`status` 字段：

| 字段 | 含义 |
| --- | --- |
| `id` | 初次发布时留空；发布后写入 personal topic ID，更新 API 使用这个 ID |
| `explanation-types` | 可选的单项内容说明，见下表 |
| `anthology-id` | 可选文集 ID；可在字段值处按文集名称补全 |

内容说明严格单选：

| metadata 值 | 网页选项 |
| --- | --- |
| `ai-generated` | 含 AI 生成内容 |
| `fictional` | 含虚构内容 |
| `marketing` | 含营销信息 |
| `minor-safety` | 含或影响未成年人身心健康信息 |
| `public-affairs` | 涉及时事、公共政策、社会事件 |
| `personal-opinion` | 个人观点仅供参考 |
| `repost` | 内容为转载，来源见正文 |
| `none` | 无需标注 |


新建广播固定公开，回复范围由 `douban-default-reply-limit` 决定。更新时
保留编辑页中的现有回复范围，不使用新建默认值。

例如：

```org
#+DOUBAN_STATUS_EXPLANATION_TYPES: ai-generated
```

长评、读书笔记和普通广播都不接受 `original` metadata。
`douban-default-original` 是唯一的原创声明开关，默认为 `t`：新建这三类
内容时使用它，更新长评时也由它控制；更新读书笔记和普通广播时
则保留编辑页中的现有原创状态。

读书笔记和普通广播的回复范围也不属于源稿 metadata。统一配置项
`douban-default-reply-limit` 可选 `all` 或 `following`，默认为 `all`；它用于
新建公开读书笔记和广播，也用于把已有读书笔记从私密切回公开。

普通广播还接受可选的 `anthology-id`，用于把本次内容加入已有文集。metadata 中只填写正整数 ID：

```org
#+DOUBAN_STATUS_ANTHOLOGY_ID: 123456
```

所有元数据都带有自动补全。


### 新建文集

需要新建文集时运行：

```text
M-x douban-new-anthology
```

命令会提示名称和封面，并立即在远端创建公开文集。名称不能为空且最多 20
个 UTF-16 code unit；当前网页编辑器要求封面，并把交互裁剪结果上传为
800×800 JPEG。本命令会验证文件具有 JPEG 标识，但不代替网页端裁剪，请预先
准备 800×800 方图。



### 图片


图片独占段落时才生成豆瓣 `IMAGE`。夹在文字、标题或表格单元格中的行内
图片改用其 `alt` 文字；空白或缺失 `alt` 时直接消失，也不会上传。链接
包裹的独立图片仍按独立图片处理。

### 读书笔记中的引用

读书笔记使用 Org quote block；发布后是普通 `blockquote`。
本包不生成豆瓣原生摘录块，也不解析章节、
页码元数据；如需记录出处，请把章节和页码直接写入可见正文。

### 文章内链接

长评、日记、读书笔记和普通广播都支持指向正文标题的文章内链接。Org 使用
`CUSTOM_ID`：

```org
[[#conclusion][跳到结论]]

* 结论
:PROPERTIES:
:CUSTOM_ID: conclusion
:END:
```

发布时会把源稿 ID 改写为豆瓣公开页实际使用的标题文字锚点；带 fragment
的完整外部 URL 不会被改写。作为跳转目标的标题必须是正文顶层的纯文字标题，
而且可见文字必须唯一。标题中的粗体、斜体、代码、链接或图片都会使远端锚点
不稳定，因此这类标题不能作为跳转目标；没有被引用的普通
标题不受此限制。引用、列表、脚注和参考文献中的标题不属于正文导航目标。

### @ 豆瓣用户

在 Org 源稿中运行 `M-x douban-insert-user-mention`，搜索并选择已关注用户。
命令会插入一个持久化 HTML export snippet，发布时转换为豆瓣 Draft.js 的原生
`USER` entity。普通用户主页链接始终只是普通超链接，不会自动升级为 mention。

### 豆瓣条目链接


不记得条目 URL 时，可以运行：

```text
M-x douban-search-subject
```

选择品类并输入名称（也可以直接输入规范 URL）后，命令会让你选择搜索结果，
并把规范条目 URL 直接插入光标处。随后可以把 URL 用在普通 Org
链接或下面的 `h-cite` 卡片中。这里的规范 URL 指 HTTPS 的
`book/movie/music.douban.com/subject/ID/` 或
`www.douban.com/game/ID/`，不带 query、fragment 或额外路径。

### 链接卡片

链接卡片直接使用 Microformats2 的 `h-cite` HTML，并作为文档顶层的
独立内容。`u-url` 与 `p-name` 必须各出现一次：

```html
<div class="h-cite">
  <a class="u-url p-name" href="https://example.com/articles/1">示例文章</a>
</div>
```

`u-url` 必须是带主机名的绝对 HTTP 或 HTTPS 链接，`p-name` 的可见文字
用作卡片标题。Blog 可以直接为 `.h-cite` 添加样式，并继续使用
`p-author`、`p-publication`、`dt-published` 等通用字段；douban.el 只提取
发布原生卡片所需的 URL 和标题。Org 源稿把同样的 HTML 放在
`#+begin_export html` / `#+end_export` 中。


发布前，程序通过豆瓣当前的 URL 解析接口取得卡片数据；服务端按 URL 返回原生
`LINK` 或 `SUBJECT` 原子卡片。



### 发布

在源稿 buffer 中运行：

```text
M-x douban-publish
```

命令会保存当前 buffer，按 Org 唯一的 `DOUBAN_*` 类型标记确定稿件类型，
并直接执行对应发布流程。
`douban-publish` 不依赖 `douban-mode`；即使没有自动识别、手动关闭了 mode，
只要源稿 metadata 合法，仍可照常发布。

如需在长评和日记末尾自动追加 Creative Commons 许可引用，可设置
`douban-cc-statement`。它支持 CC0 1.0 与六种 CC 4.0 许可，默认不追加；
声明只进入发布正文，不修改源稿，也不作用于读书笔记或普通广播。

Org 扩展高亮依赖 `org-extra-emphasis`。它随 `douban.el` 一同作为必要依赖
安装和加载；默认的 `!!文字!!` 标记会发布为豆瓣行内或块高亮。

长评和读书笔记默认不发送或保留关联广播。设置
`douban-review-send-broadcast` 为非 nil 可以恢复广播：读书笔记直接通过发布
请求控制；长评网页接口会无条件生成广播，因此默认关闭时，程序先把评论 ID
写回源稿，再从首页唯一核对并删除对应广播。长评广播可能短暂可见；若清理
失败，错误会明确说明评论已经发布，源稿中的 ID 也会保留，切勿重复发布。
更新已有长评不会删除历史广播。

## 配置项

| 变量 | 默认值 | 作用 |
| --- | --- | --- |
| `douban-cookie-browser` | `firefox` | Cookie 来源浏览器 |
| `douban-cookie-profile-directory` | `nil` | 必须显式设置的 profile 目录 |
| `douban-firefox-origin-attributes` | `""` | Firefox 普通上下文或 Container 值 |
| `douban-review-directory` | XDG 文档目录下的 `douban/reviews/` | `douban-new-review` 默认创建源稿的目录 |
| `douban-default-reply-limit` | `all` | 新建公开读书笔记和广播的回复范围；也用于读书笔记从私密切回公开 |
| `douban-default-original` | `t` | 长评、读书笔记和普通广播的全局原创声明开关 |
| `douban-review-send-broadcast` | `nil` | 长评和读书笔记是否发送并保留关联广播 |
| `douban-cc-statement` | `nil` | 长评和日记末尾的可选 CC 许可声明 |
