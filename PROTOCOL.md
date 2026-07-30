# 豆瓣内容网页协议笔记

本文记录 `douban.el` 截至 **2026-08-08** 所实现的豆瓣长评、日记、读书笔记和普通广播协议。它不是豆瓣官方文档；这些网页接口没有稳定性保证。

## 实现结论

当前实现使用“浏览器 Cookie + 豆瓣网页写接口”路线：

1. 从用户明确指定的浏览器 profile 读取适用于请求 URL 的 Cookie；
2. 不含图片的长评、读书笔记和普通广播先使用浏览器 Cookie 中的 `ck`；浏览器数据库没有持久化这个 session Cookie 时，只读一次 `https://www.douban.com/mine/`，从响应的 `Set-Cookie` 补入当前发布会话；
3. 正文含图片时，长评、日记、读书笔记和普通广播按各自协议 GET 真实编辑页，取得并核对页面绑定的 ID、`ck` 或上传凭据；
4. 严格核对页面字段、目标 ID、HTTPS 主机和响应 URL；
5. 只向核对过的豆瓣 HTTPS 地址提交写请求。

能力边界：

| 内容类型 | 表示 | 正文协议 | 远端操作 |
| --- | --- | --- | --- |
| `review` | 长评 | Draft.js raw，支持图片、用户 mention 与行内条目引用 | 创建、更新 |
| `note` | 日记 | Draft.js raw，支持图片、用户 mention 与行内条目引用 | 创建、恢复首次发布、更新 |
| `annotation` | 当前新式读书笔记 | topic Draft.js raw | 创建、更新 |
| `status` | 普通广播 | personal/topic Draft.js raw，支持图片、用户 mention 与行内条目引用 | 创建、更新 |

源稿仅支持 Markdown 和 Org。Markdown 只接受规范的顶层 `douban:`
mapping，且其中必须且只能包含 `review`、`note`、`annotation` 或 `status` 中的一个子
mapping；唯一子 mapping 的名称确定稿件类型，不保存 `kind`，也不从其他
字段推断。`id`、`topic-id`、`privacy` 等是类型子 mapping 内的
叶子字段。例如：

```yaml
douban:
  review:
    subject-id: '4908885'
    subject-type: book
```

新日记与新广播分别使用 `note: {}` 和 `status: {}`。新读书笔记使用带必填
`subject-id` 的 `annotation` mapping。初次发布省略 `id`，
新广播也省略 `topic-id`；ID 字段一旦出现便必须是非空正整数。长评的
`review.subject-id` 与 `review.subject-type` 必填；每类内容只接受自己的
叶子字段，未知类型、多个类型、未知字段和跨类型字段即使为空也直接报错。
图片处理结果不属于 metadata，也不会写回源稿。

Org 用值为空的 `#+DOUBAN_REVIEW:`、`#+DOUBAN_NOTE:`、
`#+DOUBAN_ANNOTATION:` 或 `#+DOUBAN_STATUS:` 作为唯一内容类型标记，
叶子字段则使用完整路径，例如
`#+DOUBAN_REVIEW_SUBJECT_ID:`、`#+DOUBAN_NOTE_PRIVACY:` 和
`#+DOUBAN_STATUS_TOPIC_ID:`。标记本身的值必须为空；缺少标记、出现多个
标记、使用不带类型路径的字段或把别的类型字段放在当前标记下都会报错。

读书笔记接受可选的 `annotation.privacy` 与 `annotation.explanation-types`。
普通广播还接受可选的 `status.explanation-types` 与 `status.anthology-id`。
在远端标识方面，
长评、日记和读书笔记各自只保存 `id`；普通广播同时保存公开 wrapper 的 `status.id`
和 personal topic 的 `status.topic-id`。URL 不属于源稿 metadata，只用于
响应校验和完成消息；规范 URL 由已核对的远端结果与 ID 得到，不写回源稿。
标题只取源稿中用户手写的顶层 `title` / `#+TITLE`；不会从条目名称或
文件名推导。普通广播没有标题。实现不接受 Typst。
长评、读书笔记和普通广播都不接受 `original` 叶子字段或对应的 Org
关键字；原创声明由全局 `douban-default-original` 与更新语义决定。
读书笔记和普通广播也不接受回复范围叶子字段或对应的 Org 关键字；新建时
由全局 `douban-default-reply-limit` 决定，更新时按下文规则保留远端状态。

源稿中的枚举使用可读值，网页协议码只在请求边界生成：
`review.rtype` 的 `review` / `guide` 映射为 `R` / `G`，`note.privacy` 的
`public` / `friends` 映射为 `P` / `F`；内容说明的语义值映射见广播发布
设置一节。统一 metadata CAPF 先补全内容类型，再补全该类型允许且尚未出现的
叶子字段或完整 Org 关键字，也补全这些枚举值。新长评的 `subject-id` 还可在
同一 `review` 中已有明确 `subject-type` 时按非空名称动态搜索；游戏评论的
`platforms` 可根据已有条目 ID 动态读取。
候选 annotation 只用于存在歧义、使用缩写、表达豆瓣特有语义或需要呈现正式
声明文本的值；其余自解释候选不加旁注。

这些编辑期能力由 buffer-local `douban-mode` 统一安装。进入 Markdown mode
时，YAML front matter 中存在顶层 `douban:` 便自动启用；进入 Org mode 时，
存在文档级内容类型标记便自动启用。检测只识别结构标记，不执行完整 metadata
校验，因此字段尚不完整的源稿仍可获得补全；尚未写出标记的受支持文件可手动
启用。关闭 mode 会移除本地 CAPF 与 revert hook，并清空用户、文集、条目和
平台候选缓存。发布命令独立验证源稿，不以 mode 是否启用为条件。

`review.explanation-types` 与 `status.explanation-types` 的源稿值都是严格
单选的标量枚举。Markdown 不接受 YAML sequence，Markdown 与 Org 都不接受
逗号拼接的多值；字段名中的复数不表示列表。

Markdown front matter 的每一层 mapping 都要求 key 唯一；重复 key 会在
yaml.el 覆盖旧值之前直接报错。为保证 metadata checkpoint 重写
`douban:` mapping 后不会留下悬空引用，front matter 也不接受 YAML anchor
或 alias。

## 协议证据

### 证据层级

长评协议的主要证据是实际“写评论”与“编辑评论”页面及其当时加载的编辑器 bundle：

- `https://www.douban.com/subject/{subject_id}/new_review`
- `https://{host}/review/{review_id}/edit`
- [`review_editor.js`](https://img1.doubanio.com/f/zerkalo/e4d3ddcd211290a54b72657cc8789f8b012e78cd/js/review/editor/ng/review_editor.js)（文件哈希会随豆瓣部署变化）

页面和 bundle 提供或交叉确认了 `ck`、`_APP_NAME`、`_REVIEW_ID`、`review[...]`、创建/更新端点和图片上传凭据。实现只依赖其中实际使用的创建、更新和上传契约。

日记发布与更新上下文以当前网页 HTML 表单为首要证据：

- 创建页或 `/note/{note_id}/edit` 页中的 `note_id`、`ck`、隐私选项、提交模式和上传凭据；

普通广播协议以 2026-07-30 豆瓣首页实际加载的个人话题编辑器为主要证据：

- [`personal-topic-editor.42ee6.js`](https://img9.doubanio.com/cuphead/sns-static/personal-topic-editor.42ee6.js)
- `https://www.douban.com/` 中的 `personal-topic-editor` 初始化状态和发布后 `status-item` DOM

该 bundle 明确使用 `https://m.douban.com/rexxar/api/v2/topic/post`、带凭据的 Axios、`X-CSRF-TOKEN` 和 JSON 请求体。正文是 Draft.js raw 再序列化所得的 JSON 字符串；personal 创建分支显式提交 `group_id: "0"`。首页 DOM 则明确区分 personal topic 的 `data-aid` 与广播状态的 `data-sid`。

已有普通广播的 `/topic/{aid}/edit` 页面及其当前 topic editor bundle 进一步确认：更新使用 `POST https://m.douban.com/rexxar/api/v2/group/topic/{aid}/post`，aid 只进入 URL 路径；最终 payload 不带 `id`、`title` 或 `group_id`，但会原样重送 `video_info`。页面的 `__INIT_STATE__.topic` 标明 `is_personal_topic: true`、`subtype: "personal"`，并提供需要沿用的回复权限、可见范围、兴趣标签、原创与视频标记、文集、图片 `seq_id` 和布局。兴趣标签对象按名称以 `#` 连接；personal 更新的 `topic_tag_ids` 为空字符串。

当前实现的成功判据是：长评与日记需要可解析且字段一致的 JSON；普通广播的创建端点返回 2xx 后，优先以响应中的个人话题 ID 对应首页 `data-aid`，响应没有可用 ID 时才按非原子正文唯一匹配，最后以 `data-sid` 构造规范广播 URL；普通广播更新与网页编辑器一致，只把 2xx 且 JavaScript truthy 的响应数据视为成功，并保持已有 sid 和 aid。

当前新式读书笔记协议以创建页、编辑页和 2026-08-03 加载的 topic editor
bundle 为主要证据：

- `https://www.douban.com/topic/create?subject_id={book_id}&subtype=annotation`
- `https://www.douban.com/topic/{topic_id}/edit`
- [`topic-editor/58.c.8d966.a.js`](https://img3.doubanio.com/cuphead/group-static/topic-editor/58.c.8d966.a.js)，核验时 SHA-256 为 `11e7939e56d8cd3b4bfbe879d8bf63f18d532e0327484571ce9156cb790f33e6`

bundle 明确区分 `subtype:"annotation"`，创建使用通用 topic post，更新把
`id` 代入 group topic URL 后从实际 JSON body 删除；它还给出内容说明、
隐私和图片的当前转换规则。本实现只支持这条新入口，
不实现旧 `/annotation/{id}/` 写协议，也不导入已有旧笔记。

社区实现只作为网页契约的交叉证据：

- [`doufen-org/tofu`](https://github.com/doufen-org/tofu/blob/master/extension/tasks/migrate/review.js) 使用 `/j/review/create`、`is_rich=1`、`review[...]`、`ck` 和序列化的 Draft.js raw；
- [`Wechatsync` 的固定历史 commit](https://github.com/wechatsync/Wechatsync/commit/8c9db09b19bce11ea081a2b2b6d78e54606c4a05) 交叉验证 `/j/review/create`、`/j/review/upload_image`、`picfile`、`review_id`、`ck` 与上传 token；
- [`jackjin1997/douban-mcp`](https://github.com/jackjin1997/douban-mcp) 展示网页 Cookie、`ck`、Referer 和 Origin 的组合。

Frodo App 的 access token、设备参数、HMAC 签名与内置 App 密钥属于另一条内部路线，当前实现没有采用。旧官方 OAuth v2 客户端也不能证明这些网页写接口。

“当前页面可观察到”不等于“官方承诺稳定”。因此代码以页面为准并 fail closed，而不是把社区代码当成权威规范。

## 通用会话与传输约束

### Cookie 来源

本包只支持 GNU/Linux。`douban-cookie-browser` 必须显式选择 `firefox`、`chromium` 或 `chrome`；`douban-cookie-profile-directory` 必须显式指向一个固定 profile。本包不会自动发现或猜测 profile。

各浏览器存储位置：

| 浏览器 | profile 内文件 |
| --- | --- |
| Firefox | `cookies.sqlite` |
| Chromium / Chrome | `Network/Cookies` |

读取后会按请求 URL 的 host、domain、path、secure 和 expiry 筛选 Cookie。Firefox 还按显式的 `douban-firefox-origin-attributes` 选择普通上下文或 Container。Chromium 系 Cookie 从 Linux 桌面的 Secret Service 或 KWallet 取得密钥后解密。

Cookie 数据库首先通过只读 SQLite URI 查询；若浏览器持锁导致 SQLite 错误，程序会把主数据库和现存的 `-wal` 复制到独立临时目录后查询快照。可重建的 `-shm` 不进入快照。两条路径都会回滚失败的读事务并关闭连接，临时快照在查询结束后删除。

浏览器 SQLite 中某些登录或安全 Cookie 的值会带一对语法性外层双引号。构造 HTTP `Cookie` 头时，程序只去掉这对首尾双引号，不修改中间内容，也不改写浏览器数据库。如果原样交给 `plz`，双引号会进入 curl config 的 header 语法并破坏整条 Cookie，可能使仍然有效的登录态在 `/mine/` 上表现成跳转登录页。

### 请求约束

所有携带豆瓣登录态的请求都通过 `plz` 同步调用 curl，而且目标必须是 HTTPS 豆瓣 URL。请求带适用于目标的 Cookie、对应网页的 Referer、网页 Origin，以及协议需要时的 `X-Requested-With: XMLHttpRequest` 或 `X-CSRF-TOKEN`。普通广播是从 `www.douban.com` 页面向 `m.douban.com` 发出的带凭据跨源请求，因此 Origin 仍是 `https://www.douban.com`。

统一的 plz/curl 传输会移除 curl 的重定向跟随选项，不会把 Cookie 转发给重定向目标。默认的 3xx 响应会被调用层拒绝；同时保留原始响应状态与响应头，供明确允许检查 3xx 的只读流程使用。条目搜索和公开远程图片下载也复用 `plz`，但不携带豆瓣登录态。

从发布页取得的 `ck` 会同时进入表单字段和本次会话 Cookie。不含图片的长评和普通广播建立直接会话时，先检查目标 URL 的浏览器 Cookie；若没有非空且不等于 `deleted` 的 `ck`，便使用适用于 `https://www.douban.com/mine/` 的浏览器 Cookie，通过同一个 plz/curl HTTP 层发起一次只读 GET。该 bootstrap 明确允许返回未跟随的 302，并从原始 `Set-Cookie` 取得 session `ck`，再把它合并到目标发布会话；它不会请求 `Location`。响应仍没有有效 `ck` 时，在任何 POST 前终止。登录页、必要字段缺失或页面 ID 不匹配同样会终止对应操作。

## 长评协议

### 新稿条目搜索与 subject-id 补全

`douban-new-review` 交互时先要求从 `book`、`movie`、`tv`、`music` 和 `game` 中选择品类，再输入条目名称或规范 URL。Lisp 调用也必须把品类作为独立参数显式传入，不从 URL、名称或数字 ID 猜测。

名称输入只触发一次针对所选品类的匿名请求：

```http
GET https://m.douban.com/rexxar/api/v2/search/subjects?q={query}&type={type}&start=0&count=20&sort=relevance
Referer: https://www.douban.com/search
```

请求类型映射为 `book → book`、`movie → movie`、`tv → movie`、`music → music`、`game → ilmen`。电影和剧集共用 `movie` 搜索入口，但返回结果必须继续按所选 `target_type` 过滤；其他品类同样只接受与选择一致的结果。请求不携带浏览器 Cookie。重复结果只在当前品类内按 ID 去重，不把不同内容类别的数字 ID 当作同一个全局命名空间。候选中的远端名称和副标题只用于显示；选定后仅把准确的 `review.subject-id` 与先选定的 `review.subject-type` 写入新稿，标题仍为空。

规范 URL 输入不发搜索请求，但仍须服从先选定的品类。图书、音乐和游戏 URL 的主机必须与品类一致；电影和剧集共用 `movie.douban.com` 条目 URL，主机本身不能区分二者，因此以显式选择的 `movie` 或 `tv` 为准。

统一 metadata CAPF 复用同一匿名搜索。在 Markdown 的
`review.subject-id` 值槽或 Org 的 `#+DOUBAN_REVIEW_SUBJECT_ID:` 中输入
非空名称时，只有同一个 `review` 已提供合法 `subject-type`，并且尚未存在
`review.id`，才按该品类请求候选。候选显示名称、副标题、品类和 ID；完成时
临时显示文字会被替换为规范 ID。值槽已经是正整数 ID 时不请求网络，也不把它
解释成名称。这一点与 `douban-new-review` 的名称输入框不同：后者把裸数字
保留为可能的《1917》《2046》等名称并执行搜索。

### 游戏平台补全

游戏评论的 `review.platforms` 保存平台 ID。当前 `review.subject-type` 为
`game` 且 `review.subject-id` 是正整数时，metadata CAPF 使用匿名请求读取
该条目的当前平台：

```http
GET https://m.douban.com/rexxar/api/v2/game/{subject_id}
```

候选同时显示平台名称、缩写和 ID，完成时只把平台 ID 写入源稿。Markdown
多平台值使用 YAML block sequence，每个列表项独立保存一个 ID：

```yaml
platforms:
  - '1'
  - '2'
```

YAML flow sequence（例如 `platforms: ['1', '2']`）仍可作为 metadata
读取，但不提供 completion-at-point。Org 使用一个逗号分隔列表：

```org
#+DOUBAN_REVIEW_PLATFORMS: 1,2
```

### 建立会话

长评 metadata 必须明确包含 `review.subject-type`，且只能是 `book`、`movie`、`tv`、`music` 或 `game`。不含图片的长评采用直接会话：不请求评论编辑页，只读取适用于同源条目页的浏览器 Cookie；其中没有持久化 `ck` 时，按通用规则从 `/mine/` 响应补入当前会话。创建和更新端点的主机都由 `review.subject-type` 固定映射；更新路径中的 review ID 取自 `review.id`。`tv` 在提交字段中按网页协议归入 `movie`。

正文含 `IMAGE` entity 时才读取发布页上下文。创建：

```http
GET https://www.douban.com/subject/{subject_id}/new_review
```

更新：

```http
GET https://{book|movie|music|www}.douban.com/review/{review_id}/edit
```

这条页面会话路径要求页面同时提供非空 `ck`、`review[subject_id]` 和 `_APP_NAME`。更新页还必须返回与源稿一致的 review ID；条目 ID 也必须逐次一致。否则不发送写请求。图片上传所需的动态凭据也只能从这条页面上下文取得。

### 创建与更新

```http
POST https://{editor-host}/j/review/create
POST https://{editor-host}/j/review/{review_id}/update
Content-Type: application/x-www-form-urlencoded; charset=UTF-8
```

核心字段：

| 字段 | 值 |
| --- | --- |
| `is_rich` | `1` |
| `review[subject_id]` | `review.subject-id`；使用页面会话时还会经页面复核 |
| `review[title]` | 用户在源稿中手写的标题，最多 200 个 UTF-16 code unit |
| `review[introduction]` | 空字符串或最多 140 个 UTF-16 code unit |
| `review[text]` | 序列化的 Draft.js raw |
| `review[rating]` | 空字符串或 `1`–`5` |
| `review[spoiler]` | `on` 或空字符串 |
| `review[donate]` | `on` 或空字符串 |
| `review[original]` | `douban-default-original` 为非 nil 时发送 `on`，否则发送空字符串 |
| `review[explanation_types]` | metadata 内容说明映射出的代码或空字符串 |
| `ck` | 直接会话取目标 Cookie，缺失时由 `/mine/` 响应补入；页面会话取本次发布页的值 |

`review[original]` 不来自源稿 metadata。`douban-default-original` 默认为 `t`，
创建和更新长评时都由该配置决定提交值。

游戏长评还可提交 `review[rtype]`（源稿 `review.rtype` 的 `review` /
`guide` 分别映射为 `R` / `G`）和一个或多个 `review[platforms]`。直接
会话没有页面默认值，因此不含图片的游戏长评必须在 metadata 中明确设置
`review.rtype`；页面会话可使用页面选中值。非游戏长评出现
`review.rtype` 或 `review.platforms` 时拒绝提交。每个
`review[platforms]` 的值都是源稿中持久化的平台 ID；平台名称和缩写只用于
补全候选显示。

创建与更新都只把响应中的规范评论 URL 视为成功依据；`review_id`、`id`
或 `result` 等未被当前编辑器读取的字段不能替代它。更新时 URL 中的 ID
还必须等于源稿 `review.id`，否则不确认成功。创建成功时从已核对的 URL
提取并写回 `review.id`；URL 只显示在完成消息中，不进入 metadata。更新
成功后保留原 `review.id`，不写 checkpoint。

长评网页表单没有可验证的广播开关，创建时服务端会附带生成一条 review
activity。`douban-review-send-broadcast` 默认为 nil：创建结果确认后，程序先
把 `review.id` 原子写回源稿，再用独立的 `www.douban.com` 会话从首页查找
同时满足 `data-action="7"`、`data-object-kind="1012"` 和
`data-object-id={review_id}` 的唯一广播，最后向 `/j/status/delete` 提交该
广播的 sid 与 ck。首页查找最多重试三次；删除 POST 绝不重试。没有唯一匹配
或无法确认删除成功时，评论本身和本地 checkpoint 均保持不变，程序要求用户
到主页人工检查，不能重新创建评论。这个补偿流程无法保证广播从未短暂可见。
选项为非 nil 时跳过清理；更新已有评论无论选项为何值都不删除历史广播。

公开页正文之后的固定“来自豆瓣App”文案是服务端记录的移动客户端来源，
不在 `review[...]` 表单字段或正文 raw 中。网页发布路径没有受支持的
`source`、`app`、`device` 或自由来源文案字段，因此实现不把该标签暴露为
metadata，也不尝试伪装移动客户端。

## Draft.js 富文本

长评、日记、读书笔记和普通广播共享 Draft.js raw 结构。四者都先把 Markdown 或 Org 转成 HTML，再转成 raw。最小结构：

```json
{
  "blocks": [
    {
      "key": "abc12",
      "text": "正文",
      "type": "unstyled",
      "depth": 0,
      "inlineStyleRanges": [],
      "entityRanges": [],
      "data": {}
    }
  ],
  "entityMap": {}
}
```

实现使用的块包括 `unstyled`、`header-two`、`header-three`、`header-four`、`blockquote`、`code-block`、`highlight-block`、两种 list item 和 `atomic`。行内高亮使用 `MARK` style range，删除线使用 `STRIKETHROUGH`。普通链接先生成可变的 `LINK` entity；其中规范豆瓣条目 URL 会在解析后升级为不可变的 inline `SUBJECT`。图片使用 `IMAGE` entity，分隔线使用 `SEPARATOR` entity，链接卡片使用不可变的 atomic `LINK` 或 `SUBJECT` entity。

Draft.js 的 offset 与 length 是 JavaScript UTF-16 code unit，不是 Emacs 字符数；非 BMP 字符按两个 code unit 计算。

独占段落的图片转换为 `atomic` 图片块。非独占的行内图片不生成 IMAGE
range，而以 `alt` 文字参与当前 block；空白或缺失 `alt` 时不产生文字。
因此行内图片不会进入上传流程。

Markdown 与 Org task list 的 checkbox 在 Pandoc HTML 中都是 `input`
节点。转换器把 checked/unchecked 分别写为 `☑ ` / `☐ `，随后仍生成
普通的 list item block；豆瓣端不会得到可交互 checkbox。

居中采用当前编辑器的 block data，而不是旧的 `center-block` 类型：

```json
{"type":"unstyled","data":{"align":"center"}}
```

Markdown `<div style="text-align: center">...</div>` 与 Org
`#+begin_center` 都产生居中容器。Pandoc 将 Markdown HTML 转为带有
`text-align` 内联 CSS 的原生 Div，转换器再读取该属性。直接普通段落和
标题保留各自 block type，并设置 `align:"center"`；独立图片仍为普通
atomic IMAGE，不附加 align。列表、引用、代码、表格、分隔线、卡片、
块高亮和嵌套居中不在这套容器语义内，转换时明确拒绝。

当前实现没有数学公式专用 entity，也不接受 `.typ`。需要公式时应预先渲染为能通过下述 `image/*` 内容校验的图片，再作为图片放入富文本。

### 文章内链接与目录

豆瓣 Draft.js 没有专用目录 entity。普通文章内链接仍是可变 `LINK`：

```json
{
  "type": "LINK",
  "mutability": "MUTABLE",
  "data": {"url": "#可见标题文字"}
}
```

豆瓣公开页会给纯文字标题生成等于其完整可见文字的 HTML `id`；源 HTML
标题上的自定义 `id` 不会保存在 Draft raw 中。转换器因此先按源 fragment
找到带 `id` 的标题，再把链接改写成 `#可见标题文字`。它只处理目标完全由
单个 fragment 构成的链接，`https://example.org/page#fragment` 等完整
URL 保持原样。URL 中的 fragment 先按 UTF-8 percent decoding 查找，找不到
时再按原始值查找。

可作为远端锚点的标题必须位于正文顶层的透明 block 容器中、没有脚注或参考
文献 role、只含文本子节点，并且可见文字唯一。引用、列表、表格等嵌套 block
内的标题不参与导航。只有实际被 fragment 引用或被自动目录选中的标题才执行
这些约束，不会限制无导航需求的旧文章。

Markdown 顶层 `toc: true` 会在正文开头插入一个私有 HTML 标记；
`toc-depth` 接受 1–6，默认 3。Org 的 `#+TOC: headlines N` 由 Lua filter
在 Pandoc AST 中原位替换为同一标记；`N` 接受 1–3，默认 3，源码块内的
同名文字忽略，嵌套、重复或其他形式明确拒绝。Org 限制到三级是因为当前
Pandoc Org reader 只把前三层 headline 可靠地表示为 HTML heading。

HTML 转 Draft 时，标记展开为一个完整 `BOLD` 的普通“目录”块，随后每个
标题生成一个 `unordered-list-item` 和覆盖完整文字的 `LINK` entity。
列表层级按实际标题层次计算，并截断到 Draft.js 支持的最大 depth 4。
脚注、参考文献及其反向链接带有 `doc-*` role，既不进入目录，也不参与
fragment 重写。

2026-07-30 已用同一篇[测试长评](https://book.douban.com/review/17735029/)
临时实发验证：公开 API 返回的标题节点保留了等于可见文字的 `id`，自动目录
和手写链接生成的两个 fragment `LINK` 都指向该节点。验证后已恢复原始
Draft raw、全部编辑表单字段和公开正文，并逐项比对一致。

### 用户 mention

豆瓣[广播帮助](https://help.douban.com/broadcast?app=1#t0-q5)明确说明
`@个性域名` 会让对应用户收到消息提醒。当前
[`personal-topic-editor.42ee6.js`](https://img9.doubanio.com/cuphead/sns-static/personal-topic-editor.42ee6.js)
并不靠正文字符串猜测用户，而是在输入 `@` 后以当前登录态查询：

```http
GET https://m.douban.com/rexxar/api/v2/search/user_complete?q={query}&start=0&count=10&ck={ck}
Referer: https://www.douban.com/
```

候选必须提供非空 `name`、正整数 `id` 和规范
`https://www.douban.com/people/{uid}/` URL，并且响应中的 `followed` 必须
为 true；搜索端点本身可能返回全站用户，实现会在本地丢弃未关注和畸形
候选。`douban-insert-user-mention` 先让用户选择确定候选，再把三项身份
写入 Markdown 或 Org 的私有链接标记；普通用户主页链接和裸 `@名字` 都
不会自动升级为 mention。

Markdown 中的 `douban-mode` 把非空 `@query` 暴露给
completion-at-point，候选表为 exclusive，仍只含已关注用户；Org 不安装
这项 CAPF，继续使用显式插入命令。YAML front matter、邮箱或 URL 中的 `@`、
反引号 code span、fenced code 和 raw HTML 不构成补全位置。completion
前端只有在 `finished` 或 `exact` 状态才把候选固化为源标记；`sole` 只表示
还能继续 cycling，不能提前提交。回调依靠 marker 回到源 buffer，因此即使
由 minibuffer 或 Completions buffer 调用也不会误改当前 buffer。

Markdown 私有链接的可见文字逐字符写成 HTML 数字字符引用，Org 再用
`@@html:...@@` 包裹同一 anchor。这避免用户名中的强调符、反引号、HTML
entity、标签形文本或 emoji 被 Pandoc 重新解释。

转换后的 Draft entity 为：

```json
{
  "type": "USER",
  "mutability": "IMMUTABLE",
  "data": {
    "name": "用户名",
    "id": "123",
    "url": "https://www.douban.com/people/example/",
    "display": "inline"
  }
}
```

entity range 覆盖正文中的完整 `@用户名`，offset 和 length 仍使用 UTF-16
code unit。长评、日记、读书笔记和普通广播的正文都提交 Draft.js raw，因此统一保留
该实体。长评网页编辑器没有提供 mention 输入插件，但这不改变源稿直接构造
并提交 `USER` entity 的正文协议。

### 行内条目引用

Markdown 和 Org 都直接使用各自的普通文字超链接语法，不引入私有标记：

```markdown
[这本书](https://book.douban.com/subject/4908885/)
```

```org
[[https://book.douban.com/subject/4908885/][这本书]]
```

HTML 到 raw 的第一步仍把它们生成为可变的 inline `LINK`。若 URL 能被
严格解析为图书、影视、音乐或游戏的规范豆瓣条目 URL，发布前再复用
`get_url_info` 匿名接口核对条目，并把 entity 原位升级。

识别范围只包括 HTTPS 的 `book/movie/music.douban.com/subject/{id}/`
与 `www.douban.com/game/{id}/`，可省略末尾斜杠，但不接受 query、
fragment、额外路径、用户信息或非默认端口。最终 entity 为：

```json
{
  "type": "SUBJECT",
  "mutability": "IMMUTABLE",
  "data": {
    "id": "4908885",
    "title": "局外人",
    "type": "book",
    "subject_type": "book",
    "url": "https://book.douban.com/subject/4908885/",
    "display": "inline"
  }
}
```

提交 raw 的 entity range 和源稿链接文字保持不变；服务端返回的其他条目
字段也保留。豆瓣公开渲染器会根据 entity data 的规范标题显示
`《标题》`，而不是把任意源稿链接文字当作可自定义远端名称。非条目 URL
继续使用普通 `LINK`。相同 URL 的 inline 引用和 atomic 卡片共享一次
解析缓存，但分别写入 `display: "inline"` 与 `display: "atomic"`，
不会互相污染。

2026-07-30 已用同一篇[测试长评](https://book.douban.com/review/17735029/)
实发验证：长评更新端保留该实体，公开页渲染为段落内的
`<a class="subject-quote">《局外人》</a>`；源稿故意使用“这本书”作为
链接文字，公开页仍按规范条目标题显示。同一测试也验证了下节的 atomic
外链卡片。

### 链接卡片

链接卡片沿用 `zhihu.el` 的 `link-card` 源稿与中间 HTML 约定，必须由
源稿显式标记，而且必须是文档顶层的独立链接段落。Markdown 用链接
title `"card"`：

```markdown
[示例文章](https://example.com/articles/1 "card")
```

Org 用紧邻独立链接的 `ATTR_DOUBAN`：

```org
#+ATTR_DOUBAN: :type link-card
[[https://example.com/articles/1][示例文章]]
```

Pandoc 中间 HTML 与 `zhihu.el` 一样使用
`data-draft-node="block"`、`data-draft-type="link-card"`、
源稿链接标题对应的 `data-draft-title` 和空 `data-draft-cover`。进入
豆瓣 Draft.js 协议时先生成 `type: "atomic"` 的块和不可变的 `LINK`
占位 entity；未带标记的外部链接仍生成普通的可变 `LINK` entity，未带
标记的豆瓣条目链接则按上一节升级为 inline `SUBJECT`。

卡片链接必须是带主机名的绝对 HTTP 或 HTTPS URL，可以指向外部站点，
也可以是 HTTP 豆瓣条目；相对地址、scheme-relative 地址和其他协议均
拒绝。发布前匿名解析：

```http
GET https://m.douban.com/rexxar/api/v2/get_url_info?url={url}&need_card=1&editor_type=group
Referer: https://www.douban.com/
```

响应 `type` 必须是 `LINK` 或 `SUBJECT`，并由服务端根据 URL 决定。
`LINK` 必须提供非空 `title` 和带主机名的绝对 HTTP(S) `url`；可选
`cover_url` 若存在则必须是 HTTPS。`SUBJECT` 必须提供有效的 `id`、
`type`、`title` 和规范豆瓣 HTTPS `url`；可选 `cover` 若存在也必须是
HTTPS。实现保留服务端返回的其他字段，并统一补上
`display: "atomic"`。

最终 raw 仍使用 `type: "atomic"` 的块，块文字为一个空格，长度为 1
的 entity range 指向与响应同类型的 `LINK` 或 `SUBJECT` entity；
`mutability` 为 `IMMUTABLE`。三类内容共用这套转换、解析和按 URL
缓存的协议。

### 高亮与块高亮

当前豆瓣编辑器把普通高亮建模为 Draft 行内样式 `MARK`，可以覆盖普通 block 中任意一段文字，并与 `BOLD`、`ITALIC` 等 range 重叠。Markdown 使用 Pandoc `markdown+mark` 的双等号语法：

```markdown
普通段落里的 ==高亮文字==。
```

`==...==` 不属于 CommonMark 或 GFM；Pandoc 3.0 起在自己的 `markdown`
reader 中提供非默认 `mark` 扩展。手写的 Markdown 原生 HTML
`<mark>...</mark>` 始终只生成行内 `MARK`，不会触发块高亮升级。

Org 没有官方高亮定界符。本包沿用 `org-extra-emphasis` 的黄色高亮惯例：

```org
普通段落里的 !!高亮文字!!。
```

只作用于 Org 输入的 filter 把 `!!...!!` 转成 Pandoc mark span。之后的
Pandoc filter 只检查文档顶层 block，Markdown 和 Org 使用同一规则：

- 普通 block 中只包住部分文字时，生成 UTF-16 offset/length 的 `MARK`
  range。
- 文档顶层的普通段落完全由一个非空 mark span 构成时，生成
  `type: "highlight-block"`、`data: {"align": ""}`；外层 `mark`
  只决定 block type，不再生成覆盖整段的冗余 `MARK` range。

块高亮中的其他行内样式和普通链接沿用既有转换，但不接受图片或链接卡片。
多个块高亮需要逐段包裹。标题、列表项、引用和居中段落即使完整包裹，也保留
原 block type 并生成行内 `MARK`。不再识别 `::: douban-highlight` 或
`#+begin_douban-highlight` 容器。

三类内容的正文图片都在每次发布时重新解析，并按每个图片出现位置独立执行各自协议；即使多个位置引用同一图片源，也不会复用先前的处理结果。图片处理结果不会写入源稿 metadata。

### 长评图片

```http
POST https://{editor-host}/j/review/upload_image
Content-Type: multipart/form-data
```

公共字段为 `ck`、`review_id`（创建时为空）和页面给出的动态上传 token。当前常见 token 字段名是 `upload_auth_token`。

- 本地图片：直接以 `picfile=<binary>` 上传；
- 远程 HTTPS 图片：程序先用不带豆瓣凭据的 GET 下载并验证，再以
  `picfile=<binary>` 上传，不让豆瓣服务器代抓外链。

响应必须包含有效 HTTPS 图片 URL。客户端不设格式白名单：能从字节识别类型时使用实际 MIME，否则接受语法合法的 `image/*` MIME，最终由豆瓣接口决定是否支持。

### 普通广播图片

普通广播使用 topic 编辑器的图片协议。含图时，程序先只读真实编辑页，从页面全局状态取得 `upload_auth_token`；创建读取首页，更新读取 `/topic/{aid}/edit`，其中 aid 来自 `status.topic-id`。

`m.douban.com` topic API 与 `www.douban.com` 页面/上传端点使用独立
Cookie 会话。网页会话只显式复用 API 会话取得的 `ck`；两边 Cookie
不合并，因而不会丢失浏览器数据库中的 host/path 作用域。

本地图片上传：

```http
POST https://www.douban.com/j/group/topic/add_photo
Content-Type: multipart/form-data
```

字段为 `ck`、`image_file`、`primary_color` 和 `upload_auth_token`。远程 HTTPS 图片让豆瓣端点抓取：

```http
POST https://www.douban.com/j/group/topic/fetch_photo
Content-Type: application/json
X-CSRF-TOKEN: {ck}

{"photo_url":"https://..."}
```

上传响应必须给出可用图片 ID 和 HTTPS URL。raw 中对应的 `IMAGE` entity 保存 `id`、`src`、`raw_src`、`caption`，并保留响应中存在的宽高、主色和动画标记。新建广播时，payload 按正文图片出现顺序把每次独立处理所得的 ID 编为 `1_id1,2_id2,...`；更新广播时，仍存在的旧图片沿用编辑页中的 `seq_id`，新图从旧图片最大 `seq_id + 1` 开始编号。无图时 `image_ids` 为空字符串且省略布局，有图时提交当前布局，缺省为纵向。客户端不另设扩展名或格式白名单，由豆瓣上传接口决定是否接受。

只有路径符合 `/view/group_topic/{variant}/public/p{ID}.{ext}` 的豆瓣 CDN URL 才能直接恢复 topic 图片 ID；其他豆瓣 CDN 图片（例如条目封面）仍须先经过 `fetch_photo`，不能把文件名尾部数字直接当成 `image_ids`。

### 文集字段

文集是整篇内容的发布字段，不是 Draft.js entity。它只用于普通广播：源稿
`status.anthology-id` 只持久化正整数 ID，并作为字符串 `anthology_id`
写入 topic payload。Markdown `douban.status` 子 mapping 中的
`anthology-id:` 值槽和 Org 文档关键字
`#+DOUBAN_STATUS_ANTHOLOGY_ID:` 由 `douban-mode` 注册
completion-at-point；候选主文本是当前登录账号的文集名称，旁注显示篇数，
重名时才附加 ID。补全列表直到前端真正枚举候选时才读取，并按源 buffer
缓存；最终选定后才把名称替换为规范 ID。创建时未设置便省略
`anthology_id`；更新已有普通广播时未设置表示沿用 edit state 中的文集。
长评和日记不接受该叶子字段。

已有文集通过 `GET /rexxar/api/v2/user/{uid}/anthologies` 分页读取。当前网页会
同时发送登录 Cookie，并把其中非空的 `ck` 附加为查询参数；下一页位置取响应
的 `start + count`，而不是按本页数组长度猜测。

新建文集是独立、非幂等的远端操作。当前首页 topic 编辑器使用：

```http
POST https://m.douban.com/rexxar/api/v2/doulist/create
Content-Type: multipart/form-data
X-CSRF-TOKEN: {ck}
Origin: https://www.douban.com
Referer: https://www.douban.com/
```

multipart 字段为 `title`、空 `desc`、字符串 `is_private=false`、
`type=anthology`，以及文件字段 `header_bg_image`。网页把标题限制为
trim 后非空且最多 20 个字符；文集固定公开，封面必填，并在客户端裁为
800×800、quality 0.8 的 JPEG。`douban-new-anthology` 验证文件的 JPEG
标识，但不执行裁剪，因此用户应预先准备 800×800 方图。成功响应必须直接
提供可验证的 `id` 与同名 `title`；
若还提供 `doulist_type` 或 `category`，则必须分别为 `anthology` 与
`common`。网络中断、非明确客户端错误或无法确认身份的成功响应都作为
“可能已创建”处理，绝不自动重试；确认成功后清空所有源稿 buffer 的文集
候选缓存。旧 `/doulist/new` 只创建普通豆列，不用于文集。

`/{type}/{id}/available_doulists` 返回的是包含普通豆列在内的通用列表，不是创建接口的文集专用字段。长评、日记等内容发布后加入豆列属于另一条 mutation；当前没有足够的 Web 写请求与成功响应证据，因此实现不把它泛化成 `status.anthology-id` 以外的字段。

## 日记协议

### 读取发布上下文并绑定日记 ID

新建日记发布前，程序在后台读取创建页提供的发布上下文：

```http
GET https://www.douban.com/note/create
```

该上下文会预分配 `note_id`。源稿没有 `note.id` 时读取创建页；存在
`note.id` 时读取对应编辑页：

```http
GET https://www.douban.com/note/{note_id}/edit
```

发布上下文必须提供与预期一致的 `note_id`、非空 `ck`、允许的
`note_privacy` 值、相应的提交模式和需要图片时的上传凭据。程序在任何
上传或写操作前把新分配的 ID 原子写入源稿的 `note.id`。

这是后台 HTTP 上下文读取，不会打开浏览器编辑器。`note.id` 是稳定内容
ID。没有 `note.id` 时，创建页必须返回 `action: "new"`；程序先保存页面
预分配的 ID，再执行首次发布。已有 `note.id` 时，编辑页的 `action`
决定操作：`new` 恢复同一份尚未发布的草稿，其他受支持的提交模式更新
已发布日记。源稿无需另存发布状态或 URL，也不会仅凭 ID 猜测两者。

### 直接发布

```http
POST https://www.douban.com/j/note/publish
Content-Type: application/x-www-form-urlencoded; charset=UTF-8
```

主要字段：

| 字段 | 值 |
| --- | --- |
| `is_rich` | `1` |
| `note_id` | 发布上下文预分配且已写回的 ID |
| `note_title` | 用户手写标题，最多 100 个 UTF-16 code unit |
| `note_text` | 序列化的 Draft.js raw |
| `note_privacy` | `note.privacy` 的 `public` / `friends` 映射出的 `P` / `F`，并经当前发布上下文复核 |
| `cannot_reply` | `on` 或空字符串 |
| `author_tags` | 以空格连接的标签 |
| `ck` | 本次发布上下文值 |
| `action` | 创建和恢复首次发布时为 `new`；更新使用编辑页绑定的提交模式 |

程序不会调用 autosave。新建时先写回预分配的 `note.id`；所有状态都在
处理完图片后只调用一次 `/j/note/publish`。响应必须是 JSON、显式包含
`r`，且 `r` 只能是 JSON `false` 或数字 `0`；`error: false` 和只有 URL
的响应都不能替代这个成功标记。URL 可以省略，但出现时必须是与当前
`note.id` 一致的规范日记 URL。URL 只用于校验和完成消息，不进入
metadata；恢复首次发布和更新都保留原 `note.id`。

### 日记图片

```http
POST https://www.douban.com/j/note/add_photo
Content-Type: multipart/form-data
```

字段包括 `ck`、`note_id`、发布上下文给出的动态上传 token 和 `image_file=<binary>`。远程图片不会让豆瓣端点直接抓取；程序先用不带豆瓣凭据的 HTTPS GET 下载并验证，再以 `image_file` 上传。

## 新式读书笔记协议

这里只实现当前 topic editor 的“新笔记”协议，不提供专门的交互式建稿命令。
用户在普通源稿的 `douban:` 下通过 metadata 补全选择 `annotation` 及其字段；
`annotation.subject-id` 支持按书名搜索并写入图书 ID。实现不读取旧笔记。
标题由用户填写，最多 70 个 UTF-16 code unit。

发布会话使用创建端点对应的 `m.douban.com` Cookie 和 `ck`，但写请求的
Referer 绑定到实际 `www.douban.com` 创建页或编辑页。任何 POST 前先确认
subject 是目标图书：

```http
GET https://m.douban.com/rexxar/api/v2/book/{book_id}
Referer: https://www.douban.com/topic/create?subject_id={book_id}&subtype=annotation
```

响应的 `id` 必须与 metadata 一致，`type` 必须是 `book`。实现还会尽力读取
当前编辑器的活动映射；失败不阻断发布，有合法名称时才增加 `hobbit_tag`：

```http
GET https://m.douban.com/rexxar/api/v2/hobbit/mapping?source_type=book&source_id={book_id}
```

### 创建与更新

创建请求为：

```http
POST https://m.douban.com/rexxar/api/v2/topic/post
Content-Type: application/json;charset=utf-8
Accept: application/json
X-CSRF-TOKEN: {ck}
Referer: https://www.douban.com/topic/create?subject_id={book_id}&subtype=annotation
Origin: https://www.douban.com
```

基础 JSON payload 为：

```json
{
  "title": "用户标题",
  "content": "{\"blocks\":[...],\"entityMap\":{}}",
  "image_ids": "",
  "topic_tag_ids": "",
  "interest_tags": "",
  "subtype": "annotation",
  "subject_id": "123",
  "accessible": "public",
  "reply_limit": "A",
  "explanation_types": "",
  "send_status": false,
  "original": true,
  "is_event": false,
  "is_activity_rule": false,
  "enable_item_tag": false
}
```

创建默认公开，并按 `douban-default-reply-limit` 发送 `reply_limit:"A"`
（`all`）或 `reply_limit:"F"`（`following`）。`annotation.privacy: private`
会同时发送 `accessible:"private"` 与 `reply_limit:"N"`。显式
`annotation.explanation-types: none` 发送 `N`；没有选择才发送空字符串。
其他内容说明代码与长评、广播相同。payload 不含 `group_id`。

`send_status` 由 `douban-review-send-broadcast` 控制，默认发送 `false`，不为
读书笔记生成广播；显式启用该选项时发送 `true`。创建和更新使用同一规则。
`original` 不来自源稿 metadata；创建时由默认为 `t` 的
`douban-default-original` 决定。更新时不应用该创建默认值，而是保留
编辑页中的现有原创状态。

有 `annotation.id` 时，先 GET：

```http
https://www.douban.com/topic/{topic_id}/edit
```

页面的 `__INIT_STATE__.topic` 必须同时满足：`id` 与 metadata 相等、
`subtype` 是 `annotation`、subject ID 与 `annotation.subject-id` 相等，并且
包含需要保留的设置与图片状态。任何一项不符都会在上传或 POST 前终止。
更新端点为：

```http
POST https://m.douban.com/rexxar/api/v2/group/topic/{topic_id}/post
Referer: https://www.douban.com/topic/{topic_id}/edit
```

topic ID 只进入 URL，实际 JSON body 不带 `id`。更新时实现保留远端的
`accessible`、`reply_limit`、原创、内容说明、兴趣标签、`video_info`、文集、
已有图片 `seq_id` 和布局；源稿中的其他设置仍可按各自规则覆盖。公开切换为
私密时回复范围固定为 `N`；私密切回公开时使用
`douban-default-reply-limit` 对应的 `A` 或 `F`。

创建 2xx 响应只要提供有效 topic `id` 或可解析的 `/topic/{id}/` URL 之一，
就能构造规范 URL；两者同时出现时必须一致。两者都没有、任一已出现字段
无效或相互冲突时，远端可能已经创建成功，但程序不会写回 checkpoint，也
不会自动重发。成功时只把 `annotation.id` 原子写回源稿。普通 4xx（408
除外）是明确失败；传输中断、408、5xx 和无法判定的响应按不确定创建处理。
已有 ID 的更新永远不会退化成再次创建。
更新与当前 bundle 一样，要求 HTTP 2xx 且 `response.data` 按 JavaScript
语义为 truthy；空 body、`null`、`false`、`0` 和空字符串都不能确认成功。
响应不必包含固定字段，但若出现 `id` 或 URL，必须仍指向当前 topic。

### 读书笔记中的引用

本实现不生成编辑器专用的原生摘录块。Markdown 和 Org 的引用均按普通
`blockquote` 编译；章节、页码等出处信息需要作为可见正文编写。

### 读书笔记图片

读书笔记与 personal topic 共用 `group_topic` 图片协议：页面会话提供上传
token，本地图片以 `image_file` multipart 上传，HTTPS 远程图片交给
`fetch_photo`，最终 entity 使用 `id`、`src`、`raw_src` 和 `caption`。
更新时保留已有图片的 `seq_id`，新增图片从现有最大序号后继续；有图才发送
`image_layout`。

## 普通广播协议

普通广播是 Draft.js 富文本；线上对象是个人 `personal/topic`，支持创建和更新，不再使用旧首页纯文本表单。

建立广播会话时，程序先读取适用于创建端点的浏览器 Cookie。浏览器数据库没有持久化 `ck` 时，按通用规则只读一次 `/mine/` 并把响应设置的 session `ck` 合并进该会话。无图时不解析首页发布表单；有图时只读首页以取得上传凭据。

```http
https://m.douban.com/rexxar/api/v2/topic/post
```

创建请求与 2026-07 的 `personal-topic-editor.42ee6.js` 一致：

```http
POST https://m.douban.com/rexxar/api/v2/topic/post
Content-Type: application/json;charset=utf-8
Accept: application/json
X-CSRF-TOKEN: {ck}
Referer: https://www.douban.com/
Origin: https://www.douban.com
```

`ck` 不进入 JSON 正文；它只作为 `X-CSRF-TOKEN`，同时当前会话 Cookie 随请求发送。外层 JSON 的基础 personal payload 为：

```json
{
  "content": "{\"blocks\":[...],\"entityMap\":{}}",
  "image_ids": "",
  "interest_tags": "",
  "subtype": "personal",
  "group_id": "0",
  "accessible": "public",
  "reply_limit": "A",
  "send_status": true,
  "explanation_types": "",
  "original": true
}
```

`content` 不是嵌套 JSON 对象，而是 Draft.js raw 再 `JSON.stringify` 一次所得的字符串；因此整个请求是二次 JSON 编码。正文由 Markdown 或 Org 经 HTML 转成 raw，可以包含普通格式、图片、链接卡片和块高亮。无图时 `image_ids` 是空字符串；有图时按公共 topic 图片协议提交图片 ID 和 `image_layout: "vertical"`。设置了 `status.anthology-id` 时才增加字符串 `anthology_id`。`group_id` 必须是字符串 `"0"`，不能省略，也不是 JSON 数字 `0`。

### 广播发布设置

源稿设置与网页协议的映射为：

| metadata | 创建/更新 payload | 语义 |
| --- | --- | --- |
| `status.explanation-types` | 映射后的 `explanation_types` 字符串 | 单项内容说明 |

新建固定发送 `accessible:"public"`，并按 `douban-default-reply-limit` 发送
`reply_limit:"A"`（`all`）或 `reply_limit:"F"`（`following`）。回复范围
不来自源稿 metadata；更新时保留 edit state 中的 `accessible` 与
`reply_limit`。`original` 同样不来自源稿 metadata；创建时由
`douban-default-original` 决定，更新时则保留 edit state 中的现有值。
原创与内容说明彼此独立，网页仍允许原创声明与转载说明 `R` 同时出现。

内容说明的源稿值依次映射为：`ai-generated` → `A`、`fictional` → `X`、
`marketing` → `K`、`minor-safety` → `M`、`public-affairs` → `P`、
`personal-opinion` → `O`、`repost` → `R`。网页严格单选。源稿用 `none`
表示“无需标注”：首页创建器会发送空字符串，独立编辑器则使用 `N`，
因此实现按创建/更新分别发送 `explanation_types:""` / `"N"`。长评使用
同一映射，其中 `none` 发送空字符串。源稿在所有路径上都只接受一个标量
枚举，不接受 YAML sequence 或逗号分隔的多值字符串。

`douban-default-reply-limit` 是读书笔记和普通广播统一的回复范围配置，可选
`all` 或 `following`，默认为 `all`。它用于新建公开内容；读书笔记私密时
强制使用 `N`，从私密切回公开时重新使用该配置。其他更新保留 edit state
中的现有回复范围。`douban-default-original` 是新建长评、读书笔记和普通
广播唯一的原创声明开关，默认为 `t`，不对应任何源稿 metadata。长评更新
仍由该配置控制；读书笔记和普通广播更新则沿用 edit state 中的现有原创状态。

Draft.js raw 正文不能为空；文字块或有效原子块都可构成正文。当前 personal-topic 编辑器没有旧首页广播的 140 UTF-16 code unit 前端限制，本包也不再施加该旧限制。

创建端点的成功响应不提供可直接写回的广播 `status sid`。响应顶层若出现有效正整数 `id`，它是 personal topic ID，也就是首页的 `aid`，不能当作广播 `sid`。

因此 2xx 响应后，程序先按当前网页编辑器留出的约 300ms 可见性窗口等待，再使用同一登录态、`Cache-Control: no-cache` 读取首页，并只匹配：

```text
.status-item[data-atype="personal/topic"]
```

候选必须同时具有数字 `data-uid`、`data-sid` 和 `data-aid`。响应给出 topic ID 时，程序优先要求 `data-aid` 与之相等；响应没有该 ID 时，才把 raw 中所有非原子块的文字规范化后，与候选 `blockquote` 纯文本做唯一匹配。只有恰好一个匹配项才确认成功：

```text
data-sid  -> status.id
data-aid  -> status.topic-id
https://www.douban.com/people/{data-uid}/status/{data-sid}/
```

`aid` 与 `sid` 是两个不同命名空间；源稿的 `status.id` 必须写 `sid`，
`status.topic-id` 必须写 `aid`。创建成功时，程序同时写回 `status.id`、
`status.topic-id`。规范 URL 用于完成消息，不写入源稿。首页是创建流程
取得 `sid` 并完成发布对账的唯一依据。纯图片或纯链接卡片没有可用于正文
回查的非原子文字，因此响应又缺少 topic ID 时无法自动确认。POST 已返回
2xx 但首页没有唯一匹配时，发布已经被接受却无法安全写回 checkpoint，
程序会要求用户到个人主页记录链接并禁止直接重发。普通 4xx（408 除外）
视为明确失败；传输中断、408、5xx 或其他无法判断服务端是否写入的结果
仍按不确定创建处理。

### 更新

只有 `status.id` 和 `status.topic-id` 同时存在时才进入更新路径；两者
只出现一个会直接报错。更新端点使用 personal topic aid，不使用公开
wrapper sid：

```http
POST https://m.douban.com/rexxar/api/v2/group/topic/{aid}/post
Content-Type: application/json;charset=utf-8
Accept: application/json
X-CSRF-TOKEN: {ck}
Referer: https://www.douban.com/topic/{aid}/edit
Origin: https://www.douban.com
```

aid 取自 `status.topic-id`，只出现在更新 URL 中，JSON 不提交 `id`、
`title` 或 `group_id`。正文仍是二次 JSON 编码的 Draft.js raw，`subtype`
固定为 `"personal"`。每次更新都先只读对应 edit 页，核对 personal topic
的 aid，并取得现有回复权限、可见范围、兴趣标签、原创、内容说明与视频标记、文集、
`photos[].seq_id` 与图片布局；含图时还要求该页提供
`upload_auth_token`。`video_info` 保持 JSON null 或原对象，兴趣标签取
对象的 `name` 后以 `#` 连接，personal 的 `topic_tag_ids` 固定为空字符串。
旧图保留序号，新图从现有最大序号继续编号。源稿显式设置
`status.explanation-types` 或 `status.anthology-id` 时覆盖对应页面值，否则
保留现有设置。回复范围始终保留页面值。
原创状态没有对应的 status metadata，因此始终保留页面值。

网页编辑器对更新响应不要求 `r`、`id` 或 `url` 成功标记，但只在 Axios 取得 JavaScript truthy 的响应数据时进入完成状态；因此本包要求 2xx 和同样的 truthy 数据。空 body、JSON `null`、`false`、`0` 与空字符串均不算成功；空对象和空数组在 JavaScript 中为 truthy。更新不读取首页对账，不写入新的 checkpoint，也绝不因失败而退回 `/topic/post` 创建新广播。明确 4xx 作为拒绝返回；传输错误、408 与 5xx 作为更新错误返回，由用户决定是否再次更新同一个 topic。

## 更新边界

更新能力有意收窄：

- `review`：没有 `review.id` 时创建；有 `review.id` 时更新。不含图片时按 `review.subject-type` 选择固定同源端点，使用发布页上下文时还必须核对 `review.id` 和 `review.subject-id`；两条路径都只接受规范评论 URL，更新时还核对 URL 中的 ID；
- `note`：没有 `note.id` 时读取创建页、保存预分配 ID 并首次发布；有 `note.id` 时复用匹配 ID 的 `/note/{note_id}/edit` 页面会话，由页面 action 区分恢复尚未发布的草稿与更新已发布日记；
- `annotation`：没有 `annotation.id` 时通过新式 topic 入口创建；有 ID 时必须从 edit state 同时核对 topic ID、`annotation` subtype 和图书 subject，随后只更新同一 topic；
- `status`：没有 `status.id` 和 `status.topic-id` 时创建；两者同时存在时用 aid 更新同一 personal topic，并保持两个 checkpoint 字段不变。

因此页面字段或提交模式发生未知变化时不会降级成创建，也不能通过修改或
混用不同内容类型的 checkpoint 字段绕过 ID 校验。

## 不确定创建

创建 POST 不是幂等操作。传输中断、HTTP 408 或响应无法核对，都可能发生在服务端完成写入之后。四类内容的创建请求都只发送一次，不会自动重试；通常在结果无法确认时抛出 `douban-create-result-unknown`。普通广播 POST 已返回 2xx 但首页无法唯一取得 `sid`，以及读书笔记 POST 已返回 2xx 却没有一致可用的 topic 身份时，会抛出 `douban-published-but-not-checkpointed`，表示远端已经接受发布而本地不能安全写回。

确认成功时，程序只在各流程要求的成功标记与远端身份通过验证后写回
checkpoint：长评、日记和读书笔记写 `id`，普通广播写 `id` 与
`topic-id`。URL 只用于校验和消息，不写入源稿。普通 4xx（408 除外）或
明确的表单/JSON 错误直接作为失败返回。

### 对账与恢复

结果不确定时：

- 长评到个人页检查对应条目和标题；
- 日记用源稿中已保存的 `note.id` 检查对应日记或草稿，不新建另一个草稿；
- 读书笔记到对应图书的新式笔记列表检查 topic，记录其 `/topic/ID/` 链接且不要直接重发；
- 普通广播只到个人主页检查；以首页 personal/topic 的 `data-sid` 为广播 ID，不能用 `data-aid` 代替。

只有确认服务端没有创建内容后，才可手动重试。日记还有一层更早的恢复信息：发布上下文在公开发布前已经分配稳定 `note_id`，程序会先保存这个 ID，再处理图片和发布。

### 写回失败

远端成功后，程序用同目录临时文件原子替换 metadata。若远端已成功而本地
ID checkpoint 写回失败，会抛出专门错误，并在消息中给出远端 ID 和规范
URL；此时必须手工记录并修复源稿，不能重新创建。

## 失效边界

以下变化都可能要求更新代码：

- Cookie 数据库格式或系统密钥存储方式改变；
- 页面不再提供预期的 `ck`、ID 或上传 token；
- 发布端点、字段名、Draft.js schema 或图片响应结构改变；
- 登录增加新的验证码、设备验证或跨源跳转；
- 成功响应不再含该流程要求的可核对成功标记，或首页 personal/topic 的 `data-aid` / `data-sid` 映射发生变化。

遇到这些情况，实现选择停止：不猜字段、不跨主机跟随重定向、不把未知响应当成功、不自动重试不确定创建。

## 合规说明

豆瓣未公开网页接口不构成稳定公共 API。使用者应阅读并遵守[豆瓣使用协议](https://www.douban.com/about/agreement)和[豆瓣法律声明](https://www.douban.com/about/legal)，只操作自己的内容，避免批量、高频、垃圾发布或规避安全机制。
