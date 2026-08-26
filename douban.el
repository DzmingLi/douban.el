;;; douban.el --- Write and publish Douban content  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dzming Li

;; Author: Dzming Li <i@dzming.li>
;; Maintainer: Dzming Li <i@dzming.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (markdown-mode "2.7") (plz "0.10-pre") (yaml "1.2.4"))
;; Keywords: convenience, hypermedia, tools
;; URL: https://github.com/DzmingLi/douban.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use Markdown files as the source of Douban reviews, notes, book
;; annotations, and ordinary statuses.
;; The package reads an existing browser login from an explicitly selected
;; profile, converts rich source documents to the Draft.js raw format where
;; required, and submits each content type through its validated web form.
;;
;; This package uses undocumented web endpoints.  They may change without
;; notice.  It deliberately exposes only an interactive, one-item-at-a-time
;; publishing workflow.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'plz)
(require 'url)
(require 'url-util)
(require 'json)
(require 'dom)
(require 'xml)
(require 'image)
(require 'mailcap)
(require 'sqlite)
(require 'gnutls)
(require 'xdg)

(declare-function secrets-search-item-paths "secrets"
                  (collection &rest attributes))
(declare-function secrets-get-secret "secrets" (collection item))
(declare-function markdown-mode "markdown-mode" ())

(defgroup douban nil
  "在 Emacs 中编辑并发布豆瓣内容。"
  :group 'applications
  :prefix "douban-")

(define-error
 'douban-create-result-unknown
 "Douban content creation result is unknown")

(define-error
 'douban-published-but-not-checkpointed
 "Douban content was published but local metadata was not saved")

(define-error
 'douban-review-broadcast-cleanup-failed
 "Douban review was published but its broadcast was not removed")

;;;; Customization

(defconst douban-minimum-review-length 140
  "长评正文允许的最少非空白字符数。")

(defcustom douban-user-agent
  "Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0"
  "豆瓣网页请求使用的 User-Agent。"
  :type 'string
  :group 'douban)

(defcustom douban-review-directory
  (file-name-as-directory
   (expand-file-name
    "douban/reviews"
    (or (xdg-user-dir "DOCUMENTS")
        (expand-file-name "Documents" "~"))))
  "`douban-new-review' 默认创建长评源稿的目录。
交互调用新建命令时，如果该目录尚不存在，会自动创建。Lisp 调用显式传入的
文件路径不受本选项限制。"
  :type 'directory
  :group 'douban)

(defcustom douban-default-reply-limit 'all
  "新建读书笔记和普通广播的默认回复范围。
`all' 表示所有人可回复，`following' 表示仅我关注的用户可回复。更新时
通常保留远端设置；私密读书笔记始终禁止回复，从私密切回公开时使用本选项。"
  :type '(choice
          (const :tag "所有人可回复" all)
          (const :tag "仅我关注的用户可回复" following))
  :group 'douban)

(defcustom douban-default-original t
  "非 nil 时，长评及新建的读书笔记和普通广播勾选原创内容声明。
更新读书笔记和普通广播时保留远端设置；更新长评时使用本选项。"
  :type 'boolean
  :group 'douban)

(defcustom douban-review-send-broadcast nil
  "非 nil 时，发布长评或读书笔记时发送并保留对应广播。
读书笔记通过请求字段直接控制是否发送广播。长评网页接口在创建时总会自动
生成广播；本选项为 nil 时，程序会先把新评论 ID 安全写回源稿，再删除唯一
匹配该 ID 的广播。因此长评广播可能短暂可见。更新已有长评不会删除历史广播。"
  :type 'boolean
  :group 'douban)

(defcustom douban-cc-statement nil
  "发布长评和日记时在正文末尾追加的 Creative Commons 声明。
nil 表示不追加；其它值选择一种 CC 4.0 国际许可或 CC0 1.0 公共领域
贡献。声明只覆盖未另行声明的原创内容，不作用于读书笔记或普通广播，
也不修改源稿。
CC 许可和 CC0 均不可撤销，启用前应确认自己拥有或控制相关内容的版权。"
  :type '(choice
          (const :tag "不追加 CC 声明" nil)
          (const :tag "CC0 1.0（公共领域贡献）" cc0)
          (const :tag "CC BY 4.0（署名）" by)
          (const :tag "CC BY-SA 4.0（署名—相同方式共享）" by-sa)
          (const :tag "CC BY-ND 4.0（署名—禁止演绎）" by-nd)
          (const :tag "CC BY-NC 4.0（署名—非商业性使用）" by-nc)
          (const
           :tag "CC BY-NC-SA 4.0（署名—非商业性使用—相同方式共享）"
           by-nc-sa)
          (const
           :tag "CC BY-NC-ND 4.0（署名—非商业性使用—禁止演绎）"
           by-nc-nd))
  :group 'douban)

;;;; Data structures

(cl-defstruct (douban--session
               (:constructor douban--make-session))
  "一次发布操作的认证、请求来源与页面状态。"
  kind
  cookies
  ck
  referer
  host
  state)

(defun douban--session-state-get (session key)
  "返回 SESSION 协议状态中 KEY 对应的值。"
  (plist-get (douban--session-state session) key))

(cl-defstruct (douban--draft
               (:constructor douban--make-draft))
  "正在构建的可变 Draft.js 文档。"
  blocks
  entities
  next-entity
  next-block
  (toc-headings nil :read-only t))

(cl-defstruct (douban--block
               (:constructor douban--make-block))
  "正在构建的可变 Draft.js 内容块。"
  key
  type
  depth
  (text "" :read-only t)
  (utf16-length 0 :read-only t)
  inline-ranges
  entity-ranges
  data)

(cl-defstruct
    (douban--metadata-completion-session
     (:constructor douban--make-metadata-completion-session))
  "一次 metadata 值补全的源稿位置、候选和提交规则。"
  source-buffer
  start-marker
  end-marker
  format
  sequence-item-p
  kind
  field
  seen
  value-function)

(cl-defstruct
    (douban--metadata-source-index
     (:constructor douban--make-metadata-source-index))
  "一次 metadata CAPF 上下文读取到的源稿快照。
KINDS 是源稿中已有的类型容器；ENTRIES 按类型保存已有字段和简单标量值。
该结构只挂在当前补全上下文上，绝不跨 CAPF 调用缓存。"
  kinds
  entries)

;;;; Cookies

(defcustom douban-cookie-browser 'firefox
  "提供豆瓣登录 Cookie 的浏览器。
Firefox 直接读取 profile 的 SQLite 数据库；Chromium 系浏览器还会通过
Linux 桌面凭据存储取得 Cookie 解密密钥。"
  :type '(choice
          (const :tag "Firefox" firefox)
          (const :tag "Chromium" chromium)
          (const :tag "Google Chrome" chrome))
  :group 'douban)

(defcustom douban-cookie-profile-directory nil
  "用于读取豆瓣 Cookie 的浏览器 profile 目录。
必须显式设置；本包不会扫描或猜测 profile。Firefox 应指向包含
cookies.sqlite 的目录，Chromium/Chrome 应指向包含 Network/Cookies
的目录。"
  :type '(choice (const :tag "未配置" nil)
                 (directory :must-match t))
  :group 'douban)

(defcustom douban-firefox-origin-attributes ""
  "豆瓣 Cookie 使用的准确 Firefox `originAttributes' 值。
空字符串表示普通的非容器上下文。Firefox Multi-Account Containers 常用
`^userContextId=2' 之类的值；请显式设置此选项，以免混用不同容器的 Cookie。"
  :type 'string
  :group 'douban)

(cl-defstruct
    (douban--cookie-record
     (:constructor douban--make-cookie-record))
  "从浏览器数据库读取、尚未按请求 URL 筛选的 Cookie。"
  name value domain path expires secure creation)

(declare-function
 dbus-call-method "dbus"
 (bus service path interface method &rest args))

(defconst douban--chromium-browser-specs
  '((chromium
     :secret-applications ("chromium")
     :kwallet-folder "Chromium Keys"
     :kwallet-key "Chromium Safe Storage")
    (chrome
     :secret-applications ("chrome")
     :kwallet-folder "Chrome Keys"
     :kwallet-key "Chrome Safe Storage"))
  "Chromium 系浏览器的 Linux 桌面凭据存储标识。")

(defconst douban--chromium-linux-v10-key
  (unibyte-string
   #xfd #x62 #x1f #xe5 #xa2 #xb4 #x02 #x53
   #x9d #xfa #x14 #x7c #xa9 #x27 #x27 #x78)
  "Chromium Linux 基础密码存储使用的 AES-128 密钥。")

(defconst douban--chromium-aes-cbc-iv
  (encode-coding-string (make-string 16 ?\s) 'us-ascii t)
  "Chromium v10/v11 AES-CBC 使用的固定 IV。")

(defconst douban--chromium-time-epoch-offset 11644473600000000
  "Unix epoch 之前的 Chromium 微秒数。")

(defun douban--cookie-store-file (browser)
  "返回 BROWSER 的显式 profile 中唯一的 Cookie 存储文件。"
  (let* ((relative
          (pcase browser
            ('firefox "cookies.sqlite")
            ((or 'chromium 'chrome) "Network/Cookies")
            (_
             (error "douban: 不支持的 Cookie 浏览器：%S" browser))))
         (profile douban-cookie-profile-directory))
    (unless profile
      (error
       "douban: 请设置 douban-cookie-profile-directory；本包不会自动选择浏览器 profile"))
    (let ((file (expand-file-name relative profile)))
      (unless (file-readable-p file)
        (error "douban: Cookie 文件不可读：%s" file))
      file)))

(defun douban--cookie-url-parts (url)
  "解析 URL，返回 (HOST PATH SECURE)。
URL 必须是带 host 的绝对 HTTP(S) URL。"
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (scheme (downcase (or (url-type parsed) "")))
         (filename (or (url-filename parsed) "/"))
         (path (car (split-string filename "[?#]" t))))
    (unless (and (stringp host) (not (string-empty-p host))
                 (member scheme '("http" "https")))
      (error "douban: 无法从 URL 读取浏览器 Cookie：%s" url))
    (list (downcase host)
          (if (and path (string-prefix-p "/" path)) path "/")
          (string-equal scheme "https"))))

(defun douban--cookie-domain-matches-p (domain host)
  "返回 DOMAIN Cookie 是否适用于 HOST。"
  (let ((domain (downcase domain))
        (host (downcase host)))
    (if (string-prefix-p "." domain)
        (let ((bare (substring domain 1)))
          (or (string-equal host bare)
              (string-suffix-p (concat "." bare) host)))
      (string-equal domain host))))

(defun douban--cookie-domain-candidates (host)
  "返回 HOST 可能匹配的浏览器 Cookie domain 候选。"
  (let ((candidates (list host (concat "." host)))
        (start 0))
    (while (string-match "\\." host start)
      (push (substring host (match-beginning 0)) candidates)
      (setq start (match-end 0)))
    (delete-dups candidates)))

(defun douban--cookie-path-matches-p (cookie-path request-path)
  "返回 COOKIE-PATH 是否适用于 REQUEST-PATH。"
  (let ((cookie-path (if (string-empty-p cookie-path) "/" cookie-path)))
    (or (string-equal cookie-path request-path)
        (and (string-prefix-p cookie-path request-path)
             (or (string-suffix-p "/" cookie-path)
                 (and (> (length request-path) (length cookie-path))
                      (eq (aref request-path (length cookie-path)) ?/)))))))

(defun douban--cookie-records-for-url (records url)
  "筛选并排序适用于 URL 的 RECORDS，返回 ((NAME . VALUE) ...)。"
  (pcase-let* ((`(,host ,path ,secure) (douban--cookie-url-parts url))
               (now (float-time))
               (applicable
                (cl-remove-if-not
                 (lambda (record)
                   (and
                    (douban--cookie-domain-matches-p
                     (douban--cookie-record-domain record) host)
                    (douban--cookie-path-matches-p
                     (douban--cookie-record-path record) path)
                    (or (not (douban--cookie-record-secure record)) secure)
                    (or (null (douban--cookie-record-expires record))
                        (> (douban--cookie-record-expires record) now))))
                 records)))
    ;; RFC 6265 发送顺序：更长的 Path 在前，同 Path 先创建的在前。
    (setq applicable
          (cl-stable-sort
           applicable
           (lambda (left right)
             (let ((left-length
                    (length (douban--cookie-record-path left)))
                   (right-length
                    (length (douban--cookie-record-path right))))
               (if (= left-length right-length)
                   (< (or (douban--cookie-record-creation left) 0)
                      (or (douban--cookie-record-creation right) 0))
                 (> left-length right-length))))))
    (mapcar
     (lambda (record)
       (cons (douban--cookie-record-name record)
             (douban--cookie-record-value record)))
     applicable)))

;; Firefox cookie database

(defun douban--select-firefox-cookies (db table url)
  "从 DB 的可信 TABLE 中选出适用于 URL 的默认容器 Cookie。"
  (pcase-let* ((`(,host ,_path ,_secure)
                 (douban--cookie-url-parts url))
               (domains (douban--cookie-domain-candidates host))
               (placeholders
                (mapconcat (lambda (_domain) "?") domains ","))
               (records
                (mapcar
                 (lambda (row)
                   (let ((expiry (nth 4 row)))
                     (douban--make-cookie-record
                      :name (nth 0 row)
                      :value (nth 1 row)
                      :domain (nth 2 row)
                      :path (or (nth 3 row) "/")
                      ;; Firefox profiles in the wild use both seconds and
                      ;; milliseconds for `expiry'.
                      :expires (and (numberp expiry)
                                    (not (zerop expiry))
                                    (if (> expiry 100000000000)
                                        (/ expiry 1000.0)
                                      expiry))
                      :secure (not (zerop (or (nth 5 row) 0)))
                      :creation (nth 6 row))))
                 (sqlite-select
                  db
                  (concat
                   "SELECT name, value, host, path, expiry, isSecure, "
                   "creationTime FROM " table " "
                   "WHERE host IN (" placeholders
                   ") AND originAttributes = ?")
                  (append
                   domains
                   (list douban-firefox-origin-attributes))))))
    (douban--cookie-records-for-url records url)))

(defun douban--sqlite-readonly-uri (path)
  "把 PATH 转成不会误解析文件名中 URI 保留字符的只读 SQLite URI。"
  (concat "file:"
          (url-hexify-string
           (expand-file-name path)
           (cons ?/ url-unreserved-chars))
          "?mode=ro&cache=private"))

(defun douban--query-cookie-database (path label query)
  "查询 Cookie 数据库 PATH，并以 LABEL 生成错误信息。
QUERY 接收数据库和 schema 名；只读查询发生 SQLite 错误时改读临时
数据库/WAL 副本。"
  (let ((run-query
         (lambda (db schema)
           (let (transaction)
             (unwind-protect
                 (progn
                   (sqlite-execute db "BEGIN")
                   (setq transaction t)
                   (prog1 (funcall query db schema)
                     (sqlite-execute db "COMMIT")
                     (setq transaction nil)))
               (when transaction
                 (ignore-errors (sqlite-execute db "ROLLBACK"))))))))
    (condition-case readonly-error
        (let (db)
          (unwind-protect
              (progn
                ;; `sqlite-open' 没有 readonly 参数，因此只创建内存主库，
                ;; 再以只读 URI ATTACH 浏览器数据库。
                (setq db (sqlite-open))
                (sqlite-execute
                 db "ATTACH DATABASE ? AS cookies"
                 (list (douban--sqlite-readonly-uri path)))
                (funcall run-query db "cookies"))
            (when db
              (ignore-errors (sqlite-close db)))))
      (sqlite-error
       (condition-case copy-error
           (let (snapshot-directory db)
             (unwind-protect
                 (let ((snapshot-name (file-name-nondirectory path)))
                   (setq snapshot-directory
                         (make-temp-file "douban-cookie-snapshot-" t))
                   (dolist (suffix '("" "-wal"))
                     (let ((source (concat path suffix)))
                       (when (file-readable-p source)
                         (copy-file
                          source
                          (expand-file-name
                           (concat snapshot-name suffix)
                           snapshot-directory)
                          t))))
                   (setq db
                         (sqlite-open
                          (expand-file-name
                           snapshot-name snapshot-directory)))
                   (funcall run-query db "main"))
               (when db
                 (ignore-errors (sqlite-close db)))
               (when snapshot-directory
                 (ignore-errors
                   (delete-directory snapshot-directory t)))))
         (error
          (error "douban: 读 %s 失败：只读访问：%s；临时副本：%s"
                 label
                 (error-message-string readonly-error)
                 (error-message-string copy-error))))))))

(defun douban--read-firefox-cookies (path url)
  "从 Firefox cookies.sqlite 的 PATH 读出适用于 URL 的 Cookie。
返回 ((NAME . VALUE) ...) alist。只读访问失败时改读临时 DB/WAL 副本。"
  (unless (file-readable-p path)
    (error "douban: Firefox Cookie 数据库不可读：%s" path))
  (douban--cookie-url-parts url)
  (douban--query-cookie-database
   path "Firefox cookies"
   (lambda (db schema)
     (douban--select-firefox-cookies
      db (concat schema ".moz_cookies") url))))

(defun douban--chromium-browser-spec (browser)
  "返回 BROWSER 的 Chromium 凭据存储配置。"
  (or (cdr (assq browser douban--chromium-browser-specs))
      (error "douban: 不支持的 Chromium 系浏览器：%S" browser)))

(defun douban--hmac-sha1-bytes (key bytes)
  "返回 KEY 对 BYTES 的 HMAC-SHA1 原始字节。"
  ;; GnuTLS 会主动清空字符串形式的 key，传副本避免破坏调用方缓存。
  (gnutls-hash-mac 'SHA1 (copy-sequence key) bytes))

(defun douban--xor-byte-strings (left right)
  "逐字节异或等长的 LEFT 与 RIGHT。"
  (let ((output (copy-sequence left)))
    (dotimes (index (length output))
      (aset output index
            (logxor (aref output index) (aref right index))))
    output))

(defun douban--pbkdf2-hmac-sha1 (password salt iterations length)
  "以 PASSWORD、SALT 和 ITERATIONS 派生 LENGTH 字节 PBKDF2 密钥。"
  (let ((block 1)
        (output (unibyte-string)))
    (while (< (length output) length)
      (let* ((counter
              (unibyte-string
               (logand (ash block -24) #xff)
               (logand (ash block -16) #xff)
               (logand (ash block -8) #xff)
               (logand block #xff)))
             (unit
              (douban--hmac-sha1-bytes
               password (concat salt counter)))
             (accumulator (copy-sequence unit)))
        (dotimes (_ (1- iterations))
          (setq unit (douban--hmac-sha1-bytes password unit)
                accumulator
                (douban--xor-byte-strings accumulator unit)))
        (setq output (concat output accumulator)
              block (1+ block))))
    (substring output 0 length)))

(defun douban--chromium-secret-service-password (spec)
  "按照 SPEC 从 Secret Service 读取 Chromium 安全存储密钥。"
  (require 'secrets)
  (unless (and (boundp 'secrets-enabled)
               (symbol-value 'secrets-enabled))
    (error "douban: 当前会话没有可用的 Secret Service"))
  (let ((collections
         (delete-dups
          (cons "default" (secrets-list-collections))))
        secret)
    (dolist (application (plist-get spec :secret-applications))
      (dolist (collection collections)
        (unless secret
          (dolist
              (item
               (secrets-search-item-paths
                collection
                :xdg:schema
                "chrome_libsecret_os_crypt_password_v2"
                :application application))
            (unless secret
              (setq secret
                    (secrets-get-secret collection item)))))))
    (unless (and (stringp secret) (not (string-empty-p secret)))
      (error "douban: Secret Service 中没有浏览器 Safe Storage 密钥"))
    (encode-coding-string secret 'utf-8 t)))

(defconst douban--chromium-kwallet-endpoints
  '(("org.kde.kwalletd6" "/modules/kwalletd6")
    ("org.kde.kwalletd5" "/modules/kwalletd5")
    ("org.kde.kwalletd" "/modules/kwalletd"))
  "当前 KDE KWallet D-Bus 服务及其对象路径。")

(defun douban--chromium-kwallet-password-at (endpoint spec)
  "从 KWallet ENDPOINT 读取 SPEC 对应的安全存储密码。"
  (require 'dbus)
  (let* ((service (car endpoint))
         (path (cadr endpoint))
         (interface "org.kde.KWallet")
         (application "douban.el")
         handle)
    (unwind-protect
        (progn
          (unless
              (dbus-call-method
               :session service path interface "isEnabled")
            (error "KWallet 未启用"))
          (let ((wallet
                 (dbus-call-method
                  :session service path interface "networkWallet")))
            (setq handle
                  (dbus-call-method
                   :session service path interface "open"
                   wallet :int64 0 application)))
          (unless (and (integerp handle) (>= handle 0))
            (error "KWallet 无法打开"))
          (unless
              (dbus-call-method
               :session service path interface "hasFolder"
               :int32 handle
               (plist-get spec :kwallet-folder)
               application)
            (error "KWallet 中没有浏览器密钥目录"))
          (unless
              (dbus-call-method
               :session service path interface "hasEntry"
               :int32 handle
               (plist-get spec :kwallet-folder)
               (plist-get spec :kwallet-key)
               application)
            (error "KWallet 中没有浏览器 Safe Storage 密钥"))
          (let ((password
                 (dbus-call-method
                  :session service path interface "readPassword"
                  :int32 handle
                  (plist-get spec :kwallet-folder)
                  (plist-get spec :kwallet-key)
                  application)))
            (unless
                (and (stringp password)
                     (not (string-empty-p password)))
              (error "KWallet 中的浏览器密钥为空"))
            (encode-coding-string password 'utf-8 t)))
      (when (and (integerp handle) (>= handle 0))
        (ignore-errors
          (dbus-call-method
           :session service path interface "close"
           :int32 handle nil application))))))

(defun douban--chromium-kwallet-password (spec)
  "从当前 KDE KWallet 读取 SPEC 对应的安全存储密码。"
  (let (password last-error)
    (dolist (endpoint douban--chromium-kwallet-endpoints)
      (unless password
        (condition-case err
            (setq password
                  (douban--chromium-kwallet-password-at endpoint spec))
          (error (setq last-error err)))))
    (or password
        (error "douban: 从 KWallet 读取浏览器密钥失败：%s"
               (if last-error
                   (error-message-string last-error)
                 "没有可用服务")))))

(defun douban--chromium-linux-password (spec)
  "从 Linux 桌面凭据存储读取 SPEC 的安全存储密码。"
  (condition-case secret-service-error
      (douban--chromium-secret-service-password spec)
    (error
     (condition-case kwallet-error
         (douban--chromium-kwallet-password spec)
       (error
        (error
         "douban: 无法读取浏览器 Safe Storage 密钥：Secret Service：%s；KWallet：%s"
         (error-message-string secret-service-error)
         (error-message-string kwallet-error)))))))

(defun douban--chromium-v10-key ()
  "返回 GNU/Linux Chromium v10 的基础密码存储密钥。"
  (copy-sequence douban--chromium-linux-v10-key))

(defun douban--chromium-v11-key (spec)
  "按照当前系统与 SPEC 返回 Chromium v11 密钥。"
  (let ((password (douban--chromium-linux-password spec)))
    (unwind-protect
        (douban--pbkdf2-hmac-sha1
         password (encode-coding-string "saltysalt" 'us-ascii t) 1 16)
      (clear-string password))))

(defun douban--pkcs7-unpad (bytes block-size)
  "校验并移除 BYTES 的 PKCS#7 填充，块大小为 BLOCK-SIZE。"
  (let* ((length (length bytes))
         (padding (and (> length 0) (aref bytes (1- length)))))
    (unless (and (integerp padding)
                 (> padding 0)
                 (<= padding block-size)
                 (<= padding length)
                 (cl-loop for index from (- length padding) below length
                          always (= (aref bytes index) padding)))
      (error "douban: Chromium Cookie 的 AES padding 无效"))
    (substring bytes 0 (- length padding))))

(defun douban--chromium-aes-cbc-decrypt (key ciphertext)
  "以 KEY 解密 Chromium AES-CBC CIPHERTEXT。"
  (let ((result
         (gnutls-symmetric-decrypt
          'AES-128-CBC
          (copy-sequence key)
          douban--chromium-aes-cbc-iv
          ciphertext)))
    (unless (and (consp result) (stringp (car result)))
      (error "douban: Chromium Cookie AES 解密失败"))
    (douban--pkcs7-unpad (car result) 16)))

(defun douban--chromium-decrypt-cookie
    (encrypted-value host-key database-version keys)
  "用 KEYS 解密 ENCRYPTED-VALUE，并校验 HOST-KEY 域绑定。"
  (unless (and (stringp encrypted-value)
               (>= (length encrypted-value) 3))
    (error "douban: Chromium Cookie 密文无效"))
  (let* ((prefix (substring encrypted-value 0 3))
         (key (cdr (assoc-string prefix keys)))
         (plaintext
          (douban--chromium-aes-cbc-decrypt
           key (substring encrypted-value 3))))
    (when (>= database-version 24)
      (let ((domain-hash
             (secure-hash
              'sha256
              (encode-coding-string host-key 'utf-8 t)
              nil nil t)))
        (unless (and
                 (>= (length plaintext) (length domain-hash))
                 (string-prefix-p domain-hash plaintext))
          (error
           "douban: Chromium Cookie 的 domain binding 校验失败"))
        (setq plaintext
              (substring plaintext (length domain-hash)))))
    (decode-coding-string plaintext 'utf-8 t)))

(defun douban--chromium-time-to-unix (value)
  "把 Chromium 微秒时间 VALUE 转成 Unix 秒。"
  (/ (- value douban--chromium-time-epoch-offset) 1000000.0))

(defun douban--select-chromium-cookies (db table url spec)
  "从 DB 的可信 TABLE 读取并解密适用于 URL 的 Chromium Cookie。"
  (pcase-let* ((`(,host ,request-path ,request-secure)
                 (douban--cookie-url-parts url))
               (domains (douban--cookie-domain-candidates host))
               (placeholders
                (mapconcat (lambda (_domain) "?") domains ","))
               (version-row
                (car
                 (sqlite-select
                  db
                  (concat
                   "SELECT value FROM " table ".meta "
                   "WHERE key = ?")
                  (list "version"))))
               (database-version
                (and version-row
                     (string-to-number (format "%s" (car version-row)))))
               (raw-rows
                (sqlite-select
                 db
                 (concat
                  "SELECT host_key, name, value, encrypted_value, path, "
                  "expires_utc, is_secure, has_expires, creation_utc "
                  "FROM " table ".cookies "
                  "WHERE top_frame_site_key = ? AND host_key IN ("
                  placeholders ")")
                 (cons "" domains)))
               ;; Apply browser policy before touching encrypted values.
               ;; An expired or wrong-path record with a newer prefix must
               ;; not abort an otherwise valid request.
               (rows
                (cl-remove-if-not
                 (lambda (row)
                   (let* ((host-key (nth 0 row))
                          (cookie-path (or (nth 4 row) "/"))
                          (expires-utc (nth 5 row))
                          (cookie-secure
                           (not (zerop (or (nth 6 row) 0))))
                          (has-expires
                           (not (zerop (or (nth 7 row) 0))))
                          (expires
                           (and has-expires
                                (numberp expires-utc)
                                (douban--chromium-time-to-unix
                                 expires-utc))))
                     (and
                      (douban--cookie-domain-matches-p host-key host)
                      (douban--cookie-path-matches-p
                       cookie-path request-path)
                      (or (not cookie-secure) request-secure)
                      (or (not has-expires)
                          (and expires (> expires (float-time)))))))
                 raw-rows)))
    (unless (and (integerp database-version) (> database-version 0))
      (error "douban: Chromium Cookie 数据库缺少有效 schema version"))
    (let ((prefixes
           (delete-dups
            (delq nil
                  (mapcar
                   (lambda (row)
                     (let ((plain (nth 2 row))
                           (encrypted (nth 3 row)))
                       (and (string-empty-p (or plain ""))
                            (stringp encrypted)
                            (>= (length encrypted) 3)
                            (substring encrypted 0 3))))
                   rows))))
          keys records)
      (dolist (prefix prefixes)
        (unless (member prefix '("v10" "v11"))
          (error "douban: 不支持 Chromium Cookie 加密格式 %s"
                 prefix)))
      (unwind-protect
          (progn
            (dolist (prefix prefixes)
              (push
               (cons
                prefix
                (if (string-equal prefix "v10")
                    (douban--chromium-v10-key)
                  (douban--chromium-v11-key spec)))
               keys))
            (dolist (row rows)
              (let* ((host-key (nth 0 row))
                     (name (nth 1 row))
                     (plain-value (nth 2 row))
                     (encrypted-value (nth 3 row))
                     (expires (nth 5 row))
                     (has-expires (not (zerop (or (nth 7 row) 0))))
                     value)
                (setq value
                      (cond
                       ((not (string-empty-p (or plain-value "")))
                        plain-value)
                       ((not (string-empty-p (or encrypted-value "")))
                        (douban--chromium-decrypt-cookie
                         encrypted-value host-key database-version keys))
                       (t "")))
                (push
                 (douban--make-cookie-record
                  :name name
                  :value value
                  :domain host-key
                  :path (or (nth 4 row) "/")
                  :expires
                  (and has-expires (numberp expires)
                       (douban--chromium-time-to-unix expires))
                  :secure (not (zerop (or (nth 6 row) 0)))
                  :creation (nth 8 row))
                 records)))
            (douban--cookie-records-for-url records url))
        (dolist (entry keys)
          (when (stringp (cdr entry))
            (clear-string (cdr entry))))))))

(defun douban--read-chromium-cookies (path url browser)
  "从 BROWSER 的 Chromium Cookie 数据库 PATH 读取 URL 的 Cookie。"
  (let ((spec (douban--chromium-browser-spec browser)))
    (unless (file-readable-p path)
      (error "douban: %s Cookie 数据库不可读：%s" browser path))
    (douban--query-cookie-database
     path (format "%s Cookie" browser)
     (lambda (db schema)
       (douban--select-chromium-cookies db schema url spec)))))

(defun douban--read-browser-cookies (url)
  "从显式配置的浏览器 profile 读取适用于完整 URL 的 Cookie。"
  (unless (eq system-type 'gnu/linux)
    (error "douban: 浏览器 Cookie 读取只支持 GNU/Linux"))
  (let* ((browser douban-cookie-browser)
         (path (douban--cookie-store-file browser)))
    (pcase browser
      ('firefox
       (douban--read-firefox-cookies path url))
      ((or 'chromium 'chrome)
       (douban--read-chromium-cookies path url browser)))))

(defun douban--cookie-put (cookies name value)
  "返回更新后的 COOKIES，其中 NAME 只出现一次且值为 VALUE。"
  (cons (cons name value)
        (cl-remove-if
         (lambda (cookie)
           (string-equal (car cookie) name))
         cookies)))

(defun douban--cookie-header (cookies)
  "把 COOKIES 关联列表格式化为 HTTP Cookie 头字段。"
  (when cookies
    (mapconcat
     (lambda (cookie)
       (let ((value (cdr cookie)))
         (when (and
                (>= (length value) 2)
                (string-prefix-p "\"" value)
                (string-suffix-p "\"" value))
           (setq value (substring value 1 -1)))
         (format "%s=%s" (car cookie) value)))
     cookies
     "; ")))

;;;; HTTP

(defconst douban--request-timeout 30
  "单次豆瓣 HTTP 请求的超时秒数。")

(defun douban--douban-host-p (host)
  "HOST 属于 douban.com 时返回非 nil。"
  (and host
       (or (string-equal host "douban.com")
           (string-suffix-p ".douban.com" host t))))

(defun douban--https-douban-url-p (url)
  "URL 是 douban.com 域下的 HTTPS URL 时返回非 nil。"
  (when (stringp url)
    (let ((parsed (url-generic-parse-url url)))
      (and
       (string-equal (url-type parsed) "https")
       (null (url-user parsed))
       (null (url-password parsed))
       (or
        (null (url-port parsed))
        (= (url-port parsed) 443))
       (douban--douban-host-p
        (downcase (or (url-host parsed) "")))))))

(defun douban--http-url-p (url)
  "URL 是带 host 的绝对 HTTP(S) URL 时返回非 nil。"
  (when
      (and
       (stringp url)
       (not (string-match-p "[ \t\r\n]" url))
       (not (string-search "\\" url)))
    (let ((parsed (url-generic-parse-url url)))
      (and
       (member (url-type parsed) '("http" "https"))
       (stringp (url-host parsed))
       (not (string-empty-p (url-host parsed)))
       (null (url-user parsed))
       (null (url-password parsed))))))

(defun douban--https-url-p (url)
  "URL 是格式正确的绝对 HTTPS URL 时返回非 nil。"
  (and
   (douban--http-url-p url)
   (string-equal
    (url-type (url-generic-parse-url url)) "https")))

(defun douban--url-host (url)
  "返回 URL 中经过规范化的 host。"
  (downcase (or (url-host (url-generic-parse-url url)) "")))

(defun douban--normalize-response-headers (headers)
  "把 plz 的 HEADERS 字段名统一为小写字符串。"
  (mapcar
   (lambda (header)
     (cons
      (downcase
       (if (symbolp (car header))
           (symbol-name (car header))
         (format "%s" (car header))))
      (cdr header)))
   headers))

(defun douban--curl-args-without-redirects ()
  "返回适合动态绑定的安全 `plz-curl-default-args'。
首个参数使用 `--disable'，因为 curl 只在该位置识别它；同时移除所有启用重定向
跟随的选项。"
  (cons
   "--disable"
   (cl-remove-if
    (lambda (argument)
      (member
       argument
       '("--disable" "--location" "-L" "--location-trusted")))
    plz-curl-default-args)))

(defconst douban--plz-filter-redirect-status 599
  "传给 `plz' 响应解析器的临时 HTTP 状态码。
`plz' 会跳过 301、302、303、307、308 响应头，即使调用方已经禁止 curl
跟随重定向。传输过滤器先把这些状态码改成这个非重定向状态，待 `plz'
通过公开的 response API 解析完毕后再还原。")

(defconst douban--http-response-status-line-regexp
  (concat
   "\\`HTTP/\\([0-9]+\\(?:\\.[0-9]+\\)?\\)"
   "[ \t]+\\([0-9][0-9][0-9]\\)"
   "\\(?:[ \t]+\\([^\r\n]*\\)\\)?\r?\n")
  "匹配 curl 输出中位于字符串开头的 HTTP 状态行。")

(defun douban--filtered-http-response-prefix (data)
  "若 DATA 已含完整的最终 HTTP 响应头，返回过滤结果。
返回值为 (OUTPUT REDIRECT-STATUS)，其中 OUTPUT 可交给 `plz' 解析；若最终
状态是重定向，OUTPUT 中的状态码暂时改为
`douban--plz-filter-redirect-status'，REDIRECT-STATUS 是原状态码，否则为
nil。代理的 CONNECT 响应和 1xx 中间响应保持原样并跳过。响应头尚不完整时
返回 nil。"
  (let ((offset 0)
        result)
    (while (and (not result) (< offset (length data)))
      (let ((response (substring data offset)))
        (if (not (string-match douban--http-response-status-line-regexp response))
            ;; 状态行仍可能横跨下一次 process filter 调用。
            (if (string-match-p "[\r\n]" response)
                ;; 完整的第一行并非 HTTP 状态行，让 plz 按原样报告错误。
                (setq result (list data nil))
              (setq offset (length data)))
          (let* ((status
                  (string-to-number (match-string 2 response)))
                 (reason (string-trim (or (match-string 3 response) "")))
                 (status-start (+ offset (match-beginning 2)))
                 (status-end (+ offset (match-end 2)))
                 (header-end
                  (string-match "\r?\n\r?\n" response)))
            (if (not header-end)
                (setq offset (length data))
              (let ((block-end (+ offset (match-end 0))))
                (cond
                 ((or
                   (and (= status 200)
                        (string-equal reason "Connection established"))
                   (<= 100 status 199))
                 (setq offset block-end))
                 ((memq status '(301 302 303 307 308))
                  (setq
                   result
                   (list
                    (concat
                     (substring data 0 status-start)
                     (number-to-string douban--plz-filter-redirect-status)
                     (substring data status-end))
                    status)))
                 (t
                  (setq result (list data nil))))))))))
    result))

(defun douban--insert-process-output (process output)
  "把 OUTPUT 追加到 PROCESS 的 buffer。
这是 `plz' 的公开 `:filter' 接口要求自定义过滤器承担的插入工作。"
  (when-let* ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output)))))

(defun douban--response-with-status (response status)
  "复制 plz RESPONSE，并把 HTTP 状态码替换为 STATUS。"
  (make-plz-response
   :version (plz-response-version response)
   :status status
   :headers (plz-response-headers response)
   :body (plz-response-body response)))

(cl-defun douban--plz-request
    (method url &key body headers (decode t))
  "通过 `plz' 同步请求 URL 并返回响应。
METHOD 是大写的方法字符串。BODY 是已经确定的二进制字节字符串或 nil，
HEADERS 是关联列表。DECODE 为 nil 时保留响应正文的原始字节。HTTP 错误中的
响应仍作为响应返回；传输错误则保留原始 `plz' condition。"
  (let ((plz-curl-default-args
         (douban--curl-args-without-redirects))
        pending-output
        response-prefix-complete-p
        redirect-status)
    (let ((response
           (condition-case err
               (plz
                (intern (downcase method))
                url
                :headers headers
                :body body
                :body-type 'binary
                :as 'response
                :decode decode
                :then 'sync
                :filter
                (lambda (process output)
                  (if response-prefix-complete-p
                      (douban--insert-process-output process output)
                    (setq pending-output
                          (concat (or pending-output "") output))
                    (when-let* ((filtered
                                (douban--filtered-http-response-prefix
                                 pending-output)))
                      (setq
                       response-prefix-complete-p t
                       redirect-status (cadr filtered))
                      (douban--insert-process-output process (car filtered))
                      (setq pending-output nil))))
                :connect-timeout douban--request-timeout
                :timeout douban--request-timeout)
             (plz-error
              (let ((data (cl-find-if #'plz-error-p (cdr err))))
                (if-let* ((error-response
                          (and data (plz-error-response data))))
                    error-response
                  (signal (car err) (cdr err))))))))
      (if redirect-status
          (douban--response-with-status response redirect-status)
        response))))

(defun douban--response-set-cookie (session headers)
  "把 HEADERS 中相关的 Set-Cookie 值合并进 SESSION。"
  (when session
    (let ((cookies (douban--session-cookies session)))
      (dolist (header headers)
        (when (and
               (string-equal (car header) "set-cookie")
               (string-match
                "\\`[ \t]*\\([^=; \t]+\\)=\\([^;]*\\)"
                (cdr header)))
          (let ((name (match-string 1 (cdr header)))
                (value (match-string 2 (cdr header))))
            (setq cookies
                  (if (or
                       (string-empty-p value)
                       (and
                        (string-equal name "ck")
                        (string-equal value "deleted")))
                      (cl-remove-if
                       (lambda (cookie)
                         (string-equal (car cookie) name))
                       cookies)
                    (douban--cookie-put cookies name value)))
            (when (string-equal name "ck")
              (setf
               (douban--session-ck session)
               (and
                (not (string-empty-p value))
                (not (string-equal value "deleted"))
                value))))))
      (setf (douban--session-cookies session) cookies))))

(cl-defun douban--http
    (method url
            &key body content-type extra-headers session cookies raw-body
            allow-redirect-response)
  "同步请求 URL 并返回响应 plist。
METHOD 是大写的方法字符串。BODY 可以是文本；RAW-BODY 为非 nil 时，也可以是
已经确定的单字节负载。SESSION 优先提供 Cookie 并接收响应中的 Cookie 更新；
没有 SESSION 时使用只读 COOKIES。请求通过 `plz' 发送且绝不跟随重定向。
默认遇到 3xx 响应便报错；ALLOW-REDIRECT-RESPONSE 为非 nil 时返回该响应，
以供显式校验。"
  (unless (douban--https-douban-url-p url)
    (error "douban: 拒绝向非 HTTPS 豆瓣 URL 发送请求：%s" url))
  (let* ((request-cookies
          (if session
              (douban--session-cookies session)
            cookies))
         (headers
          (append
           (when-let* ((cookie
                       (and request-cookies
                            (douban--cookie-header request-cookies))))
             (list (cons "Cookie" cookie)))
           (when content-type
             (list (cons "Content-Type" content-type)))
           `(("User-Agent" . ,douban-user-agent)
             ("Accept-Language" . "zh-CN,zh;q=0.9,en;q=0.8"))
           extra-headers)))
    (let* ((response
            (douban--plz-request
             method url
             :body
             (and
              body
              (if raw-body
                  body
                (encode-coding-string body 'utf-8 t)))
             :headers headers))
           (status (plz-response-status response))
           (headers
            (douban--normalize-response-headers
             (plz-response-headers response)))
           (body (plz-response-body response)))
      (when
          (and
           (<= 300 status 399)
           (not allow-redirect-response))
        (error
         (concat
          "douban: %s %s 返回 HTTP %s 重定向；"
          "为防止 Cookie 泄漏，未自动跟随。"
          "请在浏览器中检查登录或安全验证")
         method url status))
      (douban--response-set-cookie session headers)
      (list
       :status status
       :headers headers
       :body body))))

(defun douban--parse-json (text)
  "把非空 JSON TEXT 解析为 Emacs Lisp 值，解析失败则返回 nil。"
  (and
   (stringp text)
   (not (string-empty-p text))
   (condition-case nil
       (json-parse-string
        text
        :object-type 'plist
        :array-type 'list
        :null-object :json-null
        :false-object :json-false)
     (error nil))))

(cl-defun douban--http-json
    (method url
            &key body content-type extra-headers session cookies raw-body
            allow-redirect-response)
  "使用 METHOD 请求 URL，并把解析后的 `:json' 附加到响应中。
请求选项原样传给 `douban--http'。"
  (let* ((response
          (douban--http
           method url
           :body body
           :content-type content-type
           :extra-headers extra-headers
           :session session
           :cookies cookies
           :raw-body raw-body
           :allow-redirect-response allow-redirect-response))
         (json (douban--parse-json (plist-get response :body))))
    (append response (list :json json))))

(cl-defun douban--read-json-endpoint (url referer &key cookies)
  "读取豆瓣 JSON URL，并返回统一 HTTP 响应 plist。
REFERER 原样作为请求来源；COOKIES 非 nil 时只随本次请求发送。重定向不会被
跟随，但会作为响应返回，以便各端点保留自己的状态码错误说明。"
  (douban--http-json
   "GET" url
   :cookies cookies
   :extra-headers
   `(("Accept" . "application/json")
     ("Referer" . ,referer))
   :allow-redirect-response t))

(defun douban--response-detail (response)
  "从 HTTP RESPONSE 返回适合错误消息的简短正文。"
  (let ((json (plist-get response :json))
        (body (plist-get response :body)))
    (or
     (and
      (listp json)
      (cl-loop
       for field in '(:localized_message :msg :message)
       thereis
       (douban--value-string (plist-get json field))))
     (and
      (stringp body)
      (not (string-empty-p body))
      body)
     "空响应")))

(defun douban--read-html-page (url session label)
  "使用 SESSION 读取 URL，并返回 LABEL 所指 HTML 页面。"
  (let* ((response
          (douban--http
           "GET" url
           :session session
           :extra-headers
           '(("Accept" . "text/html,application/xhtml+xml"))))
         (status (plist-get response :status)))
    (unless (<= 200 status 299)
      (user-error
       "douban: 读取%s失败（HTTP %s）：%s"
       label status url))
    (plist-get response :body)))

(defun douban--signal-create-result-unknown (message)
  "以 MESSAGE 报告创建请求可能已经成功。"
  (signal 'douban-create-result-unknown (list message)))

(defun douban--create-request (request message)
  "调用一次不带参数的 REQUEST。
如果请求没有返回响应，便用 MESSAGE 报告创建结果不确定。调用方另行校验已经返回
的响应。"
  (condition-case err
      (funcall request)
    ((plz-error quit)
     (douban--signal-create-result-unknown
      (concat message "原错误：" (error-message-string err))))))

(cl-defun douban--content-mutation-request
    (session url body content-type headers
             &key create-p unknown-message)
  "使用 SESSION 向 URL 发送一次内容变更请求。
BODY、CONTENT-TYPE 与 HEADERS 原样传给 JSON HTTP 层。CREATE-P 非 nil
表示请求会创建远端对象：此时保留 3xx 响应供协议层判断，并把没有返回响应的
网络中断报告为结果不确定；UNKNOWN-MESSAGE 描述相应的人工核查方法。"
  (let ((request
         (lambda ()
           (douban--http-json
            "POST" url
            :body body
            :content-type content-type
            :extra-headers headers
            :session session
            :allow-redirect-response create-p))))
    (if create-p
        (douban--create-request request unknown-message)
      (funcall request))))

(defun douban--form-encode (fields)
  "把有序的 (NAME . VALUE) FIELDS 编码成 HTML 表单正文。"
  (mapconcat
   (lambda (field)
     (concat
      (url-hexify-string (format "%s" (car field)))
      "="
      (url-hexify-string (format "%s" (or (cdr field) "")))))
   fields
   "&"))

(defun douban--mutation-headers (session)
  "返回 SESSION 的同源变更请求头。"
  `(("X-Requested-With" . "XMLHttpRequest")
    ("Referer" . ,(douban--session-referer session))
    ("Origin" . ,(concat "https://" (douban--session-host session)))))

(defconst douban--ck-bootstrap-url "https://www.douban.com/mine/"
  "取得当前登录用户会话 `ck' 的地址。")

(defun douban--ck-value (value)
  "把 VALUE 规范化为有效的豆瓣 `ck'，无效时返回 nil。"
  (let ((value
         (douban--metadata-text "豆瓣会话 ck" value)))
    (and
     value
     (not (string-equal value "deleted"))
     value)))

(defun douban--browser-session (kind url &optional existing-ck)
  "按 URL 的浏览器 Cookie 构造 KIND 会话。
URL 同时绑定为请求来源，host 从 URL 解析。EXISTING-CK 非 nil 时覆盖
该 URL Cookie 中的同名值；否则采用浏览器数据库中的有效 `ck'。每次调用都
独立读取传入 URL 对应的 Cookie，不跨 URL 复用 Cookie jar。"
  (let* ((cookies (douban--read-browser-cookies url))
         (existing-ck (douban--ck-value existing-ck))
         (ck
          (or
           existing-ck
           (douban--ck-value
            (cdr (assoc-string "ck" cookies)))))
         (cookies
          (if existing-ck
              (douban--cookie-put cookies "ck" existing-ck)
            cookies)))
    (douban--make-session
     :kind kind
     :cookies cookies
     :ck ck
     :referer url
     :host (douban--url-host url))))

(defun douban--ensure-ck (session)
  "确保 SESSION 含有当前豆瓣会话的 `ck' 并返回它。
浏览器数据库没有保存会话 Cookie 时，只读取一次 `/mine/' 响应取得它。"
  (or
   (douban--ck-value (douban--session-ck session))
   (let* ((bootstrap
           (douban--browser-session
            'bootstrap douban--ck-bootstrap-url))
          (response
           (douban--http
            "GET" douban--ck-bootstrap-url
            :session bootstrap
            :allow-redirect-response t))
          (ck (douban--ck-value
               (douban--session-ck bootstrap))))
     (unless ck
       (user-error
        (concat
         "douban: 当前浏览器登录没有返回发布所需的会话 ck"
         "（HTTP %s）；请在所选 profile 重新登录豆瓣")
        (plist-get response :status)))
     (setf
      (douban--session-ck session) ck
      (douban--session-cookies session)
      (douban--cookie-put
       (douban--session-cookies session) "ck" ck))
     ck)))

(defun douban--cookie-session (kind url)
  "根据 URL 的 Cookie 为 KIND 构造直接发布会话。
浏览器数据库没有持久化 `ck' 时，按需从当前网页会话取得。"
  (let ((session (douban--browser-session kind url)))
    (douban--ensure-ck session)
    session))

;;;; Metadata normalization

(defun douban--value-string (value)
  "把字符串或整数 VALUE 规范化为非空字符串。"
  (let ((string
         (cond
          ((stringp value) (string-trim value))
          ((integerp value) (number-to-string value)))))
    (and string (not (string-empty-p string)) string)))

(defun douban--metadata-id (field value)
  "把 metadata 字段 FIELD 的 VALUE 规范化为正整数 ID 字符串或 nil。"
  (when value
    (let ((value (and (stringp value) (string-trim value))))
      (unless
          (and value
               (or (string-empty-p value)
                   (string-match-p "\\`[1-9][0-9]*\\'" value)))
        (error "douban: %s 必须是正整数字符串" field))
      (unless (string-empty-p value)
        value))))

(defun douban--metadata-text (field value)
  "把 metadata 字段 FIELD 的 VALUE 规范化为非空字符串或 nil。"
  (let ((text (and (stringp value) (string-trim value))))
    (cond
     ((null value) nil)
     ((and text (string-empty-p text)) nil)
     ((and text (not (string-match-p "[[:cntrl:]]" text))) text)
     (t (error "douban: %s 必须是单行字符串" field)))))

(defun douban--metadata-bool (field value)
  "把布尔型 metadata 字段 FIELD 的 VALUE 规范化为布尔值。"
  (let ((bool (and (stringp value)
                   (downcase (string-trim value)))))
    (cond
     ((null value) nil)
     ((equal bool "") nil)
     ((equal bool "false") nil)
     ((equal bool "true") t)
     (t (error "douban: %s 必须是 true 或 false" field)))))

(defconst douban--metadata-value-options
  '((:subject-type
     ("book")
     ("movie")
     ("tv" :annotation "剧集")
     ("music")
     ("game"))
    (:rating
     ("1")
     ("2")
     ("3")
     ("4")
     ("5"))
    (:spoiler
     ("true")
     ("false"))
    (:donate
     ("true" :annotation "接受赞赏")
     ("false" :annotation "不接受赞赏"))
    (:cannot-reply
     ("true" :annotation "禁止回复")
     ("false" :annotation "允许回复"))
    (:explanation-types
     ("ai-generated" :annotation "含 AI 生成内容" :protocol "A")
     ("fictional" :annotation "含虚构内容" :protocol "X")
     ("marketing" :annotation "含营销信息" :protocol "K")
     ("minor-safety"
      :annotation "含或影响未成年人身心健康信息"
      :protocol "M")
     ("public-affairs"
      :annotation "涉及时事、公共政策、社会事件"
      :protocol "P")
     ("personal-opinion" :annotation "个人观点仅供参考" :protocol "O")
     ("repost" :annotation "内容为转载，来源见正文" :protocol "R")
     ("none" :annotation "无需标注" :protocol ""))
    (:rtype
     ("review" :annotation "评测" :protocol "R")
     ("guide" :annotation "攻略" :protocol "G"))
    (:note-privacy
     ("public" :annotation "所有人可见" :protocol "P")
     ("friends" :annotation "仅朋友可见" :protocol "F"))
    (:annotation-privacy
     ("public" :annotation "公开" :protocol "public")
     ("private" :annotation "仅自己可见" :protocol "private")))
  "豆瓣源稿中可以补全的枚举值及其协议映射。
每个字段后的条目格式是 `(源稿值 . 属性列表)'。候选具有歧义、缩写或豆瓣
特有语义时才设置 `:annotation'；`:protocol' 只在构造远端请求时使用。")

(defun douban--metadata-options-for-field (field)
  "返回 metadata FIELD 的枚举值条目。"
  (cdr (assq field douban--metadata-value-options)))

(defun douban--metadata-enum (field value &optional optional)
  "规范化枚举型 metadata FIELD 的 VALUE。
FIELD 必须出现在 `douban--metadata-value-options' 中。OPTIONAL 非 nil 时，
空值返回 nil；否则空值和未知值都报错。"
  (let* ((name (substring (symbol-name field) 1))
         (text (douban--metadata-text name value))
         (options (douban--metadata-options-for-field field)))
    (unless options
      (error "douban: %s 没有枚举值规范" name))
    (cond
     ((and optional (null text)) nil)
     ((assoc-string text options) text)
     (t
      (error
       "douban: %s 必须是 %s"
       name
       (mapconcat
        (lambda (option) (car option))
        options "、"))))))

(defun douban--metadata-protocol-value (field value)
  "把 FIELD 的规范源稿 VALUE 转换为豆瓣协议值。"
  (when value
    (let ((option
           (assoc-string
            value (douban--metadata-options-for-field field))))
      (unless option
        (error
         "douban: %s 含有未规范化的值 %S"
         (substring (symbol-name field) 1) value))
      (or (plist-get (cdr option) :protocol) value))))

(defun douban--metadata-rating (value)
  "把长评评分 VALUE 规范化为 1 至 5 的整数或 nil。"
  (let ((rating (and (stringp value) (string-trim value))))
    (cond
     ((null value) nil)
     ((equal rating "") nil)
     ((and rating (string-match-p "\\`[1-5]\\'" rating))
      (string-to-number rating))
     (t (error "douban: rating 必须是 1 到 5 的整数")))))

(defun douban--metadata-list (field value)
  "把字段 FIELD 的 VALUE 规范化为非空字符串列表。"
  (cond
   ((null value) nil)
   ((stringp value)
    (douban--metadata-list
     field
     (split-string value "[ \t]*,[ \t]*" t)))
   ((vectorp value)
    (douban--metadata-list field (append value nil)))
   ((listp value)
    (mapcar
     (lambda (item)
       (let ((normalized (douban--metadata-text field item)))
         (unless (and normalized
                      (not (string-search "," normalized)))
           (error "douban: %s 的元素必须非空且不能包含逗号" field))
         normalized))
     value))
   (t (error "douban: %s 必须是字符串列表" field))))

(defconst douban--kind-specs
  '((review
     :description "长评"
     :title-p t
     :fields
     ((:source :id
       :internal :review-id
       :codec id
       :description "长评 ID；初次发布省略，发布后自动写回"
       :nonempty-if-present t)
      (:source :subject-id
       :codec id
       :description "被评论的豆瓣条目 ID"
       :completion subject
       :required t)
      (:source :subject-type
       :codec enum
       :description "条目品类"
       :required t)
      (:source :introduction
       :codec text
       :description "长评导语")
      (:source :rating
       :codec rating)
      (:source :spoiler
       :codec boolean
       :description "是否包含剧透")
      (:source :donate
       :codec boolean
       :description "是否接受赞赏")
      (:source :explanation-types
       :codec enum
       :description "单项内容说明"
       :allow-empty t)
      (:source :rtype
       :codec enum
       :description "游戏长评类型"
       :allow-empty t
       :applicability (:subject-type . "game"))
      (:source :platforms
       :codec list
       :completion platform
       :applicability (:subject-type . "game"))))
    (note
     :description "日记"
     :title-p t
     :fields
     ((:source :id
       :internal :note-id
       :codec id
       :description "日记 ID；初次发布省略，创建页预分配后自动写回"
       :nonempty-if-present t)
      (:source :privacy
       :internal :note-privacy
       :codec enum
       :allow-empty t)
      (:source :cannot-reply
       :codec boolean
       :description "是否禁止回复")
      (:source :author-tags
       :codec list)))
    (annotation
     :description "读书笔记"
     :title-p t
     :fields
     ((:source :id
       :internal :annotation-id
       :codec id
       :description "新式读书笔记 topic ID；初次发布省略，发布后自动写回"
       :nonempty-if-present t)
      (:source :subject-id
       :codec id
       :description "笔记所属的豆瓣图书条目 ID"
       :completion subject
       :required t)
      (:source :privacy
       :internal :annotation-privacy
       :codec enum
       :description "读书笔记可见范围")
      (:source :explanation-types
       :codec enum
       :description "单项内容说明")))
    (status
     :description "普通广播"
     :title-p nil
     :fields
     ((:source :id
       :internal :status-id
       :codec id
       :description "广播 topic ID；初次发布省略，发布后自动写回"
       :nonempty-if-present t)
      (:source :explanation-types
       :codec enum
       :description "单项内容说明")
      (:source :anthology-id
       :codec id
       :description "收录文集"
       :completion anthology))))
  "每种受支持豆瓣源稿类型的 metadata 布局。
`:fields' 中的 descriptor 是字段名、顺序、codec、说明、补全和适用条件的
唯一事实源。`:required' 表示源稿必须提供字段；`:allow-empty' 表示枚举可
显式留空；`:nonempty-if-present' 表示字段可以省略，但出现时必须有值。
内部字段名默认等于 `:source'；静态补全默认由 codec 推导。")

(defun douban--kind-spec (kind)
  "返回 KIND 对应的 metadata 规范。"
  (cdr (assq kind douban--kind-specs)))

(defconst douban--metadata-source-kinds
  (mapcar #'car douban--kind-specs)
  "源稿 `douban' 映射支持的内容类型。")

(defun douban--metadata-source-kind-key (kind)
  "返回 KIND 在源稿 `douban' 映射中的 keyword key。"
  (intern (format ":%s" kind)))

(defun douban--metadata-field-descriptors (kind)
  "返回 KIND 的有序 metadata 字段 descriptor。"
  (plist-get (douban--kind-spec kind) :fields))

(defun douban--metadata-descriptor-internal-field (descriptor)
  "返回字段 DESCRIPTOR 的内部字段名，默认与源稿字段名相同。"
  (or
   (plist-get descriptor :internal)
   (plist-get descriptor :source)))

(defun douban--metadata-descriptor-completion (descriptor)
  "返回字段 DESCRIPTOR 的补全 provider。
显式 `:completion' 优先；枚举、布尔和评分 codec 默认使用静态候选。"
  (if (plist-member descriptor :completion)
      (plist-get descriptor :completion)
    (pcase (plist-get descriptor :codec)
      ((or 'enum 'boolean 'rating)
       'static))))

(defun douban--metadata-field-descriptor (kind internal-field)
  "返回 KIND 中 INTERNAL-FIELD 的字段 descriptor。"
  (cl-find
   internal-field
   (douban--metadata-field-descriptors kind)
   :key
   #'douban--metadata-descriptor-internal-field))

(defun douban--metadata-source-field-descriptor (kind source-field)
  "返回 KIND 中 SOURCE-FIELD 的字段 descriptor。"
  (cl-find
   source-field
   (douban--metadata-field-descriptors kind)
   :key
   (lambda (descriptor)
     (plist-get descriptor :source))))

(defun douban--metadata-source-field (kind internal-field)
  "返回 KIND 中 INTERNAL-FIELD 对应的源稿字段。"
  (when-let* ((descriptor
              (douban--metadata-field-descriptor
               kind internal-field)))
    (plist-get descriptor :source)))

(defun douban--metadata-internal-field (kind source-field)
  "返回 KIND 中 SOURCE-FIELD 对应的内部字段。"
  (when-let* ((descriptor
              (douban--metadata-source-field-descriptor
               kind source-field)))
    (douban--metadata-descriptor-internal-field
     descriptor)))

(defun douban--metadata-id-field (kind)
  "返回 KIND 的远端内容 ID 内部字段。"
  (or
   (douban--metadata-internal-field kind :id)
   (error "douban: %S metadata 规范缺少 id 字段" kind)))

(defun douban--normalize-metadata-field
    (kind internal-field value)
  "按照 KIND 中 INTERNAL-FIELD 的 codec 规范化 VALUE。"
  (let* ((descriptor
          (douban--metadata-field-descriptor
           kind internal-field))
         (source-field
          (and descriptor
               (plist-get descriptor :source)))
         (label
          (and source-field
               (substring (symbol-name source-field) 1)))
         (codec
          (and descriptor
               (plist-get descriptor :codec)))
         (normalized
          (pcase codec
            ('id
             (douban--metadata-id label value))
            ('text
             (douban--metadata-text label value))
            ('rating
             (douban--metadata-rating value))
            ('boolean
             (douban--metadata-bool label value))
            ('enum
             (douban--metadata-enum
              internal-field value
              (plist-get descriptor :allow-empty)))
            ('list
             (douban--metadata-list label value))
            (_
             (error
              "douban: %S metadata 字段 %S 没有有效 codec"
              kind internal-field)))))
    (when
        (and
         (plist-get descriptor :required)
         (null normalized))
      (error
       "douban: %s metadata 缺少 %s"
       kind label))
    normalized))

(defun douban--validate-source-mapping (value label)
  "要求 VALUE 是 LABEL 所指的 plist mapping。"
  (unless
      (or
       (null value)
       (and
        (listp value)
        (zerop (% (length value) 2))
        (cl-loop
         for (key _item) on value by #'cddr
         always (keywordp key))))
    (error "douban: %s 必须是 mapping" label))
  value)

(defun douban--metadata-kind (value)
  "根据 VALUE 中唯一的受支持子映射返回内容类型。"
  (douban--validate-source-mapping value "douban")
  (let (kinds)
    (cl-loop
     for (field _item) on value by #'cddr
     for kind = (intern (substring (symbol-name field) 1))
     do
     (unless (memq kind douban--metadata-source-kinds)
       (error "douban: metadata 不接受顶层字段 %s" field))
     (push kind kinds))
    (unless (= (length kinds) 1)
      (error
       (concat
        "douban: metadata 必须且只能包含 review、note、annotation "
        "或 status 中的一个")))
    (car kinds)))

(defun douban--metadata-source-to-internal (value kind)
  "把 VALUE 中 KIND 子映射转换为内部 metadata plist。"
  (let* ((kind-key (douban--metadata-source-kind-key kind))
         (source (plist-get value kind-key))
         (label (format "douban.%s" kind))
         (internal (list :kind kind)))
    (douban--validate-source-mapping source label)
    (cl-loop
     for (field item) on source by #'cddr
     for descriptor =
     (douban--metadata-source-field-descriptor kind field)
     for target = (and descriptor
                       (douban--metadata-descriptor-internal-field
                        descriptor))
     do
     (unless target
       (error "douban: %s 不接受字段 %s" label field))
     (setq internal (plist-put internal target item)))
    internal))

(defun douban--content-url-id (kind url)
  "返回标准 KIND URL 中的内容 ID，无法解析时返回 nil。"
  (when (stringp url)
    (pcase kind
      ('review
       (and
        (string-match
         (concat
          "\\`https://\\(?:www\\|book\\|movie\\|music\\)"
          "\\.douban\\.com/review/\\([1-9][0-9]*\\)/?\\'")
         url)
        (match-string 1 url)))
      ('note
       (and
        (string-match
         "\\`https://www\\.douban\\.com/note/\\([1-9][0-9]*\\)/?\\'"
         url)
        (match-string 1 url)))
      ('status
       (and
        (string-match
         (concat
          "\\`https://www\\.douban\\.com/people/"
          "[^/?#[:space:]]+/status/\\([1-9][0-9]*\\)/?\\'")
         url)
        (match-string 1 url)))
      ('annotation
       (and
        (string-match
         (concat
          "\\`https://www\\.douban\\.com/topic/\\([1-9][0-9]*\\)/?"
          "\\(?:[?#].*\\)?\\'")
         url)
        (match-string 1 url))))))

(defun douban--content-result-from-url (kind value base)
  "以 BASE 为基准，把 VALUE 解析为 KIND 类型的内容结果。"
  (when (stringp value)
    (let* ((value (string-trim value))
           (url
            (unless (string-empty-p value)
              (url-expand-file-name value base)))
           (id (douban--content-url-id kind url)))
      (when id
        (list :id id :url url)))))

(defun douban--normalize-meta-plist (value title)
  "把内部 metadata VALUE 和文档标题 TITLE 规范化为内容 plist。"
  (let* ((kind (plist-get value :kind))
         (spec (douban--kind-spec kind)))
    (unless spec
      (error "douban: 不支持的 metadata 类型 %S" kind))
    (let ((meta (list :kind kind)))
      (when (plist-get spec :title-p)
        (when-let* ((normalized-title
                    (douban--metadata-text
                     "title" title)))
          (setq
           meta
           (plist-put
            meta :title normalized-title))))
      (dolist
          (descriptor
           (douban--metadata-field-descriptors kind))
        (let* ((internal-field
                (douban--metadata-descriptor-internal-field
                 descriptor))
               (present-p
                (plist-member value internal-field)))
          (when
              (or
               present-p
               (plist-get descriptor :required))
            (let ((normalized
                   (douban--normalize-metadata-field
                    kind internal-field
                    (plist-get value internal-field))))
              (when
                  (and
                   present-p
                   (plist-get
                    descriptor :nonempty-if-present)
                   (null normalized))
                (error
                 (concat
                  "douban: %s.%s 出现时不能为空；"
                  "初次发布请省略该字段")
                 kind
                 (substring
                  (symbol-name
                   (plist-get descriptor :source))
                  1)))
              (setq
               meta
               (plist-put
                meta internal-field normalized))))))
      (when
          (and
           (eq kind 'review)
           (plist-get meta :introduction)
           (> (douban--utf16-length
               (plist-get meta :introduction))
              140))
        (error "douban: introduction 不能超过 140 字"))
      (dolist
          (descriptor
           (douban--metadata-field-descriptors kind))
        (when-let* ((applicability
                    (plist-get descriptor :applicability)))
          (let ((internal-field
                 (douban--metadata-descriptor-internal-field
                  descriptor)))
            (when
                (and
                 (plist-get meta internal-field)
                 (not
                  (equal
                   (plist-get meta (car applicability))
                   (cdr applicability))))
              (error
               "douban: %s.%s 只适用于 %s 为 %S"
               kind
               (substring
                (symbol-name
                 (plist-get descriptor :source))
                1)
               (substring
                (symbol-name (car applicability))
                1)
               (cdr applicability))))))
      meta)))

(defun douban--meta-from-plist (value title)
  "把嵌套源稿 metadata VALUE 和文档标题 TITLE 规范化为内容 plist。"
  (let ((kind (douban--metadata-kind value)))
    (douban--normalize-meta-plist
     (douban--metadata-source-to-internal value kind)
     title)))

;;;; Source metadata

(declare-function yaml-parse-string "yaml")

(defun douban--file-format (file)
  "返回 FILE 的源稿格式符号；无法识别时返回 nil。"
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "md" "markdown") 'markdown)
    (_ nil)))

(defun douban--require-source-format (file)
  "返回 FILE 的源稿格式，不受支持时发出用户错误。"
  (or (douban--file-format file)
      (user-error "douban: 不支持的文件类型 %s" file)))

(defun douban--write-current-buffer-atomically (file)
  "以原子方式用当前缓冲区内容替换 FILE。
保留检测到的编码系统、换行格式和权限位。若 FILE 是符号链接，则替换其
解析后的目标文件并保留链接本身。FILE 可以是本地文件，也可以由 TRAMP
处理。"
  (let* ((expanded (expand-file-name file))
         (target
          (if (file-symlink-p expanded)
              (file-truename expanded)
            expanded)))
    (unless (and (file-regular-p target)
                 (file-writable-p target))
      (error "douban: metadata 写回目标必须是可写普通文件：%s" target))
    (let* ((directory (file-name-directory target))
           (mode (file-modes target))
           (coding
            (or buffer-file-coding-system 'utf-8-unix))
           (temporary
            (make-temp-file
             (expand-file-name ".douban-write-" directory))))
      (unwind-protect
          (progn
            (let ((coding-system-for-write coding))
              (write-region
               (point-min) (point-max) temporary nil 'silent))
            (when mode (set-file-modes temporary mode))
            (rename-file temporary target t)
            (setq temporary nil))
        (when temporary
          (ignore-errors (delete-file temporary)))))))

(defun douban--shell-convert (program arguments input)
  "以 INPUT 作为标准输入，用 ARGUMENTS 运行 PROGRAM，并返回标准输出。"
  (let ((stderr-file (make-temp-file "douban-stderr-"))
        (stdout-buffer (generate-new-buffer " *douban-stdout*")))
    (unwind-protect
        (with-temp-buffer
          (insert input)
          (let ((coding-system-for-write 'utf-8)
                (coding-system-for-read 'utf-8)
                (status
                 (apply
                  #'call-process-region
                  (point-min) (point-max)
                  program nil
                  (list stdout-buffer stderr-file)
                  nil arguments)))
            (if (eq status 0)
                (with-current-buffer stdout-buffer
                  (buffer-string))
              (let ((detail
                     (with-temp-buffer
                       (insert-file-contents stderr-file)
                       (string-trim (buffer-string)))))
                (error
                 "douban: %s 退出 %s%s"
                 program status
                 (if (string-empty-p detail)
                     ""
                   (concat "：" detail)))))))
      (when (buffer-live-p stdout-buffer)
        (kill-buffer stdout-buffer))
      (ignore-errors (delete-file stderr-file)))))

(defun douban--metadata-entries (meta)
  "从 META 返回按固定顺序排列的源文件子映射条目。
每个条目是 `(字段-DESCRIPTOR 值)'；nil 值不会写入。"
  (let ((kind (plist-get meta :kind)))
    (cl-loop
     for descriptor in
     (douban--metadata-field-descriptors kind)
     for internal-field =
     (douban--metadata-descriptor-internal-field
      descriptor)
     for value = (plist-get meta internal-field)
     when value
     collect (list descriptor value))))

;; Markdown front matter

(defconst douban--md-douban-key-regexp
  "^douban[ \t]*:[^\n]*"
  "规范的顶层 YAML `douban' 键。")

(defun douban--md-parse-frontmatter (frontmatter)
  "解析 Markdown FRONTMATTER，并为解析错误添加来源上下文。"
  (unless (require 'yaml nil t)
    (error "douban: 读取 Markdown metadata 需要 yaml.el"))
  (condition-case err
      (yaml-parse-string
       frontmatter
       :object-type 'plist
       :sequence-type 'array
       :string-values t)
    (error
     (error
      "douban: Markdown front matter 解析失败：%s"
      (error-message-string err)))))

(defun douban--md-split-frontmatter (text)
  "将 TEXT 拆分为 (FRONTMATTER . BODY)。"
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (if (not (looking-at "---[ \t]*\n"))
        (cons nil text)
      (goto-char (match-end 0))
      (let ((frontmatter-begin (point))
            frontmatter-end
            body-begin)
        (while (and (not body-begin) (not (eobp)))
          (if (looking-at "---[ \t]*$")
              (progn
                (setq frontmatter-end (point))
                (forward-line 1)
                (setq body-begin (point)))
            (forward-line 1)))
        (if (not body-begin)
            (cons nil text)
          (when (and
                 (> frontmatter-end frontmatter-begin)
                 (eq (char-before frontmatter-end) ?\n))
            (setq frontmatter-end (1- frontmatter-end)))
          (cons
           (buffer-substring-no-properties
            frontmatter-begin frontmatter-end)
           (buffer-substring-no-properties
            body-begin (point-max))))))))

(defun douban--md-frontmatter-meta (frontmatter)
  "从 Markdown FRONTMATTER 解析豆瓣元数据。"
  (when frontmatter
    (let* ((parsed (douban--md-parse-frontmatter frontmatter))
           (writable-douban-region
            (douban--md-douban-region frontmatter))
           (value (and (listp parsed) (plist-get parsed :douban)))
           (title (and (listp parsed) (plist-get parsed :title))))
      (when (and (listp parsed) (plist-member parsed :douban))
        ;; YAML 接受加引号的 key，但写回时只替换字面形式的顶层
        ;; `douban:' 块，因此拒绝只能解析、不能安全写回的形式。
        (unless writable-douban-region
          (error
           "douban: Markdown metadata 的 key 必须直接写成未加引号的顶层 douban:"))
        (unless (listp value)
          (error
           "douban: Markdown front matter 的 douban 必须是 mapping"))
        (douban--meta-from-plist value title)))))

(defun douban--md-meta (file)
  "从 Markdown FILE 读取规范化的豆瓣元数据。"
  (let* ((text
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string)))
         (frontmatter (car (douban--md-split-frontmatter text))))
    (douban--md-frontmatter-meta frontmatter)))

(defun douban--md-douban-region (frontmatter)
  "返回 FRONTMATTER 顶层 `douban' 映射的零基索引区间。"
  (with-temp-buffer
    (insert frontmatter)
    (goto-char (point-min))
    (when
        (re-search-forward
         douban--md-douban-key-regexp nil t)
      (let ((begin (match-beginning 0))
            end)
        (goto-char begin)
        (forward-line 1)
        (setq end (point))
        (while (< (point) (point-max))
          (cond
           ((looking-at "[ \t]*#")
            (forward-line 1))
           ((looking-at "[ \t]+")
            (forward-line 1)
            (setq end (point)))
           ((looking-at "[ \t]*\n")
            (forward-line 1))
           (t (goto-char (point-max)))))
        (cons (1- begin) (1- end))))))

(defun douban--yaml-string (value)
  "将字符串 VALUE 序列化为 YAML 单引号标量。
单引号会保留反斜杠的字面值；yaml.el 的双引号解析器无法让所有 JSON
风格的转义完整往返。"
  (concat
   "'"
   (replace-regexp-in-string "'" "''" value t t)
   "'"))

(defun douban--metadata-yaml-field-lines (descriptor value)
  "按照 DESCRIPTOR 的 codec 把 VALUE 格式化为 YAML 字段行。"
  (let ((name
         (substring
          (symbol-name
           (plist-get descriptor :source))
          1)))
    (pcase (plist-get descriptor :codec)
      ('rating
       (list (format "    %s: %d" name value)))
      ('boolean
       (list
        (format
         "    %s: %s"
         name
         (if (eq value :json-false) "false" "true"))))
      ('list
       (cons
        (format "    %s:" name)
        (mapcar
         (lambda (item)
           (format
            "      - %s"
            (douban--yaml-string item)))
         value)))
      ('enum
       (list
        (format "    %s: %s" name value)))
      ((or 'id 'text)
       (list
        (format
         "    %s: %s"
         name
         (douban--yaml-string value))))
      (_
       (error
        "douban: metadata 字段 %S 没有可序列化的 codec"
        (douban--metadata-descriptor-internal-field
         descriptor))))))

(defun douban--format-yaml-meta (meta)
  "将 META 格式化为 Markdown 的嵌套 `douban' 映射。"
  (let* ((kind (plist-get meta :kind))
         (entries (douban--metadata-entries meta))
         lines)
    (push "douban:" lines)
    (push
     (if entries
         (format "  %s:" kind)
       (format "  %s: {}" kind))
     lines)
    (dolist (entry entries)
      (pcase-let ((`(,descriptor ,value) entry))
        (dolist
            (line
             (douban--metadata-yaml-field-lines
              descriptor value))
          (push line lines))))
    (concat
     (mapconcat #'identity (nreverse lines) "\n")
     "\n")))

(defun douban--md-write-meta (file meta)
  "把 META 写入 Markdown FILE 的顶层 `douban' 映射。"
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((text (buffer-string))
           (split (douban--md-split-frontmatter text))
           (frontmatter (car split))
           (body (cdr split))
           (new-douban (douban--format-yaml-meta meta))
           (region
            (and
             frontmatter
             (douban--md-douban-region frontmatter))))
      (cond
       ((null frontmatter)
        (erase-buffer)
        (insert "---\n" new-douban "---\n" body))
       (region
        (let ((new-frontmatter
               (concat
                (substring frontmatter 0 (car region))
                new-douban
                (substring frontmatter (cdr region)))))
          (erase-buffer)
          (insert "---\n" new-frontmatter "\n---\n" body)))
       (t
        (erase-buffer)
        (insert
         "---\n" frontmatter "\n" new-douban "---\n" body))))
    (douban--write-current-buffer-atomically file)))

;; Metadata dispatch

(defun douban--read-meta (file)
  "从 FILE 读取规范化的豆瓣元数据。"
  (when (eq (douban--file-format file) 'markdown)
    (douban--md-meta file)))

(defun douban--write-meta (file meta)
  "将规范化的 META 写回 FILE。"
  (douban--require-source-format file)
  (douban--md-write-meta file meta))

(defun douban--current-source ()
  "保存当前源稿，并返回 `(FILE META)'。"
  (unless buffer-file-name
    (user-error "douban: 当前 buffer 没有源稿文件"))
  (when (buffer-modified-p)
    (save-buffer))
  (let ((file buffer-file-name))
    (douban--require-source-format file)
    (list
     file
     (or
      (douban--read-meta file)
      (user-error
       "douban: %s 缺少 douban metadata"
       (file-name-nondirectory file))))))

;;;; Source conversion

(defun douban--parse-html (html)
  "使用 libxml 解析 HTML，并返回其 DOM。"
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defconst douban--highlight-block-marker-attribute
  "data-douban-highlight-block"
  "Pandoc HTML 中标记原生块高亮的私有属性。")

(defconst douban--pandoc-highlight-block-filter
  (concat
   "local marker = '" douban--highlight-block-marker-attribute "'\n\n"
   "local function full_mark(block)\n"
   "  if block.t ~= 'Para' or #block.content ~= 1 then\n"
   "    return nil\n"
   "  end\n"
   "  local inline = block.content[1]\n"
   "  if inline.t ~= 'Span'\n"
   "      or inline.identifier ~= ''\n"
   "      or #inline.classes ~= 1\n"
   "      or inline.classes[1] ~= 'mark' then\n"
   "    return nil\n"
   "  end\n"
   "  for _, _ in pairs(inline.attributes) do\n"
   "    return nil\n"
   "  end\n"
   "  return inline.content\n"
   "end\n\n"
   "function Pandoc(doc)\n"
   "  local output = pandoc.List()\n"
   "  for _, block in ipairs(doc.blocks) do\n"
   "    local content = full_mark(block)\n"
   "    if content == nil then\n"
   "      output:insert(block)\n"
   "    else\n"
   "      output:insert(pandoc.Div(\n"
   "        {pandoc.Para(content)},\n"
   "        pandoc.Attr('', {}, {[marker] = 'true'})\n"
   "      ))\n"
   "    end\n"
   "  end\n"
   "  doc.blocks = output\n"
   "  return doc\n"
   "end\n")
  "把 Markdown 的完整顶层 mark 段落标记为原生块高亮。")

(defconst douban--toc-marker-attribute
  "data-douban-generated-toc-depth"
  "Pandoc HTML 中标记自动目录位置和深度的私有属性。")

(defun douban--metadata-toc-depth (value)
  "把 Markdown 顶层 `toc-depth' VALUE 规范化为 1 至 6。"
  (let ((text (and (stringp value) (string-trim value))))
    (unless (and text (string-match-p "\\`[1-6]\\'" text))
      (error "douban: Markdown toc-depth 必须是 1 到 6 的整数"))
    (string-to-number text)))

(defun douban--md-toc-depth (frontmatter)
  "返回 Markdown FRONTMATTER 请求的目录深度，否则返回 nil。
`toc: true' 默认使用 Pandoc 的三级目录深度；可用顶层 `toc-depth'
把深度设置为 1 至 6。"
  (when frontmatter
    (let* ((parsed (douban--md-parse-frontmatter frontmatter))
           (toc-present (and (listp parsed) (plist-member parsed :toc)))
           (toc-value (and toc-present (plist-get parsed :toc)))
           (toc
            (and
             toc-present
             (let ((text
                    (and
                     (stringp toc-value)
                     (downcase (string-trim toc-value)))))
               (cond
                ((equal text "true") t)
                ((equal text "false") nil)
                (t
                 (error
                  "douban: Markdown 顶层 toc 必须是 true 或 false"))))))
           (depth-present
            (and (listp parsed) (plist-member parsed :toc-depth)))
           (depth
            (and
             depth-present
             (douban--metadata-toc-depth
              (plist-get parsed :toc-depth)))))
      (and toc (or depth 3)))))

(defun douban--pandoc-to-html (from input)
  "将 INPUT 从 Pandoc 的 FROM 格式转换为 HTML 片段。"
  (let (filters)
    (unwind-protect
        (progn
          (when (string-prefix-p "markdown" from)
            (push
             (make-temp-file
              "douban-highlight-block-" nil ".lua"
              douban--pandoc-highlight-block-filter)
             filters))
          (setq filters (nreverse filters))
          (douban--shell-convert
           "pandoc"
           (append
            (list "-f" from "-t" "html5")
            (cl-mapcan
             (lambda (filter)
               (list "--lua-filter" filter))
             filters)
            (list "--wrap=none" "--no-highlight"))
           input))
      (dolist (filter filters)
        (ignore-errors (delete-file filter))))))

(defun douban--source-html (file)
  "将源稿 FILE 转换为 HTML 片段。"
  (douban--require-source-format file)
  (let* ((text
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string)))
         (split (douban--md-split-frontmatter text))
         (frontmatter (car split))
         (body (cdr split))
         (toc-depth (douban--md-toc-depth frontmatter))
         (html (douban--pandoc-to-html "markdown+mark" body)))
    (if toc-depth
        (format
         "<div %s=\"%d\"></div>\n%s"
         douban--toc-marker-attribute toc-depth html)
      html)))

(defun douban--current-buffer-meta ()
  "从当前未保存的源稿 buffer 读取规范化 metadata。"
  (unless buffer-file-name
    (user-error "douban: 当前 buffer 没有源稿文件"))
  (douban--require-source-format buffer-file-name)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((text
              (buffer-substring-no-properties
               (point-min) (point-max)))
             (frontmatter
              (car (douban--md-split-frontmatter text))))
        (or
         (douban--md-frontmatter-meta frontmatter)
         (user-error
          "douban: %s 缺少 douban metadata"
          (file-name-nondirectory buffer-file-name)))))))

;;;; User mentions

(defconst douban--user-search-endpoint
  "https://m.douban.com/rexxar/api/v2/search/user_complete"
  "当前豆瓣编辑器使用的登录态用户补全端点。")

(defconst douban--user-mention-title-prefix
  "douban-user-mention:"
  "源稿链接 title 中标识豆瓣用户 mention 的前缀。")

(defun douban--user-profile-url-p (value)
  "若 VALUE 是规范豆瓣用户主页 URL，则返回非 nil。"
  (and
   (stringp value)
   (string-match
    (concat
     "\\`https://www\\.douban\\.com/people/"
     "\\([A-Za-z0-9._~-]+\\)/\\'")
    value)
   (not
    (member
     (match-string 1 value)
     '("." "..")))))

(defun douban--search-users (query)
  "补全 QUERY 对应的已关注豆瓣用户，并返回规范用户 plist 列表。"
  (setq query (string-trim query))
  (when (string-empty-p query)
    (user-error "douban: 用户搜索词不能为空"))
  (let* ((session
          (douban--cookie-session
           'status douban--user-search-endpoint))
         (_referer
          (setf
           (douban--session-referer session)
           "https://www.douban.com/"))
         (url
          (format
           "%s?q=%s&start=0&count=10&ck=%s"
           douban--user-search-endpoint
           (url-hexify-string query)
           (url-hexify-string
            (douban--session-ck session))))
         (response
          (douban--http-json
           "GET" url
           :session session
           :extra-headers
           '(("Accept" . "application/json")
             ("Referer" . "https://www.douban.com/"))))
         (status (plist-get response :status))
         (json (plist-get response :json))
         (detail
          (and
           (listp json)
           (or
            (plist-get json :localized_message)
            (plist-get json :msg)
            (plist-get json :message)))))
    (unless (<= 200 status 299)
      (if (or (memq status '(401 403))
              (equal (plist-get json :code) 121))
          (user-error
           "douban: 用户搜索需要有效的豆瓣浏览器登录")
        (error
         "douban: 用户搜索失败（HTTP %s）%s"
         status
         (if detail (concat "：" detail) ""))))
    (unless (and (listp json)
                 (listp (plist-get json :users)))
      (error "douban: 用户搜索返回了无效 JSON"))
    (cl-loop
     for user in (plist-get json :users)
     for normalized =
     (condition-case nil
         (when
             (and
              (listp user)
              (eq (plist-get user :followed) t))
           (let ((id
                  (douban--value-string
                   (plist-get user :id)))
                 (name
                  (douban--metadata-text
                   "豆瓣用户名" (plist-get user :name)))
                 (profile-url (plist-get user :url)))
             (when
                 (and
                  id
                  (string-match-p "\\`[1-9][0-9]*\\'" id)
                  name
                  (douban--user-profile-url-p profile-url))
               (list
                :id id :name name :url profile-url))))
       (error nil))
     when normalized collect normalized)))

(defun douban--html-numeric-entities (text)
  "把 TEXT 的每个字符编码为 HTML 数字字符引用。"
  (mapconcat
   (lambda (character)
     (format "&#x%X;" character))
   text
   ""))

(defun douban--user-mention-source (user)
  "把 USER 格式化为 Markdown 持久化 mention 源标记。"
  (let* ((id (plist-get user :id))
         (name (plist-get user :name))
         (profile-url (plist-get user :url))
         (marker (concat douban--user-mention-title-prefix id))
         (text (concat "@" name))
         (anchor
          (format
           "<a href=\"%s\" title=\"%s\">%s</a>"
           (xml-escape-string profile-url)
           (xml-escape-string marker)
           (douban--html-numeric-entities text))))
    anchor))

(defun douban--user-completion-candidates (users)
  "把规范化 USERS 转为补全使用的 `(LABEL . USER)' 列表。"
  (mapcar
   (lambda (user)
     (cons
      (format
       "%s (%s)"
       (plist-get user :name)
       (plist-get user :id))
      user))
   users))

;;;###autoload
(defun douban-insert-user-mention (query)
  "补全 QUERY 对应的已关注豆瓣用户，并在光标处插入原生 mention。
长评、日记和普通广播都使用 Draft.js 原生 USER entity。"
  (interactive (list (read-string "补全已关注豆瓣用户: ")))
  (unless
      (and
       buffer-file-name
       (eq (douban--file-format buffer-file-name) 'markdown))
      (user-error "douban: 当前 buffer 不是受支持的 Markdown 源稿"))
  (douban--current-buffer-meta)
  (let* ((users (douban--search-users query))
         (candidates (douban--user-completion-candidates users)))
    (unless candidates
      (user-error "douban: 没有找到已关注用户：%s" query))
    (let* ((selected
            (completing-read
             "已关注豆瓣用户: " candidates nil t))
           (user (cdr (assoc selected candidates))))
      (insert (douban--user-mention-source user)))))

(defvar-local douban--user-mention-completion-cache nil
  "当前 buffer 中按查询前缀缓存的已关注用户补全结果。")

(defun douban--markdown-body-start ()
  "返回当前 Markdown buffer 的正文起点。
若开头存在未闭合 YAML front matter，则返回 `point-max'。"
  (save-excursion
    (goto-char (point-min))
    (if (not (looking-at "---[ \t]*\n"))
        (point-min)
      (goto-char (match-end 0))
      (if (re-search-forward "^---[ \t]*\r?$" nil t)
          (progn
            (forward-line 1)
            (point))
        (point-max)))))

(defun douban--markdown-mention-boundary-p (position)
  "POSITION 处的 `@' 可以开始正文 mention 时返回非 nil。"
  (or
   (= position (line-beginning-position))
   (let ((previous (char-before position)))
     (or
      (memq previous
            '(?\s ?\t ?\r ?\n ?\( ?\[ ?\{ ?\" ?\'))
      (and
       previous
       (> previous 127)
       (eq (char-syntax previous) ?.))))))

(defun douban--markdown-fenced-code-p (position body-start)
  "POSITION 位于 BODY-START 之后的 Markdown fenced code 时返回非 nil。"
  (save-excursion
    (let ((current-line
           (progn
             (goto-char position)
             (line-beginning-position)))
          fence-character
          fence-length)
      (goto-char body-start)
      (while (< (point) current-line)
        (when
            (looking-at
             " \\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)")
          (let* ((marker (match-string-no-properties 1))
                 (character (aref marker 0))
                 (length (length marker)))
            (cond
             ((null fence-character)
              (setq fence-character character
                    fence-length length))
             ((and
               (= character fence-character)
               (>= length fence-length))
              (setq fence-character nil
                    fence-length nil)))))
        (forward-line 1))
      (or
       fence-character
       (looking-at
        " \\{0,3\\}\\(?:`\\{3,\\}\\|~\\{3,\\}\\)")))))

(defun douban--markdown-inline-code-p (position)
  "POSITION 位于当前 Markdown 行的反引号 code span 时返回非 nil。"
  (save-excursion
    (let ((line-start
           (progn
             (goto-char position)
             (line-beginning-position)))
          delimiter-length)
      (goto-char line-start)
      (while (re-search-forward "`+" position t)
        (let ((escaped
               (save-excursion
                 (goto-char (match-beginning 0))
                 (let ((slashes 0))
                   (while (eq (char-before) ?\\)
                     (cl-incf slashes)
                     (backward-char))
                   (= (% slashes 2) 1))))
              (length (- (match-end 0) (match-beginning 0))))
          (unless escaped
            (cond
             ((null delimiter-length)
              (setq delimiter-length length))
             ((= delimiter-length length)
              (setq delimiter-length nil))))))
      delimiter-length)))

(defun douban--markdown-html-context-p (position body-start)
  "POSITION 位于 BODY-START 后的 raw HTML 上下文时返回非 nil。"
  (save-excursion
    (let ((case-fold-search t)
          (block-tags
           '(html body main article section div header footer aside nav
             p h1 h2 h3 h4 h5 h6 blockquote pre figure hr
             ul ol table dl details summary))
          (depth 0)
          inside-tag
          inside-comment)
      (goto-char body-start)
      (while
          (re-search-forward
           "<\\(/?\\)\\([[:alpha:]][[:alnum:]-]*\\)\\(?:[ \t\r\n][^>]*\\)?>"
           position t)
        (let ((closing (not (string-empty-p (match-string 1))))
              (tag (intern (downcase (match-string 2))))
              (self-closing
               (string-match-p
                "/[ \t\r\n]*>\\'"
                (match-string 0))))
          (when (and
                 (memq tag block-tags)
                 (not self-closing)
                 (not (memq tag '(hr))))
            (setq depth
                  (if closing
                      (max 0 (1- depth))
                    (1+ depth))))))
      (let* ((prefix
              (buffer-substring-no-properties
               body-start position))
             (last-open (string-match-p "<[^>]*\\'" prefix))
             (search-start 0)
             last-comment-open
             last-comment-close)
        (while (string-match "<!--" prefix search-start)
          (setq last-comment-open (match-beginning 0)
                search-start (match-end 0)))
        (setq search-start 0)
        (while (string-match "-->" prefix search-start)
          (setq last-comment-close (match-beginning 0)
                search-start (match-end 0)))
        (setq inside-tag last-open
              inside-comment
              (and
               last-comment-open
               (or
                (null last-comment-close)
                (> last-comment-open
                   last-comment-close)))))
      (or (> depth 0) inside-tag inside-comment))))

(defun douban--markdown-user-mention-bounds ()
  "返回光标前 Markdown `@QUERY' 的边界，边界包含 `@'。"
  (when
      (and
       buffer-file-name
       (eq (douban--file-format buffer-file-name) 'markdown))
    (let ((end (point)))
      (save-excursion
        (skip-chars-backward "^@ \t\r\n")
        (when
            (and
             (< (point) end)
             (eq (char-before) ?@))
          (let* ((mention-start (1- (point)))
                 (body-start (douban--markdown-body-start)))
            (when
                (and
                 (>= mention-start body-start)
                 (douban--markdown-mention-boundary-p
                  mention-start)
                 (not
                  (douban--markdown-fenced-code-p
                   mention-start body-start))
                 (not
                  (douban--markdown-inline-code-p
                   mention-start))
                 (not
                  (douban--markdown-html-context-p
                   mention-start body-start)))
              (cons mention-start end))))))))

(defun douban--cached-user-completions (query)
  "返回 QUERY 的已关注用户补全，并在当前 buffer 中缓存。"
  (if-let* ((cached
            (assoc-string
             query douban--user-mention-completion-cache)))
      (cdr cached)
    (let ((users (douban--search-users query)))
      (push
       (cons query users)
       douban--user-mention-completion-cache)
      users)))

(defun douban-user-mention-completion-at-point ()
  "补全光标前的 Markdown `@QUERY' 为豆瓣原生用户 mention。
候选只包含当前豆瓣账号已经关注的用户。"
  (when-let* ((bounds (douban--markdown-user-mention-bounds)))
    (when
        (condition-case nil
            (progn
              (douban--current-buffer-meta)
              t)
          (error nil))
      (let* ((mention-start (car bounds))
             (start (1+ mention-start))
             (end (cdr bounds))
             (query
              (buffer-substring-no-properties start end)))
        (unless (string-empty-p query)
          (let* ((users
                  (douban--cached-user-completions query))
                 (candidates
                  (douban--user-completion-candidates users))
                 (start-marker
                  (copy-marker mention-start))
                 (end-marker
                  (copy-marker end t)))
            (list
             start end candidates
             :exclusive t
             :exit-function
             (lambda (candidate status)
               (when (memq status '(finished exact))
                 (unwind-protect
                     (when-let*
                          ((source-buffer
                           (marker-buffer start-marker))
                          (same-buffer
                           (eq source-buffer
                               (marker-buffer end-marker)))
                          (label
                           (substring-no-properties
                            candidate))
                          (user
                           (cdr
                            (assoc-string
                             label candidates))))
                       (with-current-buffer source-buffer
                         (save-excursion
                           (let ((source-start
                                  (marker-position
                                   start-marker))
                                 (source-end
                                  (marker-position
                                   end-marker)))
                             (when
                                 (and
                                  source-start source-end
                                  (< source-start source-end)
                                  (eq
                                   (char-after source-start)
                                   ?@))
                               (delete-region
                                source-start source-end)
                               (goto-char source-start)
                               (insert
                                (douban--user-mention-source
                                 user)))))))
                   (set-marker start-marker nil)
                   (set-marker end-marker nil)))))))))))

(defun douban--validate-content-draft (raw label)
  "验证 Draft RAW 正文非空，并返回字符数；LABEL 表示内容类型。"
  (let ((count (douban--draft-character-count raw))
        (occurrences
         (douban--draft-entity-occurrences raw)))
    (when
        (and
         (zerop count)
         (not
          (cl-some
           (lambda (occurrence)
             (equal
              (plist-get
               (plist-get occurrence :block)
               :type)
              "atomic"))
           occurrences)))
      (user-error "douban: %s正文不能为空" label))
    count))

(defun douban--draft-entity-occurrences (raw)
  "返回 RAW 中按 block/range 正文顺序排列的 entity occurrence。
每项是含 `:key'、`:entity'、`:block' 和 `:range' 的 plist。孤立在
entityMap 中而未被 range 引用的 entity 不会返回；range 引用不存在的
entity key 时立即报错。"
  (let ((entities (plist-get raw :entityMap))
        (missing (make-symbol "missing-entity"))
        occurrences)
    (dolist (block (append (plist-get raw :blocks) nil))
      (dolist (range (append (plist-get block :entityRanges) nil))
        (let* ((raw-key (plist-get range :key))
               (key
                (cond
                 ((and (integerp raw-key) (>= raw-key 0))
                  (number-to-string raw-key))
                 ((and
                   (stringp raw-key)
                   (string-match-p "\\`[0-9]+\\'" raw-key))
                  raw-key)
                 (t
                  (error
                   "douban: Draft block %s 含有无效的 entity key：%S"
                   (or (plist-get block :key) "<unknown>")
                   raw-key))))
               (entity
                (and
                 (hash-table-p entities)
                 (gethash key entities missing))))
          (when (or (not (hash-table-p entities))
                    (eq entity missing))
            (error
             "douban: Draft block %s 引用了不存在的 entity key %s"
             (or (plist-get block :key) "<unknown>") key))
          (push
           (list
            :key key
            :entity entity
            :block block
            :range range)
           occurrences))))
    (nreverse occurrences)))

(defun douban--draft-referenced-entities (raw)
  "返回 RAW 正文首次引用顺序中的 entity，并按 entity key 去重。"
  (let ((seen (make-hash-table :test 'equal))
        referenced)
    (dolist (occurrence (douban--draft-entity-occurrences raw))
      (let ((key (plist-get occurrence :key)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push (plist-get occurrence :entity) referenced))))
    (nreverse referenced)))

(defun douban--draft-has-entity-type-p (raw type)
  "若 Draft RAW 正文引用了 TYPE entity，则返回非 nil。"
  (cl-some
   (lambda (entity)
     (equal (plist-get entity :type) type))
   (douban--draft-referenced-entities raw)))

(defun douban--draft-has-image-p (raw)
  "若 Draft RAW 正文引用了 IMAGE entity，则返回非 nil。"
  (douban--draft-has-entity-type-p raw "IMAGE"))

;;;; Draft.js conversion

(defconst douban--block-tags
  '(p h1 h2 h3 h4 h5 h6 blockquote pre figure hr
      ul ol table dl)
  "会开启或包含 Draft.js 区块的 HTML 元素。")

(defconst douban--transparent-block-tags
  '(html body main article section div header footer aside nav)
  "其子元素应分别转换为独立区块的 HTML 容器。")

(defun douban--empty-object ()
  "返回一个新的 JSON 对象。"
  (make-hash-table :test 'equal))

(defun douban--new-draft (&optional toc-headings)
  "创建一个空的可变 Draft.js 文档。
TOC-HEADINGS 是当前 HTML 文档已经校验的自动目录标题记录。"
  (douban--make-draft
   :blocks nil
   :entities (make-hash-table :test 'equal)
   :next-entity 0
   :next-block 0
   :toc-headings toc-headings))

(defun douban--draft-key (draft)
  "在 DRAFT 中分配唯一的区块键。"
  (let ((index (douban--draft-next-block draft)))
    (setf (douban--draft-next-block draft) (1+ index))
    (format "%05x" index)))

(defun douban--draft-add-block
    (draft type &optional depth data)
  "向 DRAFT 追加一个 TYPE 区块并返回该区块。"
  (let ((block
         (douban--make-block
          :key (douban--draft-key draft)
          :type type
          :depth (or depth 0)
          :text ""
          :utf16-length 0
          :inline-ranges nil
          :entity-ranges nil
          :data (or data (douban--empty-object)))))
    (push block (douban--draft-blocks draft))
    block))

(defun douban--utf16-length (text)
  "返回 TEXT 的 JavaScript UTF-16 码元长度。"
  (cl-loop
   for character across text
   sum (if (> character #xffff) 2 1)))

(defun douban--block-offset (block)
  "返回 BLOCK 当前末尾的 UTF-16 偏移量。"
  (douban--block-utf16-length block))

(defun douban--block-write (block text)
  "向 BLOCK 追加 TEXT，并同步维护其 UTF-16 长度缓存。"
  (let ((new-text
         (concat (douban--block-text block) text))
        (new-length
         (+ (douban--block-utf16-length block)
            (douban--utf16-length text))))
    ;; `text' 和 `utf16-length' 是只读 slot，避免绕过这个唯一写入入口
    ;; 破坏二者的一致性。
    (setf
     (cl-struct-slot-value 'douban--block 'text block)
     new-text
     (cl-struct-slot-value 'douban--block 'utf16-length block)
     new-length))
  block)

(defun douban--draft-add-entity
    (draft type mutability data)
  "向 DRAFT 添加具有 TYPE、MUTABILITY 和 DATA 的 entity。
返回其数字键。"
  (let ((key (douban--draft-next-entity draft)))
    (puthash
     (number-to-string key)
     (list
      :type type
      :mutability mutability
      :data data)
     (douban--draft-entities draft))
    (setf (douban--draft-next-entity draft) (1+ key))
    key))

(defun douban--block-add-inline-range
    (block style offset length)
  "向 BLOCK 添加从 OFFSET 开始、长度为 LENGTH 的 STYLE 区间。"
  (when (> length 0)
    (push
     (list :offset offset :length length :style style)
     (douban--block-inline-ranges block))))

(defun douban--block-add-entity-range
    (block key offset length)
  "向 BLOCK 添加从 OFFSET 开始、长度为 LENGTH、键为 KEY 的 entity 区间。"
  (when (> length 0)
    (push
     (list :offset offset :length length :key key)
     (douban--block-entity-ranges block))))

(defun douban--dom-element-children (node)
  "返回 NODE 的直接元素子节点。"
  (cl-remove-if-not #'consp (dom-children node)))

(defun douban--dom-significant-children (node)
  "返回 NODE 的子节点，但排除纯空白文本。"
  (cl-remove-if
   (lambda (child)
     (and (stringp child)
          (string-match-p "\\`[ \t\r\n]*\\'" child)))
   (dom-children node)))

(defun douban--dom-has-descendant-p (node predicate)
  "若 NODE 的任意元素后代满足 PREDICATE，则返回非 nil。"
  (cl-some
   (lambda (child)
     (and
      (consp child)
      (or
       (funcall predicate child)
       (douban--dom-has-descendant-p child predicate))))
   (dom-children node)))

(defun douban--dom-whitespace-node-p (node)
  "若 NODE 是纯空白文本，则返回非 nil。"
  (and
   (stringp node)
   (string-match-p "\\`[ \t\r\n]*\\'" node)))

(defun douban--node-has-class-p (node class)
  "若 NODE 的 class 属性含有完整的 CLASS token，则返回非 nil。"
  (when-let* ((classes
              (and (consp node) (dom-attr node 'class))))
    (member class (split-string classes "[ \t\r\n]+" t))))

(defun douban--css-property-value (node property)
  "返回 NODE 的内联 CSS 中 PROPERTY 最后一次声明的值。
属性名不区分大小写；不存在该属性或 `style' 不是字符串时返回 nil。"
  (when-let* ((style
              (and
               (consp node)
               (stringp (dom-attr node 'style))
               (dom-attr node 'style))))
    (let ((case-fold-search t)
          value
          (pattern
           (concat
            "\\`[ \t\r\n]*"
            (regexp-quote property)
            "[ \t\r\n]*:[ \t\r\n]*"
            "\\(.*?\\)[ \t\r\n]*\\'")))
      (dolist (declaration (split-string style ";" t))
        (when (string-match pattern declaration)
          (setq value (match-string 1 declaration))))
      value)))

(defconst douban--document-reference-roles
  '("doc-biblioref" "doc-noteref" "doc-backlink"
    "doc-bibliography" "doc-endnotes" "doc-footnote")
  "不应当作为正文标题导航处理的 HTML 文档引用 role。")

(defun douban--document-reference-role-p (role)
  "若 ROLE 含有脚注或参考文献语义则返回非 nil。"
  (and
   (stringp role)
   (cl-some
    (lambda (token)
      (member token douban--document-reference-roles))
    (split-string role "[[:space:]]+" t))))

(defun douban--decode-fragment-id (fragment)
  "把 URL 中的 FRAGMENT 解码为 UTF-8 HTML id。
解码失败时返回原字符串，由调用方继续给出目标错误。"
  (condition-case nil
      (decode-coding-string (url-unhex-string fragment) 'utf-8 t)
    (error fragment)))

(defun douban--heading-level (tag)
  "返回 HTML 标题 TAG 的数字层级，否则返回 nil。"
  (pcase tag
    ('h1 1)
    ('h2 2)
    ('h3 3)
    ('h4 4)
    ('h5 5)
    ('h6 6)))

(defun douban--plain-heading-text (node)
  "返回纯文本标题 NODE 的精确可见文字，否则返回 nil。
豆瓣公开渲染器只会为没有行内元素的标题生成可依赖的文字 `id'。"
  (let ((children (dom-children node)))
    (when (cl-every #'stringp children)
      (let ((text (mapconcat #'identity children "")))
        (and
         (not (string-empty-p (string-trim text)))
         (not (string-match-p "[[:cntrl:]]" text))
         text)))))

(defun douban--toc-marker-node-p (node)
  "若 NODE 是 `douban--source-html' 生成的目录标记则返回非 nil。"
  (and
   (consp node)
   (eq (dom-tag node) 'div)
   (dom-attr node (intern douban--toc-marker-attribute))))

(defun douban--prepare-section-navigation (body)
  "分析并改写 BODY 中的正文标题导航。
源 fragment 根据 HTML `id' 找到标题后，会改写为豆瓣公开页实际使用的
`#可见标题文字'。若存在自动目录标记，返回应写入目录的标题记录。"
  (let ((all-ids (make-hash-table :test #'equal))
        (headings-by-id (make-hash-table :test #'equal))
        (plain-text-counts (make-hash-table :test #'equal))
        headings
        fragment-links
        toc-markers)
    (cl-labels
        ((walk
          (node in-document-reference heading-context)
          (when (consp node)
            (let* ((tag (dom-tag node))
                   (role (dom-attr node 'role))
                   (document-reference
                    (or
                     in-document-reference
                     (douban--document-reference-role-p role)))
                   (identifier (dom-attr node 'id))
                   (level (douban--heading-level tag)))
              (when
                  (and
                   (stringp identifier)
                   (not (string-empty-p identifier)))
                (puthash
                 identifier
                 (1+ (gethash identifier all-ids 0))
                 all-ids))
              (when
                  (and
                   level
                   heading-context
                   (not document-reference))
                (let* ((plain-text
                        (douban--plain-heading-text node))
                       (record
                        (list
                         :level level
                         :text plain-text)))
                  (push record headings)
                  (when plain-text
                    (puthash
                     plain-text
                     (1+ (gethash plain-text plain-text-counts 0))
                     plain-text-counts))
                  (when
                      (and
                       (stringp identifier)
                       (not (string-empty-p identifier)))
                    (puthash
                     identifier
                     (cons
                      record
                      (gethash identifier headings-by-id))
                     headings-by-id))))
              (when
                  (and
                   (douban--toc-marker-node-p node)
                   heading-context
                   (not document-reference))
                (push node toc-markers))
              (when
                  (and
                   (eq tag 'a)
                   (not document-reference))
                (let ((href (dom-attr node 'href)))
                  (when
                      (and
                       (stringp href)
                       (string-match-p
                        "\\`#[^#[:space:]]+\\'" href))
                    (push (cons node href) fragment-links))))
              (let ((child-heading-context
                     (and
                      heading-context
                      (memq
                       tag
                       douban--transparent-block-tags))))
                (dolist (child (dom-children node))
                  (walk
                   child
                   document-reference
                   child-heading-context)))))))
      (walk body nil t))
    (setq headings (nreverse headings)
          fragment-links (nreverse fragment-links)
          toc-markers (nreverse toc-markers))
    (when (> (length toc-markers) 1)
      (user-error "douban: 一篇源稿只能生成一个自动目录"))
    (dolist (entry fragment-links)
      (let* ((node (car entry))
             (href (cdr entry))
             (raw (substring href 1))
             (decoded (douban--decode-fragment-id raw))
             (identifier
              (cond
               ((gethash decoded all-ids) decoded)
               ((gethash raw all-ids) raw)
               (t decoded)))
             (id-count (gethash identifier all-ids))
             (targets (gethash identifier headings-by-id))
             (target (car targets))
             (text (and target (plist-get target :text))))
        (cond
         ((null id-count)
          (user-error
           "douban: 文章内链接 %s 找不到目标 id=%s"
           href identifier))
         ((or (cdr targets) (> id-count 1))
          (user-error
           "douban: 文章内链接 %s 的目标 id=%s 不唯一"
           href identifier))
         ((null target)
          (user-error
           "douban: 文章内链接 %s 的目标不是正文标题"
           href))
         ((null text)
          (user-error
           (concat
            "douban: 文章内链接 %s 的目标标题含有行内格式；"
            "豆瓣不会为它生成稳定锚点")
           href))
         ((> (gethash text plain-text-counts 0) 1)
          (user-error
           "douban: 文章内链接 %s 的可见标题文字重复：%s"
           href text)))
        (dom-set-attribute node 'href (concat "#" text))))
    (when-let* ((marker (car toc-markers)))
      (let* ((depth
              (string-to-number
               (dom-attr
                marker
                (intern douban--toc-marker-attribute))))
             (selected
              (cl-remove-if
               (lambda (record)
                 (> (plist-get record :level) depth))
               headings)))
        (unless selected
          (user-error
           "douban: 自动目录在深度 %d 内找不到正文标题"
           depth))
        (dolist (record selected)
          (let ((text (plist-get record :text)))
            (unless text
              (user-error
               (concat
                "douban: 自动目录中的标题含有行内格式；"
                "豆瓣不会为它生成稳定锚点")))
            (when (> (gethash text plain-text-counts 0) 1)
              (user-error
               "douban: 自动目录要求可见标题文字唯一：%s"
               text))))
        selected))))

(defun douban--highlight-block-node-p (node)
  "若 NODE 是 Pandoc 生成的豆瓣块高亮容器，则返回非 nil。"
  (and
   (consp node)
   (eq (dom-tag node) 'div)
   (equal
    (dom-attr
     node (intern douban--highlight-block-marker-attribute))
    "true")))

(defun douban--center-node-p (node)
  "若 NODE 是源稿生成的居中容器，则返回非 nil。"
  (and
   (consp node)
   (eq (dom-tag node) 'div)
   (or
    (douban--node-has-class-p node "center")
    (when-let* ((alignment
                (douban--css-property-value node "text-align")))
      (string-match-p
       "\\`center\\'"
       (downcase alignment))))))

(defun douban--block-container-node-p (node)
  "若 NODE 会开启或包含 Draft.js 区块，则返回非 nil。"
  (and
   (consp node)
   (or
    (memq (dom-tag node) douban--block-tags)
    (memq (dom-tag node) douban--transparent-block-tags))))

(defun douban--image-caption (node &optional fallback)
  "返回图片 NODE 的说明文字；找不到时可使用 FALLBACK。"
  (or
   (douban--metadata-text "image alt" (dom-attr node 'alt))
   (douban--metadata-text "image title" (dom-attr node 'title))
   fallback
   ""))

(defun douban--image-data (node &optional caption)
  "根据 HTML 图片 NODE 和 CAPTION 构造 Draft.js IMAGE 数据。"
  (let ((source (dom-attr node 'src)))
    (unless (and (stringp source) (not (string-empty-p source)))
      (error "douban: HTML 图片缺少 src"))
    (list
     :src source
     :caption (douban--image-caption node caption))))

(defun douban--draft-add-atomic-entity-block
    (draft entity-type data)
  "向 DRAFT 追加一个含不可变 ENTITY-TYPE 和 DATA 的原子块。
豆瓣 Draft.js 原子块固定使用单个空格作为文字，并用长度为 1 的 entity
range 引用该 entity。"
  (let* ((block (douban--draft-add-block draft "atomic"))
         (key
          (douban--draft-add-entity
           draft entity-type "IMMUTABLE" data)))
    (douban--block-write block " ")
    (douban--block-add-entity-range block key 0 1)
    block))

(defun douban--add-image-block
    (draft image &optional caption)
  "向 DRAFT 追加一个原子 IMAGE 区块。"
  (douban--draft-add-atomic-entity-block
   draft "IMAGE" (douban--image-data image caption)))

(defun douban--h-cite-node-p (node)
  "若 NODE 是 Microformats2 `h-cite' 根节点，则返回非 nil。"
  (and (consp node) (douban--node-has-class-p node "h-cite")))

(defun douban--h-cite-property-nodes (node property)
  "返回 NODE 内具有 Microformats2 PROPERTY class 的元素。"
  (dom-search
   node
   (lambda (child)
     (and
      (consp child)
      (douban--node-has-class-p child property)))))

(defun douban--validate-h-cite-placement (body)
  "确保 BODY 中的 `h-cite' 只作为顶层独立内容出现。"
  (let (allowed)
    (dolist (child (douban--dom-significant-children body))
      (cond
       ((douban--h-cite-node-p child)
        (push child allowed))
       ((and (consp child) (eq (dom-tag child) 'p))
        (pcase (douban--dom-significant-children child)
          (`(,only)
           (when (douban--h-cite-node-p only)
             (push only allowed)))))))
    (dolist
        (node
         (dom-search body #'douban--h-cite-node-p))
      (unless (memq node allowed)
        (user-error
         "douban: h-cite 卡片必须是文档顶层的独立内容")))))

(defun douban--user-mention-node-p (node)
  "若 NODE 是带有豆瓣用户 mention 源标记的链接，则返回非 nil。"
  (and
   (consp node)
   (eq (dom-tag node) 'a)
   (let ((title (dom-attr node 'title)))
     (and
      (stringp title)
      (string-prefix-p douban--user-mention-title-prefix title)))))

(defun douban--user-mention-data (node)
  "从用户 mention 链接 NODE 构造原生 USER entity data。"
  (let* ((title (dom-attr node 'title))
         (regexp
          (concat
           "\\`"
           (regexp-quote douban--user-mention-title-prefix)
           "\\([1-9][0-9]*\\)\\'"))
         (profile-url (dom-attr node 'href))
         (text
          (string-trim
           (replace-regexp-in-string
            "[\t\r\n]+" " " (dom-inner-text node))))
         (id
          (and
           (string-match regexp title)
           (match-string 1 title))))
    (unless id
      (user-error "douban: 用户 mention 标记中的用户 ID 无效"))
    (unless (douban--user-profile-url-p profile-url)
      (user-error
       "douban: 用户 mention 必须链接到规范豆瓣用户主页"))
    (unless (and
             (string-prefix-p "@" text)
             (> (length text) 1))
      (user-error "douban: 用户 mention 的链接文字必须是 @用户名"))
    (let ((name
           (douban--metadata-text
            "用户 mention 名称" (substring text 1))))
      (unless name
        (user-error "douban: 用户 mention 名称不能为空"))
      (list
       :url profile-url
       :name name
      :display "inline"
       :id id))))

(defun douban--card-data (node)
  "根据 Microformats2 `h-cite' NODE 构造待解析的原子 LINK 数据。"
  (let* ((url-nodes (douban--h-cite-property-nodes node "u-url"))
         (name-nodes (douban--h-cite-property-nodes node "p-name")))
    (unless (= (length url-nodes) 1)
      (user-error "douban: h-cite 卡片必须只包含一个 u-url"))
    (unless (= (length name-nodes) 1)
      (user-error "douban: h-cite 卡片必须只包含一个 p-name"))
    (let* ((url-node (car url-nodes))
           (name-node (car name-nodes))
           (url
            (and (eq (dom-tag url-node) 'a)
                 (dom-attr url-node 'href)))
           (title
            (string-trim
             (replace-regexp-in-string
              "[ \t\r\n]+" " " (dom-inner-text name-node)))))
    (unless (douban--http-url-p url)
      (user-error
       "douban: h-cite 的 u-url 必须是含 host 的 HTTP(S) 链接：%s"
       (or url "")))
    (when (string-empty-p title)
      (user-error "douban: h-cite 的 p-name 不能为空"))
    (list :url url :title title :display "atomic"))))

(defun douban--add-card-block (draft node)
  "把 `h-cite' NODE 作为原子 LINK 区块追加到 DRAFT。"
  (douban--draft-add-atomic-entity-block
   draft "LINK" (douban--card-data node)))

(defun douban--single-card-child (node)
  "返回 NODE 唯一的 `h-cite' 卡片子节点。"
  (pcase (douban--dom-significant-children node)
    (`(,child)
     (and (douban--h-cite-node-p child) child))))

(defun douban--add-separator-block (draft)
  "向 DRAFT 追加一个原子分隔线区块。"
  (douban--draft-add-atomic-entity-block
   draft "SEPARATOR" (douban--empty-object)))

(defun douban--inline-style-for-tag (tag)
  "返回 HTML TAG 对应的 Draft 行内样式名称。"
  (pcase tag
    ((or 'b 'strong) "BOLD")
    ((or 'i 'em) "ITALIC")
    ('code "CODE")
    ('mark "MARK")
    ('u "UNDERLINE")
    ((or 's 'del 'strike) "STRIKETHROUGH")
    (_ nil)))

(defun douban--walk-inline (draft block node)
  "将行内 DOM NODE 追加到 DRAFT 的 BLOCK。"
  (cond
   ((stringp node)
    (douban--block-write block node))
   ((not (consp node)) nil)
   (t
    (let* ((tag (dom-tag node))
           (style (douban--inline-style-for-tag tag)))
      (cond
       ((eq tag 'br)
        (douban--block-write block "\n"))
       (style
        (let ((offset (douban--block-offset block)))
          (dolist (child (dom-children node))
            (douban--walk-inline draft block child))
          (douban--block-add-inline-range
           block style offset
           (- (douban--block-offset block) offset))))
       ((douban--h-cite-node-p node)
        (user-error
         "douban: h-cite 卡片必须是文档顶层的独立内容"))
       ((douban--highlight-block-node-p node)
        (user-error "douban: 块高亮不能嵌入其他正文块"))
       ((douban--center-node-p node)
        (user-error "douban: 居中容器不能嵌入其他正文块"))
       ((douban--user-mention-node-p node)
        (let* ((offset (douban--block-offset block))
               (data (douban--user-mention-data node))
               (text (concat "@" (plist-get data :name))))
          (douban--block-write block text)
          (douban--block-add-entity-range
           block
           (douban--draft-add-entity
            draft "USER" "IMMUTABLE" data)
           offset
           (douban--utf16-length text))))
       ((eq tag 'a)
        (let ((href (dom-attr node 'href))
              (offset (douban--block-offset block)))
          (dolist (child (dom-children node))
            (douban--walk-inline draft block child))
          (let ((length (- (douban--block-offset block) offset)))
            (when
                (and
                 (stringp href)
                 (not (string-empty-p href))
                 (> length 0))
              (douban--block-add-entity-range
               block
               (douban--draft-add-entity
                draft "LINK" "MUTABLE"
                (list :url href))
               offset length)))))
       ((eq tag 'img)
        (when-let* ((alt
                    (douban--metadata-text
                     "image alt" (dom-attr node 'alt))))
          (douban--block-write block alt)))
       (t
        (dolist (child (dom-children node))
          (douban--walk-inline draft block child))))))))

(defun douban--draft-add-inline-block
    (draft type nodes &optional depth data)
  "向 DRAFT 追加 TYPE 区块，并把 NODES 作为行内内容写入。
DEPTH 和 DATA 原样传给 `douban--draft-add-block'。这个入口只适用于
已经确定不会产生嵌套 Draft.js 区块的 DOM 节点。"
  (let ((block
         (douban--draft-add-block draft type depth data)))
    (dolist (node nodes)
      (douban--walk-inline draft block node))
    block))

(defun douban--single-image-child (node)
  "返回 NODE 唯一的图片子节点，允许图片外层包裹一个链接。"
  (let ((children (douban--dom-significant-children node)))
    (pcase children
      (`(,child)
       (cond
        ((and (consp child) (eq (dom-tag child) 'img)) child)
        ((and (consp child) (eq (dom-tag child) 'a))
         (let ((inner (douban--dom-significant-children child)))
           (and
            (= (length inner) 1)
            (consp (car inner))
            (eq (dom-tag (car inner)) 'img)
            (car inner)))))))))

(defun douban--heading-block-type (tag)
  "返回标题 TAG 对应的 Draft 区块类型。"
  (pcase tag
    ((or 'h1 'h2) "header-two")
    ('h3 "header-three")
    ((or 'h4 'h5 'h6) "header-four")
    (_ "unstyled")))

(defun douban--add-generated-toc (draft)
  "把 DRAFT 转换上下文中的标题作为普通目录追加到 DRAFT。"
  (let ((headings (douban--draft-toc-headings draft)))
    (unless headings
      (error "douban: 自动目录标记缺少已校验的标题"))
    (let ((title (douban--draft-add-block draft "unstyled")))
      (douban--block-write title "目录")
      (douban--block-add-inline-range title "BOLD" 0 2))
    (let (level-stack)
      (dolist (heading headings)
        (let* ((level (plist-get heading :level))
               (text (plist-get heading :text)))
          (while
              (and
               level-stack
               (>= (car level-stack) level))
            (pop level-stack))
          (let* ((depth (min 4 (length level-stack)))
                 (block
                  (douban--draft-add-block
                   draft "unordered-list-item" depth))
                 (key
                  (douban--draft-add-entity
                   draft "LINK" "MUTABLE"
                   (list :url (concat "#" text))))
                 (length (douban--utf16-length text)))
            (douban--block-write block text)
            (douban--block-add-entity-range block key 0 length))
          (push level level-stack))))))

(defun douban--walk-container-parts
    (draft parts type depth context)
  "把容器 PARTS 作为 TYPE 区块写入 DRAFT。
DEPTH 是列表深度；CONTEXT 是 `list' 或 `quote'，决定嵌套块的语义。
若 PARTS 本身生成了区块则返回非 nil。"
  (let ((label (if (eq context 'list) "列表" "引用"))
        pending emitted)
    (cl-labels
        ((flush-inline
          ()
          (when pending
            (douban--draft-add-inline-block
             draft type (reverse pending) depth)
            (setq pending nil
                  emitted t)))
         (write-block
          (node)
          (flush-inline)
          (if-let* ((image (douban--single-image-child node)))
              (douban--add-image-block draft image)
            (douban--draft-add-inline-block
             draft type (dom-children node) depth))
          (setq emitted t)))
      (dolist (part parts)
        (cond
         ((douban--dom-whitespace-node-p part))
         ((not (consp part))
          (push part pending))
         ((douban--highlight-block-node-p part)
          (user-error "douban: 块高亮不能嵌入%s" label))
         ((douban--center-node-p part)
          (user-error "douban: 居中容器不能嵌入%s" label))
         ((and
           (eq context 'list)
           (memq (dom-tag part) '(ul ol)))
          (flush-inline)
          (douban--walk-list
           draft part (1+ depth) (eq (dom-tag part) 'ol)))
         ((and
           (eq context 'quote)
           (eq (dom-tag part) 'blockquote))
          (flush-inline)
          (douban--walk-blockquote draft part)
          (setq emitted t))
         ((and
           (eq context 'quote)
           (memq (dom-tag part) '(ul ol)))
          (flush-inline)
          (douban--walk-list
           draft part 0 (eq (dom-tag part) 'ol))
          (setq emitted t))
         ((memq (dom-tag part) '(p h1 h2 h3 h4 h5 h6))
          (write-block part))
         ((memq (dom-tag part) douban--transparent-block-tags)
          (flush-inline)
          (when
              (douban--walk-container-parts
               draft (dom-children part) type depth context)
            (setq emitted t)))
         ((or
           (memq (dom-tag part) douban--block-tags)
           (cl-some
            #'douban--block-container-node-p
            (dom-children part)))
          (flush-inline)
          (douban--walk-block-node draft part)
          (setq emitted t))
         (t
          (push part pending))))
      (flush-inline))
    emitted))

(defun douban--walk-list (draft node depth ordered)
  "将 DEPTH 层级的列表 NODE 转换并写入 DRAFT。
ORDERED 决定 Draft 列表区块类型。"
  (let ((type
         (if ordered
             "ordered-list-item"
           "unordered-list-item")))
    (dolist (child (douban--dom-element-children node))
      (when (eq (dom-tag child) 'li)
        (unless
            (douban--walk-container-parts
             draft (dom-children child) type depth 'list)
          ;; Draft represents an empty list item as an empty list block.
          (douban--draft-add-block draft type depth))))))

(defun douban--walk-blockquote (draft node)
  "将引用块 NODE 转换为 DRAFT 中的一个或多个区块。"
  (unless
      (douban--walk-container-parts
       draft (dom-children node) "blockquote" nil 'quote)
    (douban--draft-add-block draft "blockquote")))

(defun douban--walk-table (draft node)
  "将 HTML 表格 NODE 展平为 DRAFT 中以制表符分隔的区块。"
  (let ((rows (dom-by-tag node 'tr)))
    (dolist (row rows)
      (let ((cells
             (cl-remove-if-not
              (lambda (cell)
                (memq (dom-tag cell) '(td th)))
              (douban--dom-element-children row))))
        (when cells
          (let ((block (douban--draft-add-block draft "unstyled")))
            (cl-loop
             for cell in cells
             for first = t then nil
             do
             (unless first (douban--block-write block "\t"))
             (douban--walk-inline draft block cell))))))))

(defun douban--figure-image-and-caption (node)
  "返回标准 Pandoc figure NODE 的 `(IMAGE . CAPTION)'。
CAPTION 可以为 nil；NODE 不是只含一张图片和至多一个图注的 figure
时返回 nil。"
  (let* ((children (douban--dom-significant-children node))
         (image
          (cl-find-if
           (lambda (child)
             (and (consp child) (eq (dom-tag child) 'img)))
           children))
         (caption
          (cl-find-if
           (lambda (child)
             (and (consp child) (eq (dom-tag child) 'figcaption)))
           children)))
    (and
     image
     (cl-every
      (lambda (child)
        (or (eq child image) (eq child caption)))
      children)
     (cons image caption))))

(defun douban--add-figure-image-block (draft parts)
  "把已解析的 figure PARTS 作为图片原子块追加到 DRAFT。"
  (let ((image (car parts))
        (caption (cdr parts)))
    (douban--add-image-block
     draft image
     (and caption (string-trim (dom-inner-text caption))))))

(defun douban--walk-figure (draft node)
  "若 NODE 符合标准 Pandoc figure 结构，则转换并写入 DRAFT；否则遍历其子节点。"
  (if-let* ((parts (douban--figure-image-and-caption node)))
      (douban--add-figure-image-block draft parts)
    (dolist (child (douban--dom-significant-children node))
      (douban--walk-block-node draft child))))

(defun douban--walk-highlight-block (draft node)
  "把 NODE 中每个非空段落转换为 DRAFT 的块高亮。"
  (let ((children
         (douban--dom-significant-children node)))
    (unless children
      (user-error "douban: 块高亮不能为空"))
    (dolist (child children)
      (unless
          (and
           (consp child)
           (eq (dom-tag child) 'p)
           (not
            (string-empty-p
             (string-trim (dom-inner-text child))))
           (not (dom-by-tag child 'img))
           (not
            (douban--dom-has-descendant-p
             child #'douban--block-container-node-p)))
        (user-error
         "douban: 块高亮只能包含非空的普通段落"))
      (douban--draft-add-inline-block
       draft "highlight-block" (dom-children child) 0
       (list :align "")))))

(defun douban--walk-center (draft node)
  "把居中容器 NODE 的段落和标题写入 DRAFT。
独立图片仍使用普通 IMAGE 原子块；其他块结构一律拒绝。"
  (let ((children (douban--dom-significant-children node)))
    (unless children
      (user-error "douban: 居中容器不能为空"))
    (dolist (child children)
      (let ((tag (and (consp child) (dom-tag child))))
        (cond
         ((eq tag 'p)
          (if-let* ((image (douban--single-image-child child)))
              (douban--add-image-block draft image)
            (douban--draft-add-inline-block
             draft "unstyled" (dom-children child) 0
             (list :align "center"))))
         ((memq tag '(h1 h2 h3 h4 h5 h6))
          (douban--draft-add-inline-block
           draft (douban--heading-block-type tag)
           (dom-children child) 0
           (list :align "center")))
         ((eq tag 'img)
          (douban--add-image-block draft child))
         ((eq tag 'figure)
          (if-let* ((parts
                    (douban--figure-image-and-caption child)))
              (douban--add-figure-image-block draft parts)
            (user-error
             "douban: 居中容器只能包含普通段落、标题和独立图片")))
         (t
          (user-error
           "douban: 居中容器只能包含普通段落、标题和独立图片")))))))

(defun douban--walk-block-node (draft node)
  "将块级 DOM NODE 转换并写入 DRAFT。"
  (cond
   ((stringp node)
    (unless (string-match-p "\\`[ \t\r\n]*\\'" node)
      (let ((block (douban--draft-add-block draft "unstyled")))
        (douban--block-write block node))))
   ((not (consp node)) nil)
   (t
    (let ((tag (dom-tag node)))
      (cond
       ((douban--toc-marker-node-p node)
        (douban--add-generated-toc draft))
       ((douban--h-cite-node-p node)
        (douban--add-card-block draft node))
       ((douban--center-node-p node)
        (douban--walk-center draft node))
       ((douban--highlight-block-node-p node)
        (douban--walk-highlight-block draft node))
       (t
        (pcase tag
          ((pred (lambda (value)
                   (memq value douban--transparent-block-tags)))
           (dolist (child (dom-children node))
             (douban--walk-block-node draft child)))
          ('p
           (if-let* ((card (douban--single-card-child node)))
               (douban--add-card-block draft card)
             (if-let* ((image (douban--single-image-child node)))
                 (douban--add-image-block draft image)
               (douban--draft-add-inline-block
                draft "unstyled" (dom-children node)))))
          ((or 'h1 'h2 'h3 'h4 'h5 'h6)
           (douban--draft-add-inline-block
            draft (douban--heading-block-type tag)
            (dom-children node)))
          ('blockquote
           (douban--walk-blockquote draft node))
          ('pre
           (let ((block
                  (douban--draft-add-block draft "code-block")))
             (douban--block-write block (dom-inner-text node))))
          ('figure
           (douban--walk-figure draft node))
          ('img
           (douban--add-image-block draft node))
          ('hr
           (douban--add-separator-block draft))
          ('ul
           (douban--walk-list draft node 0 nil))
          ('ol
           (douban--walk-list draft node 0 t))
          ('table
           (douban--walk-table draft node))
          ('dl
           (dolist (child (douban--dom-element-children node))
             (douban--draft-add-inline-block
              draft
              (if (eq (dom-tag child) 'dt)
                  "header-four"
                "unstyled")
              (list child))))
          (_
           (if (cl-some
                #'douban--block-container-node-p
                (dom-children node))
               (dolist (child (dom-children node))
                 (douban--walk-block-node draft child))
             (douban--draft-add-inline-block
              draft "unstyled" (list node)))))))))))

(defun douban--block-raw (block)
  "将可变 BLOCK 转换为可直接用于 JSON 的 Draft raw 区块 plist。"
  (list
   :key (douban--block-key block)
   :text (douban--block-text block)
   :type (douban--block-type block)
   :depth (douban--block-depth block)
   :inlineStyleRanges
   (vconcat (reverse (douban--block-inline-ranges block)))
   :entityRanges
   (vconcat (reverse (douban--block-entity-ranges block)))
   :data (douban--block-data block)))

(defun douban--draft-raw (draft)
  "将可变 DRAFT 转换为可直接用于 JSON 的 Draft.js raw plist。"
  (let ((blocks
         (mapcar
          #'douban--block-raw
          (reverse (douban--draft-blocks draft)))))
    ;; Draft.js expects at least one block.
    (unless blocks
      (setq blocks
            (list
             (douban--block-raw
              (douban--draft-add-block draft "unstyled")))))
    (list
     :blocks (vconcat blocks)
     :entityMap (douban--draft-entities draft))))

(defun douban--html-to-draft (html)
  "把 HTML 片段或完整 HTML 文档转换为 Draft.js 原始内容。"
  (let* ((document
          (douban--parse-html
           (if (string-match-p "<[ \t\n]*html\\b" html)
               html
             (concat "<html><body>" html "</body></html>"))))
         (body (car (dom-by-tag document 'body)))
         (toc-headings
          (douban--prepare-section-navigation body))
         (draft (douban--new-draft toc-headings)))
    (douban--validate-h-cite-placement body)
    (dolist (child (dom-children body))
      (douban--walk-block-node draft child))
    (douban--draft-raw draft)))

(defun douban--draft-character-count (raw)
  "统计 Draft RAW 中正文的非空白字符数。"
  (cl-loop
   for block across (plist-get raw :blocks)
   for type = (plist-get block :type)
   unless (string-equal type "atomic")
   sum
   (douban--utf16-length
    (replace-regexp-in-string
     "[ \t\r\n]+" "" (plist-get block :text)))))

(defun douban--validate-draft (raw)
  "当 Draft RAW 过短时发出用户错误。"
  (let ((count (douban--draft-character-count raw)))
    (when (< count douban-minimum-review-length)
      (user-error
       "douban: 正文只有 %d 个非空白字符；豆瓣长评至少需要 %d 字"
       count douban-minimum-review-length))
    count))


;;;; Link cards

(defconst douban--card-endpoint
  "https://m.douban.com/rexxar/api/v2/get_url_info"
  "把 URL 解析为当前编辑器卡片的端点。")

(defun douban--card-result (response source-url)
  "从 RESPONSE 读取 SOURCE-URL 对应的规范卡片实体。"
  (let* ((status (plist-get response :status))
         (json (plist-get response :json)))
    (unless (<= 200 status 299)
      (user-error
       "douban: 无法解析卡片 %s（HTTP %s）"
       source-url status))
    (let* ((entity-type (plist-get json :type))
           (source (plist-get json :data))
           (title
            (and
             (listp source)
             (douban--metadata-text
              "卡片 title" (plist-get source :title))))
           (url (and (listp source) (plist-get source :url))))
      (setq source (copy-sequence source))
      (pcase entity-type
        ("LINK"
         (unless (and title (douban--http-url-p url))
           (user-error
            "douban: 卡片响应缺少规范字段：%s"
            source-url)))
        ("SUBJECT"
         (let ((id
                (douban--metadata-id
                 "卡片 id" (plist-get source :id)))
               (type
                (douban--metadata-text
                 "卡片 type" (plist-get source :type))))
           (unless
               (and id type title
                    (douban--https-douban-url-p url))
             (user-error
              "douban: 卡片响应缺少规范字段：%s"
              source-url))
           (setq source (plist-put source :id id))
           (setq source (plist-put source :type type))))
        (_
         (user-error
          "douban: URL 不能生成卡片：%s"
          source-url)))
      (setq source (plist-put source :title title))
      (let ((cover
             (plist-get
              source
              (if (equal entity-type "LINK")
                  :cover_url
                :cover))))
        (when (and cover
                   (not (eq cover :json-null))
                   (not (douban--https-url-p cover)))
          (error "douban: 卡片封面不是 HTTPS URL")))
      (list
       :type entity-type
       :data (plist-put source :display "atomic")))))

(defun douban--resolve-card (url)
  "通过豆瓣当前 URL 解析接口取得 URL 的卡片实体。"
  (douban--card-result
   (douban--read-json-endpoint
    (format
     (concat
      "%s?url=%s&need_card=1&editor_type=group")
     douban--card-endpoint
     (url-hexify-string url))
    "https://www.douban.com/")
   url))

(defun douban--rewrite-draft-cards (raw)
  "解析 RAW 中显式原子卡片的 LINK entity。"
  (let ((entities (douban--draft-referenced-entities raw))
        (cache (make-hash-table :test 'equal)))
    (cl-labels
        ((resolve
          (url)
          (or
           (gethash url cache)
           (progn
             (message "douban: 解析卡片 %s..." url)
             (let ((result (douban--resolve-card url)))
               (puthash url result cache)
               result)))))
      (dolist (entity entities)
        (when
            (and
             (equal (plist-get entity :type) "LINK")
             (equal
              (plist-get (plist-get entity :data) :display)
              "atomic"))
          (let* ((source (plist-get entity :data))
                 (url (plist-get source :url))
                 (resolved (resolve url)))
            (setf (plist-get entity :type)
                  (plist-get resolved :type))
            (setf (plist-get entity :data)
                  (plist-get resolved :data)))))
    raw)))

;;;; Web editor context

(defun douban--dom-inputs (node name)
  "返回 NODE 中所有名为 NAME 的 input，并保留 DOM 顺序。"
  (cl-remove-if-not
   (lambda (input)
     (equal (dom-attr input 'name) name))
   (dom-by-tag node 'input)))

(defun douban--dom-input-value (node name)
  "返回 NODE 中第一个名为 NAME 的非空 input 值。"
  (cl-loop
   for input in (douban--dom-inputs node name)
   for value = (douban--value-string (dom-attr input 'value))
   when value return value))

(defun douban--dom-input-choice-value (node name)
  "返回 NODE 中名为 NAME 的已选或隐藏非空 input 值。
对于 radio 和 checkbox 组，优先返回已选元素；否则返回第一个
非选择型 input 的值。"
  (let ((inputs (douban--dom-inputs node name)))
    (or
     (cl-loop
      for input in inputs
      for value = (douban--value-string (dom-attr input 'value))
      when (and (dom-attr input 'checked) value)
      return value)
     (cl-loop
      for input in inputs
      for type = (downcase (or (dom-attr input 'type) ""))
      for value = (douban--value-string (dom-attr input 'value))
      when
      (and
       (not (member type '("radio" "checkbox")))
       value)
      return value))))

(defun douban--dom-form-with-input (document name)
  "返回 DOCUMENT 中唯一包含 NAME input 的 form。"
  (let ((forms
         (cl-remove-if-not
          (lambda (form)
            (douban--dom-inputs form name))
          (dom-by-tag document 'form))))
    (and (= (length forms) 1) (car forms))))

(defun douban--javascript-variable (html name type)
  "从 HTML 中按 TYPE 提取 JavaScript 变量 NAME。
TYPE 为 `string' 时接受带引号的字符串，为 `id' 时接受十进制 ID。
两种类型都返回字符串；找不到匹配变量时返回 nil。"
  (let ((case-fold-search nil)
        (value-pattern
         (pcase type
           ('string "['\"]\\([^'\"]*\\)['\"]")
           ('id "\\([0-9]+\\)")
           (_
            (error
             "douban: 不支持的 JavaScript 变量类型：%S"
             type)))))
    (when
        (string-match
         (concat
          "^" (regexp-quote name)
          "[ \t]*=[ \t]*" value-pattern "[ \t]*;")
         html)
      (match-string 1 html))))

(defun douban--upload-credential (html)
  "从编辑器 HTML 中提取上传凭据 `(FIELD . TOKEN)'。"
  (let ((case-fold-search nil))
    (when
        (string-match
         (concat
          "_POST_PARAMS[ \t]*="
          "\\(?:.\\|\n\\)*?"
          "siteCookie[ \t]*:"
          "\\(?:.\\|\n\\)*?"
          "name[ \t]*:[ \t]*['\"]\\([^'\"]+\\)['\"]"
          "\\(?:.\\|\n\\)*?"
          "value[ \t]*:[ \t]*['\"]\\([^'\"]+\\)['\"]")
         html)
      (cons (match-string 1 html) (match-string 2 html)))))

(defun douban--review-editor-state (html game-p)
  "从 HTML 读取评论编辑状态。
GAME-P 非 nil 时一并读取游戏评论类型。"
  (let* ((form
          (cl-find-if
           (lambda (candidate)
             (equal (dom-attr candidate 'id) "review-editor-form"))
           (dom-by-tag (douban--parse-html html) 'form)))
         (ck (and form (douban--dom-input-value form "ck")))
         (subject-id
          (and
           form
           (douban--dom-input-value
            form "review[subject_id]")))
         (credential (douban--upload-credential html)))
    (unless (and ck subject-id)
      (user-error "douban: 评论编辑页不可用"))
    (list
     :ck ck
     :subject-id subject-id
     :review-id
     (douban--javascript-variable html "_REVIEW_ID" 'id)
     :rtype
     (and
      game-p
      (douban--dom-input-choice-value form "review[rtype]"))
     :upload-field (car credential)
     :upload-token (cdr credential))))

(defun douban--review-app-name (subject-type)
  "返回 SUBJECT-TYPE 对应的评论编辑器 app 名称。"
  (if (string-equal subject-type "tv") "movie" subject-type))

(defun douban--subject-type-host (subject-type)
  "返回 SUBJECT-TYPE 通常使用的网页主机名。"
  (pcase subject-type
    ((or "movie" "tv") "movie.douban.com")
    ("book" "book.douban.com")
    ("music" "music.douban.com")
    ("game" "www.douban.com")))

(defun douban--subject-url (subject-type subject-id)
  "返回 SUBJECT-TYPE 和 SUBJECT-ID 对应的规范 URL。"
  (format
   (if (string-equal subject-type "game")
       "https://%s/game/%s/"
     "https://%s/subject/%s/")
   (douban--subject-type-host subject-type)
   subject-id))

(defun douban--canonical-review-url (subject-type review-id)
  "返回 SUBJECT-TYPE 中 REVIEW-ID 对应的规范评论 URL。"
  (format
   "https://%s/review/%s/"
   (douban--subject-type-host subject-type)
   review-id))

(defun douban--review-direct-session (meta)
  "仅根据 META 和 Cookie 构造评论直接发布会话。"
  (let* ((subject-id (plist-get meta :subject-id))
         (subject-type (plist-get meta :subject-type))
         (review-id (plist-get meta :review-id))
         (referer
          (if review-id
              (douban--canonical-review-url
               subject-type review-id)
            (douban--subject-url
             subject-type subject-id)))
         (session
          (douban--cookie-session 'review referer)))
    (setf
     (douban--session-state session)
     (list
      :app-name (douban--review-app-name subject-type)))
    session))

(defun douban--review-editor-session (meta)
  "读取 META 对应的评论编辑页并返回页面绑定的会话。"
  (let* ((subject-type (plist-get meta :subject-type))
         (expected-id (plist-get meta :review-id))
         (expected-subject-id (plist-get meta :subject-id))
         (app-name (douban--review-app-name subject-type))
         (editor-url
          (if expected-id
              (concat
               (douban--canonical-review-url
                subject-type expected-id)
               "edit")
            (format
             "https://www.douban.com/subject/%s/new_review"
             expected-subject-id)))
         (session (douban--browser-session 'review editor-url))
         (state
          (douban--review-editor-state
           (douban--read-html-page
            editor-url session "评论编辑页")
           (string-equal app-name "game"))))
    (unless
        (and
         (equal expected-id (plist-get state :review-id))
         (equal
          expected-subject-id
          (plist-get state :subject-id)))
      (user-error "douban: 评论编辑页与当前源稿身份不匹配"))
    (setq state (plist-put state :app-name app-name))
    (setf
     (douban--session-ck session) (plist-get state :ck)
     (douban--session-state session) state
     (douban--session-cookies session)
     (douban--cookie-put
      (douban--session-cookies session)
      "ck" (douban--session-ck session)))
    session))

;;;; Images

(defun douban--topic-kind-p (kind)
  "KIND 使用当前 topic 编辑器及其图片协议时返回非 nil。"
  (memq kind '(status annotation)))

(defconst douban--topic-image-endpoint
  "https://www.douban.com/j/group/topic/add_photo"
  "topic 内容使用的本地图片上传端点。")

(defconst douban--topic-fetch-image-endpoint
  "https://www.douban.com/j/group/topic/fetch_photo"
  "topic 内容使用的远程图片抓取端点。")

(defun douban--image-mime (mime bytes source)
  "识别来自 SOURCE 的 BYTES，返回安全的图片 MIME。
优先采用从字节识别出的类型；否则接受语法有效的 `image/*' MIME，
由豆瓣决定是否支持其格式。"
  (let* ((type
          (or
           (and
            (stringp bytes)
            (not (string-empty-p bytes))
            (image-type-from-data bytes))
           (and
            (stringp bytes)
            (>= (length bytes) 2)
            (= (aref bytes 0) ?B)
            (= (aref bytes 1) ?M)
            'bmp)))
         (detected
          (and
           type
           (mailcap-file-name-to-mime-type
            (concat "image." (symbol-name type)))))
         (mime (downcase (or detected mime ""))))
    (unless
        (string-match-p
         "\\`image/[a-z0-9][a-z0-9!#$&^_.+-]*\\'" mime)
      (user-error
       "douban: 图片 MIME 无效 %S：%s" mime source))
    mime))

(defun douban--decode-image-data-url (source)
  "若 SOURCE 是图片 data URL，则解码并返回 `(MIME . BYTES)'；否则返回 nil。"
  (let ((case-fold-search t))
    (when (and
           (stringp source)
           (string-match-p "\\`data:" source))
      (unless
          (string-match
           (concat
            "\\`data:"
            "\\(image/[A-Za-z0-9][A-Za-z0-9!#$&^_.+-]*\\)"
            ";base64,\\([^ \t\r\n]+\\)\\'")
           source)
        (user-error
         "douban: 图片 data URL 必须是 data:image/SUBTYPE;base64,..."))
      (let* ((declared (downcase (match-string 1 source)))
             (payload (match-string 2 source))
             (bytes
              (condition-case nil
                  (base64-decode-string payload)
                (error
                 (user-error "douban: 图片 data URL 的 base64 无效")))))
        (when (string-empty-p bytes)
          (user-error "douban: 图片 data URL 的内容为空"))
        (cons declared bytes)))))

(defun douban--image-file-name-for-mime (mime)
  "返回适用于图片 MIME 的安全上传文件名。"
  (let ((subtype (substring mime (1+ (string-search "/" mime)))))
    (concat
     "image."
     (replace-regexp-in-string
      "[^[:alnum:]]" "_" subtype))))

(defun douban--read-file-bytes (path)
  "以单字节字符串读取 PATH。"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun douban--multipart-quote (text)
  "转义用于 multipart Content-Disposition 参数的 TEXT。"
  (replace-regexp-in-string
   "[\"\r\n]" "_" (format "%s" text)))

(cl-defun douban--multipart-body
    (fields &key file-field file-name file-mime file-bytes)
  "根据 FIELDS 和可选的 FILE-FIELD 数据构造 multipart 数据。
FILE-NAME、FILE-MIME 和 FILE-BYTES 描述可选文件。
返回 `(CONTENT-TYPE . UNIBYTE-BODY)'。"
  (let* ((boundary
          (concat
           "----douban-el-"
           (substring
            (secure-hash
             'sha256
             (format
              "%s:%s:%s" (float-time) (random) (emacs-pid)))
            0 32)))
         (crlf "\r\n")
         (buffer (generate-new-buffer " *douban-multipart*")))
    (unwind-protect
        (with-current-buffer buffer
          (set-buffer-multibyte nil)
          (dolist (field fields)
            (insert
             (encode-coding-string
              (concat
               "--" boundary crlf
               "Content-Disposition: form-data; name=\""
               (douban--multipart-quote (car field))
               "\"" crlf crlf
               (format "%s" (or (cdr field) ""))
               crlf)
              'utf-8)))
          (when file-field
            (insert
             (encode-coding-string
              (concat
               "--" boundary crlf
               "Content-Disposition: form-data; name=\""
               (douban--multipart-quote file-field)
               "\"; filename=\""
               (douban--multipart-quote file-name)
               "\"" crlf
               "Content-Type: " file-mime crlf crlf)
              'utf-8))
            (insert file-bytes)
            (insert crlf))
          (insert
           (encode-coding-string
            (concat "--" boundary "--" crlf)
            'utf-8))
          (cons
           (concat "multipart/form-data; boundary=" boundary)
           (buffer-string)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun douban--upload-response-photo (response)
  "校验图片上传 RESPONSE，并返回其中的 photo plist。"
  (let* ((status (plist-get response :status))
         (json (plist-get response :json))
         (detail (douban--response-detail response))
         (result-code (and json (plist-get json :r)))
         (photo (and json (plist-get json :photo))))
    (unless (<= 200 status 299)
      (error
       "douban: 图片上传 HTTP %s：%s"
       status detail))
    (when (and result-code
               (not (or (eq result-code 0)
                        (eq result-code :json-false))))
      (error
       "douban: 图片上传失败：%s"
       (or (plist-get json :err)
           (plist-get json :message)
           result-code)))
    (unless (consp photo)
      (error "douban: 图片上传响应缺少 photo：%s" detail))
    photo))

(defun douban--photo-url (photo kind)
  "从已上传的 PHOTO 中返回 KIND 对应的规范 HTTPS URL。"
  (let* ((thumb
          (and
           (eq kind 'review)
           (douban--value-string (plist-get photo :thumb))))
         (url
          (or
           (douban--value-string (plist-get photo :url))
           (and
            thumb
            (replace-regexp-in-string
             "small" "large" thumb t t)))))
    (unless (and
             url
             (douban--https-url-p url))
      (error "douban: 图片响应没有 HTTPS URL"))
    url))

(defun douban--upload-common-fields (session)
  "返回 SESSION 上传图片时共用的 multipart 字段。"
  (let ((field
         (douban--session-state-get session :upload-field))
        (token
         (douban--session-state-get session :upload-token)))
    (unless (and
             field token
             (not (string-empty-p field))
             (not (string-empty-p token)))
      (error
       "douban: 编辑器没有提供图片上传凭据；正文含图片时无法继续"))
    (append
     (list (cons "ck" (douban--session-ck session)))
     (pcase (douban--session-kind session)
       ('note
       (list
         (cons
          "note_id"
          (douban--session-state-get session :note-id))))
       ('review
        (list
         (cons
          "review_id"
          (or
           (douban--session-state-get session :review-id)
           ""))))
       ((or 'status 'annotation)
        '(("primary_color" . ""))))
     (list (cons field token)))))

(defun douban--download-image-url (image-url)
  "下载公开 HTTPS IMAGE-URL，且不发送豆瓣凭据。
校验响应后返回 `(MIME . BYTES)'。不会跟随重定向，因而源 URL
不能悄然切换到其他源站。"
  (let* ((response
          (douban--plz-request
           "GET" image-url
           :headers
           `(("User-Agent" . ,douban-user-agent)
             ("Accept" . "image/*"))
           :decode nil))
         (status (plz-response-status response))
         (headers
          (douban--normalize-response-headers
           (plz-response-headers response)))
         (content-type
          (cdr (assoc-string "content-type" headers t)))
         (mime
          (and
           content-type
           (downcase
            (car
             (split-string content-type "[ \t]*;[ \t]*" t)))))
         (bytes (plz-response-body response)))
    (unless (<= 200 status 299)
      (error
       "douban: 下载远程图片失败（HTTP %s）：%s"
       (or status "无效") image-url))
    (cons mime bytes)))

(defun douban--upload-image-url (session image-url)
  "在 SESSION 中处理远程 IMAGE-URL，并返回其 photo plist。"
  (unless (douban--https-url-p image-url)
    (user-error
     "douban: 远程图片必须是合法 HTTPS URL：%s"
     image-url))
  (pcase (douban--session-kind session)
    ((or 'note 'review)
     (pcase-let ((`(,mime . ,bytes)
                  (douban--download-image-url image-url)))
       (douban--upload-image-bytes session bytes mime)))
    ((or 'status 'annotation)
     (douban--upload-response-photo
      (douban--http-json
       "POST" douban--topic-fetch-image-endpoint
       :body
       (json-serialize (list :photo_url image-url))
       :content-type "application/json;charset=utf-8"
       :extra-headers
       `(("Accept" . "application/json")
         ("X-CSRF-TOKEN" . ,(douban--session-ck session))
         ("Referer" . ,(douban--session-referer session))
         ("Origin" . "https://www.douban.com"))
       :session session)))))

(defun douban--upload-image-bytes (session bytes mime)
  "在 SESSION 中上传图片 BYTES（MIME 类型为 MIME），并返回其 photo plist。"
  (setq mime (douban--image-mime mime bytes "上传内容"))
  (let* ((kind (douban--session-kind session))
         (note-p (eq kind 'note))
         (topic-p (douban--topic-kind-p kind))
         (multipart
          (douban--multipart-body
           (douban--upload-common-fields session)
           :file-field
           (if (or note-p topic-p) "image_file" "picfile")
           :file-name (douban--image-file-name-for-mime mime)
           :file-mime mime
           :file-bytes bytes))
         (endpoint
          (cond
           (note-p
            "https://www.douban.com/j/note/add_photo")
           (topic-p douban--topic-image-endpoint)
           (t
            (format
             "https://%s/j/review/upload_image"
             (douban--session-host session)))))
         (response
          (douban--http-json
           "POST" endpoint
           :body (cdr multipart)
           :content-type (car multipart)
           :extra-headers
           (if topic-p
               `(("X-Requested-With" . "XMLHttpRequest")
                 ("Referer" .
                  ,(douban--session-referer session))
                 ("Origin" . "https://www.douban.com"))
             (douban--mutation-headers session))
           :session session
           :raw-body t)))
    (douban--upload-response-photo response)))

(defun douban--douban-image-url-p (source)
  "当 SOURCE 已托管于豆瓣图片 CDN 时返回非 nil。"
  (when (douban--https-url-p source)
    (let ((host (douban--url-host source)))
      (or
       (string-equal host "doubanio.com")
       (string-suffix-p ".doubanio.com" host t)))))

(defun douban--local-image-path (source base-directory)
  "相对于 BASE-DIRECTORY 解析文件图片 SOURCE。
所得路径可以是本地路径，也可以由 TRAMP 处理。"
  (let* ((reference
          (substring
           source 0
           (or (string-match "[?#]" source) (length source))))
         (parsed (url-generic-parse-url reference))
         (scheme (url-type parsed))
         (host (url-host parsed)))
    (cond
     ((or
       (member scheme '("http" "https"))
       (and (null scheme) host))
      nil)
     ((and scheme (not (string-equal scheme "file")))
      (user-error "douban: 不支持图片 URL scheme：%s" scheme))
     ((and
       host
       (not (string-empty-p host))
       (not (string-equal (downcase host) "localhost")))
      (user-error "douban: 不支持带远程 host 的 file URL：%s" source))
     (t
      (let ((encoded-path
             (if scheme (url-filename parsed) reference)))
        (when (string-empty-p encoded-path)
          (user-error "douban: 图片 src 缺少本地路径"))
        (expand-file-name
         (decode-coding-string
          (url-unhex-string encoded-path) 'utf-8)
         base-directory))))))

(defun douban--image-source (source base-directory)
  "解析 SOURCE，并返回其上传描述 plist。"
  (if-let* ((data-image (douban--decode-image-data-url source)))
      (list
       :mime (car data-image)
       :bytes (cdr data-image))
    (if-let* ((path (douban--local-image-path source base-directory)))
        (let* ((bytes (douban--read-file-bytes path))
               (mime (mailcap-file-name-to-mime-type path)))
          (list
           :mime mime
           :bytes bytes))
      (list :url source))))

(defun douban--image-id-from-url (url)
  "尽力从 URL 中提取豆瓣图片 ID。"
  (when
      (string-match
       "\\(?:^\\|[^0-9]\\)\\([0-9]+\\)\\.[[:alnum:]]+\\(?:[?#].*\\)?\\'"
       url)
    (match-string 1 url)))

(defun douban--topic-photo-id-from-url (url)
  "从已上传的 topic 图片 URL 中提取图片 ID。
只有 `group_topic' 命名空间的豆瓣 CDN URL 才属于 topic 图片；其他豆瓣
CDN 图片仍须先交给抓图端点注册。"
  (when (douban--douban-image-url-p url)
    (let ((path
           (url-filename
            (url-generic-parse-url url))))
      (when
          (string-match
           (concat
            "\\`/view/group_topic/[^/?#]+/public/p"
            "\\([1-9][0-9]*\\)\\.[[:alnum:]]+"
            "\\(?:[?#].*\\)?\\'")
           path)
        (match-string 1 path)))))

(defun douban--normalized-image-data
    (url caption &optional photo kind)
  "为 KIND 正文中的 URL、CAPTION 和 PHOTO 构造图片数据。"
  (let* ((topic-p (douban--topic-kind-p kind))
         (photo-id
          (and
           photo
           (douban--value-string (plist-get photo :id))))
         (id
          (or
           photo-id
           (if topic-p
               (douban--topic-photo-id-from-url url)
             (douban--image-id-from-url url))))
         (thumb
          (and
           (not topic-p)
           photo
           (douban--metadata-text
            "photo thumb" (plist-get photo :thumb))))
         (data
          (if topic-p
              (list
               :id id
               :src url
               :raw_src url
               :caption (or caption ""))
            (append
             (list
              :src url
              :url url
              :thumb (or thumb url)
              :caption (or caption ""))
             (when id (list :id id))))))
    (if topic-p
        (progn
          (unless id
            (error "douban: topic 图片缺少可用 ID"))
          (dolist
              (field '(:width :height :primary_color :is_animated))
            (when (and photo (plist-member photo field))
              (setq
               data
               (append
                data (list field (plist-get photo field)))))))
      (when (and thumb (not (douban--https-url-p thumb)))
        (error "douban: 图片响应 thumb 不是 HTTPS URL")))
    data))

(defun douban--rewrite-draft-images (raw session base-directory)
  "上传并改写 RAW 中的 IMAGE entity。
使用 SESSION，以 BASE-DIRECTORY 为基准解析本地路径。"
  (let ((entities (douban--draft-referenced-entities raw)))
    (dolist (entity entities)
      (when (string-equal (plist-get entity :type) "IMAGE")
        (let* ((data (plist-get entity :data))
               (source (plist-get data :src))
               (caption (or (plist-get data :caption) ""))
               url
               photo)
          (let ((hosted
                 (and
                  (douban--douban-image-url-p source)
                  (or
                   (not
                    (douban--topic-kind-p
                     (douban--session-kind session)))
                   (douban--topic-photo-id-from-url source)))))
            (if hosted
                (setq url source)
              (let ((descriptor
                     (douban--image-source source base-directory)))
                (message "douban: 上传图片...")
                (setq
                 photo
                 (if-let* ((bytes (plist-get descriptor :bytes)))
                     (douban--upload-image-bytes
                      session bytes
                      (plist-get descriptor :mime))
                   (douban--upload-image-url
                    session (plist-get descriptor :url))))
                (setq
                 url
                 (douban--photo-url
                  photo (douban--session-kind session))))))
          (setf
           (plist-get entity :data)
           (douban--normalized-image-data
            url caption photo (douban--session-kind session))))))
    raw))

;;;; Status mutation

(defconst douban--topic-post-endpoint
  "https://m.douban.com/rexxar/api/v2/topic/post"
  "创建当前 topic 内容的端点。")

(defconst douban--topic-update-endpoint-format
  "https://m.douban.com/rexxar/api/v2/group/topic/%s/post"
  "更新已有 topic 内容的端点格式。")

(defconst douban--status-home-url "https://www.douban.com/"
  "广播发布页与登录用户首页。")

(defconst douban--status-delete-endpoint
  "https://www.douban.com/j/status/delete"
  "删除首页广播的网页端点。")

(defun douban--global-upload-token (html)
  "从豆瓣页面 HTML 的全局导航状态中读取图片上传 token。"
  (let ((case-fold-search t))
    (when
        (string-match
         (concat
          "upload_auth_token[ \t]*\\(?:=\\|:\\)[ \t]*"
          "['\"]\\([^'\"]+\\)['\"]")
         html)
      (douban--metadata-text
       "upload_auth_token" (match-string 1 html)))))

(defun douban--topic-api-session (kind referer)
  "建立 KIND 的 topic API 会话，并把 REFERER 绑定为写请求来源。"
  (let ((session
         (douban--cookie-session
          kind douban--topic-post-endpoint)))
    (setf (douban--session-referer session) referer)
    session))

(defun douban--status-api-session (referer)
  "建立普通广播 API 会话，并把 REFERER 绑定为写请求来源。"
  (douban--topic-api-session 'status referer))

(defun douban--topic-page-context
    (kind referer ck images-p label)
  "读取 KIND 的 REFERER 页面，返回 `(SESSION . HTML)'。
页面 SESSION 只用于 www 页面和图片端点，不与 topic API Cookie 合并。
IMAGES-P 非 nil 时读取页面提供的上传凭据。"
  (let* ((session
          (douban--browser-session kind referer ck))
         (html (douban--read-html-page referer session label))
         (token
          (and images-p (douban--global-upload-token html))))
    (when images-p
      (setf
       (douban--session-state session)
       (list
        :upload-field "upload_auth_token"
        :upload-token token)))
    (cons session html)))

(defun douban--status-page-context
    (referer ck images-p label)
  "读取普通广播的 REFERER 页面，返回 `(SESSION . HTML)'。"
  (douban--topic-page-context
   'status referer ck images-p label))

(defun douban--draft-json-string (raw)
  "把 Draft.js RAW 序列化为可嵌入外层 JSON 的字符串。"
  (decode-coding-string
   (json-serialize
    raw
    :null-object :json-null
    :false-object :json-false)
   'utf-8 t))

(defun douban--topic-image-ids (raw &optional photos)
  "返回 RAW 中 IMAGE entity 按正文次序组成的 topic image_ids。
PHOTOS 是已有图片；其中的 `seq_id' 会在更新时保留。"
  (let ((occurrences (douban--draft-entity-occurrences raw))
        (sequences (make-hash-table :test 'equal))
        (next-sequence 0)
        ids)
    (dolist (photo (append photos nil))
      (let* ((id
              (douban--metadata-id
               "已有 topic 图片 id"
               (douban--value-string (plist-get photo :id))))
             (sequence
              (string-to-number
               (douban--metadata-id
                "已有 topic 图片 seq_id"
                (douban--value-string
                 (plist-get photo :seq_id))))))
        (puthash id sequence sequences)
        (setq next-sequence (max next-sequence sequence))))
    (dolist (occurrence occurrences)
      (let ((entity (plist-get occurrence :entity)))
        (when (equal (plist-get entity :type) "IMAGE")
          (push (plist-get (plist-get entity :data) :id) ids))))
    (setq ids (nreverse ids))
    (mapconcat
     #'identity
     (mapcar
      (lambda (id)
        (let ((sequence (gethash id sequences)))
          (unless sequence
            (setq sequence (cl-incf next-sequence)))
          (format "%d_%s" sequence id)))
     ids)
     ",")))

(defun douban--status-explanation-type-string (value)
  "从广播编辑状态 VALUE 读取单项内容说明。"
  (when (or (vectorp value) (listp value))
    (setq value (car (append value nil))))
  (if (memq value '(nil :json-null))
      ""
    (or
     (douban--metadata-text "广播 explanation_types" value)
     "")))

(defun douban--status-interest-tags (value)
  "把编辑状态 VALUE 中的兴趣标签名称连接成提交字符串。"
  (when (eq value :json-null)
    (setq value nil))
  (unless (or (vectorp value) (listp value))
    (error "douban: 广播编辑状态 interest_tags 不是标签列表"))
  (mapconcat
   (lambda (tag)
     (or
      (and
       (listp tag)
       (douban--metadata-text
        "广播兴趣标签名称" (plist-get tag :name)))
      (error "douban: 广播编辑状态含有无效的兴趣标签")))
   (append value nil)
   "#"))

(defun douban--default-reply-limit-protocol-value ()
  "返回 `douban-default-reply-limit' 对应的豆瓣协议值。"
  (pcase douban-default-reply-limit
    ('all "A")
    ('following "F")
    (_
     (error
      "douban: 无效的 douban-default-reply-limit：%S"
      douban-default-reply-limit))))

(defun douban--status-request-body
    (raw anthology-id topic-id &optional state meta)
  "返回写入 Draft.js RAW 广播的 JSON 正文。
ANTHOLOGY-ID 为 nil 时沿用 STATE 的文集；TOPIC-ID 非 nil 表示更新已有
广播。STATE 是已有广播的编辑状态。META 中显式出现的内容说明覆盖
STATE；回复范围创建时使用全局默认值，更新时保留现有设置。"
  (let* ((image-ids
          (douban--topic-image-ids
           raw (plist-get state :photos)))
         (anthology-id
          (or anthology-id (plist-get state :anthology-id)))
         (accessible
          (or (plist-get state :accessible) "public"))
         (reply-limit
          (if topic-id
              (or
               (plist-get state :reply-limit)
               (douban--default-reply-limit-protocol-value))
            (douban--default-reply-limit-protocol-value)))
         (original
          (cond
           ((and topic-id (plist-member state :original))
            (plist-get state :original))
           ((and (not topic-id) douban-default-original)
            t)
           (t :json-false)))
         (explanation-types
          (if (and meta (plist-member meta :explanation-types))
              (let ((value (plist-get meta :explanation-types)))
                (if (equal value "none")
                    (if topic-id "N" "")
                  (douban--metadata-protocol-value
                   :explanation-types value)))
            (douban--status-explanation-type-string
             (plist-get state :explanation-types))))
         (payload
          (if topic-id
              (list
               :content (douban--draft-json-string raw)
               :video_info (plist-get state :video-info)
               :image_ids image-ids
               :topic_tag_ids ""
               :interest_tags
               (or (plist-get state :interest-tags) "")
               :is_event :json-false
               :subtype "personal"
               :reply_limit reply-limit
               :accessible accessible
               :explanation_types explanation-types
               :send_status t
               :original original
               :is_activity_rule :json-false
               :enable_item_tag :json-false)
            (list
             :content
             (douban--draft-json-string raw)
             :image_ids image-ids
             :interest_tags ""
             :subtype "personal"
             :group_id "0"
             :accessible accessible
             :reply_limit reply-limit
             :send_status t
             :explanation_types explanation-types
             :original original))))
    (when anthology-id
      (setq payload
            (append payload (list :anthology_id anthology-id))))
    (unless (string-empty-p image-ids)
      (setq payload
            (append
             payload
             (list
              :image_layout
              (or
               (plist-get state :image-layout)
               "vertical")))))
    (json-serialize
     payload
     :null-object :json-null
     :false-object :json-false)))

(defun douban--review-broadcast-sid (html review-id)
  "从首页 HTML 返回唯一对应 REVIEW-ID 的长评广播 sid。
只接受网页标记为 review activity 的完整正整数标识；没有唯一匹配时返回
nil，以免误删无关广播。"
  (let ((review-id (douban--metadata-id "评论 ID" review-id))
        matches)
    (unless review-id
      (error "douban: 删除评论广播需要非空评论 ID"))
    (dolist
        (item
         (dom-by-class
          (douban--parse-html html) "status-item"))
      (let ((sid (dom-attr item 'data-sid)))
        (when
            (and
             (stringp sid)
             (string-match-p "\\`[1-9][0-9]*\\'" sid)
             (equal (dom-attr item 'data-action) "7")
             (equal (dom-attr item 'data-object-kind) "1012")
             (equal (dom-attr item 'data-object-id) review-id))
          (push sid matches))))
    (and (= (length matches) 1) (car matches))))

(defun douban--remove-created-review-broadcast (review-session review-id)
  "删除 REVIEW-SESSION 刚创建的 REVIEW-ID 所对应广播。
先从登录用户首页唯一核对 sid，再通过同源网页端点删除；任何无法确认的情况
都报错而不猜测目标。"
  (let* ((ck (douban--ensure-ck review-session))
         (home-session
          (douban--browser-session
           'status douban--status-home-url ck))
         (attempt 0)
         home-status
         sid)
    ;; 新建评论的广播是服务端副作用；只重试读取首页，删除请求始终至多一次。
    (while (and (< attempt 3) (not sid))
      (cl-incf attempt)
      (sleep-for 0.3)
      (let ((home-response
             (douban--http
              "GET" douban--status-home-url
              :session home-session
              :extra-headers
              '(("Accept" . "text/html,application/xhtml+xml")
                ("Cache-Control" . "no-cache")))))
        (setq home-status (plist-get home-response :status))
        (unless
            (and
             (integerp home-status)
             (<= 200 home-status 299))
          (error
           "首页返回 HTTP %s，无法核对评论 %s 的广播"
           home-status review-id))
        (setq sid
              (douban--review-broadcast-sid
               (plist-get home-response :body) review-id))))
    (unless sid
      (error
       "连续 %s 次读取首页仍没有唯一匹配评论 %s 的广播，未执行删除"
       attempt review-id))
    (let* ((response
            (douban--content-mutation-request
             home-session douban--status-delete-endpoint
             (douban--form-encode
              `(("sid" . ,sid)
                ("ck" . ,(douban--session-ck home-session))))
             "application/x-www-form-urlencoded; charset=UTF-8"
             (douban--mutation-headers home-session)
             :create-p nil))
           (json (plist-get response :json)))
      (douban--require-mutation-success
       response nil "评论广播删除" nil)
      (unless (and (listp json) (plist-member json :r))
        (error
         "douban: 评论广播删除响应无法确认成功：%s"
         (douban--response-detail response)))
      (unless (equal (plist-get json :r) 0)
        (user-error
         "douban: 评论广播删除被拒绝：%s"
         (douban--response-detail response))))
    (message "douban: 已删除评论对应广播 %s" sid)
    sid))

(defun douban--status-result-not-checkpointed (detail)
  "以 DETAIL 报告广播已提交但无法写回 topic ID。"
  (signal
   'douban-published-but-not-checkpointed
   (list
    (concat
     "豆瓣已经接受广播发布请求，但响应中没有合法的 topic ID，"
     "因此没有写回源稿。请到自己的豆瓣主页记录广播链接，不要重复发布。"
     "原错误：" detail))))

(defun douban--require-mutation-success
    (response create-p label unknown-guidance)
  "要求 RESPONSE 的 HTTP 状态表示 LABEL 成功。
CREATE-P 非 nil 时，除 408 外的明确 4xx 表示请求被拒绝；其他非 2xx 状态
不能证明远端没有创建对象，使用 UNKNOWN-GUIDANCE 报告结果不确定。"
  (let* ((status (plist-get response :status))
         (detail (douban--response-detail response))
         (rejected-p
          (and
           (integerp status)
           (<= 400 status 499)
           (/= status 408))))
    (cond
     ((and (integerp status) (<= 200 status 299))
      response)
     ((and create-p (not rejected-p))
      (douban--signal-create-result-unknown
       (format
        "%s返回 HTTP %s，结果可能已经成功。%s响应：%s"
        label status unknown-guidance detail)))
     (t
      (user-error
       "douban: %s失败（HTTP %s）：%s"
       label status detail)))))

(defun douban--status-create-result (response)
  "校验广播创建 RESPONSE，并返回响应中的 topic ID。"
  (douban--require-mutation-success
   response t "广播发布"
   "请先到自己的豆瓣主页检查。")
  (let* ((json (plist-get response :json))
         (topic-id
          (and
           json
           (douban--value-string
            (plist-get json :id)))))
    (unless
        (and topic-id
             (string-match-p "\\`[1-9][0-9]*\\'" topic-id))
      (douban--status-result-not-checkpointed
       (douban--response-detail response)))
    (list :id topic-id)))

(defun douban--status-update-result (response meta)
  "校验广播更新 RESPONSE，并返回 META 中已有的广播标识。"
  (douban--require-mutation-success
   response nil "广播更新" nil)
  (let ((body (plist-get response :body)))
    (unless
        (douban--javascript-truthy-response-body-p body)
      (error
       "douban: 广播更新返回空数据，无法确认更新成功"))
    (list
     :id (plist-get meta :status-id))))

(defun douban--javascript-truthy-response-body-p (body)
  "若 BODY 对应 JavaScript 中的 truthy 响应数据，则返回非 nil。"
  (when (and (stringp body)
             (not (string-empty-p (string-trim body))))
    (let ((value
           (condition-case nil
               (json-parse-string
                body
                :object-type 'hash-table
                :array-type 'array
                :null-object :json-null
                :false-object :json-false)
             (error :not-json))))
      (not
       (or
        (memq value '(:json-null :json-false))
        (and (stringp value) (string-empty-p value))
        (and (numberp value) (zerop value)))))))

(defun douban--topic-edit-json (html regexp label)
  "返回 HTML 中首个匹配 REGEXP 的 LABEL 编辑状态 JSON。"
  (condition-case nil
      (progn
        (unless (string-match regexp html)
          (user-error "douban: 无法读取%s编辑页状态" label))
        (json-parse-string
         (match-string 1 html)
         :object-type 'plist :array-type 'array
         :null-object :json-null :false-object :json-false))
    (error
     (user-error "douban: 无法读取%s编辑页状态" label))))

(defun douban--status-edit-json (html regexp)
  "返回 HTML 中首个匹配 REGEXP 的广播编辑状态 JSON。"
  (douban--topic-edit-json html regexp "广播"))

(defun douban--status-edit-state (html topic-id)
  "从广播编辑页 HTML 读取并校验 TOPIC-ID 的现有编辑状态。"
  (let* ((topic
          (douban--status-edit-json
           html
           (concat
            "^[ \t]*__INIT_STATE__\\.topic = "
            "\\({.*}\\)[ \t]*;?[ \t]*$")))
         (photos
          (douban--status-edit-json
           html
           (concat
            "^[ \t]*__INIT_STATE__\\.topic\\.photos = "
            "\\(\\[.*\\]\\)[ \t]*;?[ \t]*$")))
         (original (plist-get topic :is_original))
         (video-info (plist-get topic :video_info))
         (state
          (list
           :photos photos
           :image-layout
           (unless (eq (plist-get topic :image_layout) :json-null)
             (douban--metadata-text
              "广播 image_layout" (plist-get topic :image_layout)))
           :reply-limit
           (douban--metadata-text
            "广播 reply_limit" (plist-get topic :reply_limit))
           :accessible
           (douban--metadata-text
            "广播 accessible" (plist-get topic :accessible))
           :interest-tags
           (douban--status-interest-tags (plist-get topic :interest_tags))
           :explanation-types
           (douban--status-explanation-type-string
            (plist-get topic :explanation_types))
           :original (if (eq original t) t :json-false)
           :video-info video-info
           :anthology-id
           (unless (memq (plist-get topic :anthology_id) '(nil :json-null))
             (douban--metadata-id
              "广播 anthology_id"
              (douban--value-string (plist-get topic :anthology_id)))))))
    (unless
        (and
         (equal (douban--value-string (plist-get topic :id)) topic-id)
         (equal (plist-get topic :subtype) "personal")
         (vectorp photos)
         (cl-every
          (lambda (field) (plist-member topic field))
          '(:reply_limit :accessible :interest_tags
            :explanation_types :is_original :video_info))
         (plist-get state :reply-limit)
         (plist-get state :accessible)
         (memq original '(t :json-false :json-null))
         (or (eq video-info :json-null) (listp video-info)))
      (user-error
       "douban: 广播编辑页不是对应的个人广播或缺少更新状态"))
    state))

(defun douban--status-sessions (meta images-p)
  "根据 META 返回普通广播的 `(API-SESSION . UPLOAD-SESSION)'。
已有广播总会读取编辑状态；IMAGES-P 非 nil 时返回独立的网页上传会话。"
  (let* ((topic-id (plist-get meta :status-id))
         (referer
          (if topic-id
              (format "https://www.douban.com/topic/%s/edit" topic-id)
            douban--status-home-url))
         (api-session (douban--status-api-session referer))
         (page
          (when (or topic-id images-p)
           (douban--status-page-context
            referer (douban--session-ck api-session) images-p
            (if topic-id "广播编辑页" "广播发布页")))))
    (when topic-id
      (setf
       (douban--session-state api-session)
       (douban--status-edit-state (cdr page) topic-id)))
    (cons api-session (and images-p (car page)))))

(defun douban--submit-status (meta session raw)
  "通过 SESSION 根据 META 和 RAW 创建或更新普通豆瓣广播。
创建请求的结果不确定时绝不重试；更新请求只写入 META 指向的原广播。"
  (let* ((topic-id (plist-get meta :status-id))
         (endpoint
          (if topic-id
              (format
               douban--topic-update-endpoint-format
               (url-hexify-string topic-id))
            douban--topic-post-endpoint))
         (body
          (douban--status-request-body
           raw
           (plist-get meta :anthology-id)
           topic-id
           (douban--session-state session)
           meta))
         (response
          (douban--content-mutation-request
           session endpoint body
           "application/json;charset=utf-8"
           `(("Accept" . "application/json")
             ("X-CSRF-TOKEN" .
              ,(douban--session-ck session))
             ("Referer" .
              ,(douban--session-referer session))
             ("Origin" . "https://www.douban.com"))
           :create-p (not topic-id)
           :unknown-message
           (concat
            "豆瓣广播创建请求中断，结果可能已经成功。"
            "请先到自己的豆瓣主页检查，"
            "确认没有新广播后再重试。"))))
    (if topic-id
        (douban--status-update-result response meta)
      (douban--status-create-result response))))

;;;; Book annotation mutation

(defconst douban--annotation-create-url-format
  "https://www.douban.com/topic/create?subject_id=%s&subtype=annotation"
  "创建当前新式读书笔记的网页入口格式。")

(defconst douban--annotation-hobbit-mapping-endpoint-format
  (concat
   "https://m.douban.com/rexxar/api/v2/hobbit/mapping"
   "?source_type=book&source_id=%s")
  "查询图书对应读书活动标签的端点格式。")

(defconst douban--annotation-subject-endpoint-format
  "https://m.douban.com/rexxar/api/v2/book/%s"
  "新式读书笔记编辑器读取图书条目的端点格式。")

(defun douban--canonical-annotation-url (annotation-id)
  "返回新式读书笔记 ANNOTATION-ID 的规范 topic URL。"
  (format "https://www.douban.com/topic/%s/" annotation-id))

(defun douban--annotation-create-url (subject-id)
  "返回图书 SUBJECT-ID 对应的新式读书笔记创建入口。"
  (format
   douban--annotation-create-url-format
   (url-hexify-string subject-id)))

(defun douban--annotation-hobbit-tag (session subject-id)
  "尽力使用 SESSION 读取图书 SUBJECT-ID 的活动标签名称。
当前网页编辑器把这个请求当作可选增强；请求失败或响应没有合法名称时返回
nil，不影响读书笔记本身的创建或更新。"
  (condition-case nil
      (let* ((response
              (douban--http-json
               "GET"
               (format
                douban--annotation-hobbit-mapping-endpoint-format
                (url-hexify-string subject-id))
               :session session
               :extra-headers
               `(("Accept" . "application/json")
                 ("Referer" . ,(douban--session-referer session))
                 ("Origin" . "https://www.douban.com"))))
             (status (plist-get response :status))
             (json (plist-get response :json)))
        (and
         (integerp status)
         (<= 200 status 299)
         (listp json)
         (douban--metadata-text
          "读书活动标签" (plist-get json :hobbit_name))))
    (error nil)))

(defun douban--annotation-require-subject (session subject-id)
  "使用 SESSION 要求 SUBJECT-ID 是当前可读取的豆瓣图书。"
  (let* ((response
          (douban--http-json
           "GET"
           (format
            douban--annotation-subject-endpoint-format
            (url-hexify-string subject-id))
           :session session
           :extra-headers
           `(("Accept" . "application/json")
             ("Referer" . ,(douban--session-referer session))
             ("Origin" . "https://www.douban.com"))))
         (status (plist-get response :status))
         (json (plist-get response :json)))
    (unless
        (and
         (integerp status)
         (<= 200 status 299)
         (listp json)
         (equal
          (douban--value-string (plist-get json :id))
          subject-id)
         (equal (plist-get json :type) "book"))
      (user-error
       "douban: 无法确认读书笔记图书 %s（HTTP %s）：%s"
       subject-id status (douban--response-detail response)))
    json))

(defun douban--annotation-edit-state (html meta)
  "从 HTML 读取并校验 META 指向的新式读书笔记编辑状态。"
  (let* ((annotation-id (plist-get meta :annotation-id))
         (subject-id (plist-get meta :subject-id))
         (topic
          (douban--topic-edit-json
           html
           (concat
            "^[ \t]*__INIT_STATE__\\.topic = "
            "\\({.*}\\)[ \t]*;?[ \t]*$")
           "读书笔记"))
         (photos
          (douban--topic-edit-json
           html
           (concat
            "^[ \t]*__INIT_STATE__\\.topic\\.photos = "
            "\\(\\[.*\\]\\)[ \t]*;?[ \t]*$")
           "读书笔记"))
         (remote-subject (plist-get topic :subject))
         (remote-subject-id
          (douban--value-string
           (or
            (plist-get topic :subject_id)
            (and
             (listp remote-subject)
             (plist-get remote-subject :id)))))
         (original (plist-get topic :is_original))
         (video-info (plist-get topic :video_info))
         (accessible
          (douban--metadata-text
           "读书笔记 accessible" (plist-get topic :accessible)))
         (reply-limit
          (douban--metadata-text
           "读书笔记 reply_limit" (plist-get topic :reply_limit)))
         (state
          (list
           :photos photos
           :image-layout
           (unless (eq (plist-get topic :image_layout) :json-null)
             (douban--metadata-text
              "读书笔记 image_layout" (plist-get topic :image_layout)))
           :reply-limit reply-limit
           :accessible accessible
           :interest-tags
           (douban--status-interest-tags (plist-get topic :interest_tags))
           :explanation-types
           (douban--status-explanation-type-string
            (plist-get topic :explanation_types))
           :original (if (eq original t) t :json-false)
           :video-info video-info
           :anthology-id
           (unless
               (memq
                (plist-get topic :anthology_id)
                '(nil :json-null))
             (douban--metadata-id
              "读书笔记 anthology_id"
              (douban--value-string
               (plist-get topic :anthology_id)))))))
    (unless
        (and
         (equal
          (douban--value-string (plist-get topic :id))
          annotation-id)
         (equal (plist-get topic :subtype) "annotation")
         (equal remote-subject-id subject-id)
         (vectorp photos)
         (cl-every
          (lambda (field) (plist-member topic field))
          '(:reply_limit :accessible :interest_tags
            :explanation_types :is_original :video_info))
         (member accessible '("public" "private"))
         (member reply-limit '("A" "F" "N"))
         (memq original '(t :json-false :json-null))
         (or (eq video-info :json-null) (listp video-info)))
      (user-error
       (concat
        "douban: 读书笔记编辑页不是对应图书的 annotation，"
        "或缺少更新状态")))
    state))

(defun douban--annotation-request-body (raw meta &optional state)
  "返回根据 RAW、META 和可选编辑 STATE 构造的读书笔记 JSON 正文。"
  (let* ((annotation-id (plist-get meta :annotation-id))
         (update-p (and annotation-id t))
         (image-ids
          (douban--topic-image-ids
           raw (plist-get state :photos)))
         (privacy-specified-p
          (plist-member meta :annotation-privacy))
         (state-accessible (plist-get state :accessible))
         (state-reply-limit (plist-get state :reply-limit))
         (accessible
          (if privacy-specified-p
              (douban--metadata-protocol-value
               :annotation-privacy
               (plist-get meta :annotation-privacy))
            (or (plist-get state :accessible) "public")))
         (reply-limit
          (cond
           ((equal accessible "private")
            "N")
           ((and
             update-p privacy-specified-p
             (equal state-accessible "private"))
            (douban--default-reply-limit-protocol-value))
           (update-p
            (or
             state-reply-limit
             (douban--default-reply-limit-protocol-value)))
           (t (douban--default-reply-limit-protocol-value))))
         (original
          (cond
           ((and update-p (plist-member state :original))
            (plist-get state :original))
           (douban-default-original t)
           (t :json-false)))
         (explanation-types
          (if (plist-member meta :explanation-types)
              (let ((value (plist-get meta :explanation-types)))
                (if (equal value "none")
                    "N"
                  (douban--metadata-protocol-value
                   :explanation-types value)))
            (douban--status-explanation-type-string
             (plist-get state :explanation-types))))
         (payload
          (list
           :title (plist-get meta :title)
           :content (douban--draft-json-string raw)
           :image_ids image-ids
           :topic_tag_ids ""
           :interest_tags (or (plist-get state :interest-tags) "")
           :subtype "annotation"
           :subject_id (plist-get meta :subject-id)
           :accessible accessible
           :reply_limit reply-limit
           :explanation_types explanation-types
           :send_status
           (if douban-review-send-broadcast t :json-false)
           :original original
           :is_event :json-false
           :is_activity_rule :json-false
           :enable_item_tag :json-false)))
    (when
        (or
         (and (equal accessible "private")
              (not (equal reply-limit "N")))
         (and (equal accessible "public")
              (equal reply-limit "N")))
      (user-error
       (concat
        "douban: 公开读书笔记只接受 all/following 回复范围，"
        "私密读书笔记必须使用 none")))
    (when update-p
      (setq payload
            (append
             payload
             (list :video_info (plist-get state :video-info)))))
    (when-let* ((hobbit-tag (plist-get state :hobbit-tag)))
      (setq payload
            (append payload (list :hobbit_tag hobbit-tag))))
    (when-let* ((anthology-id (plist-get state :anthology-id)))
      (setq payload
            (append payload (list :anthology_id anthology-id))))
    (unless (string-empty-p image-ids)
      (setq payload
            (append
             payload
             (list
              :image_layout
              (or (plist-get state :image-layout) "vertical")))))
    (json-serialize
     payload
     :null-object :json-null
     :false-object :json-false)))

(defun douban--annotation-sessions (meta images-p)
  "根据 META 返回新式读书笔记的 `(API-SESSION . UPLOAD-SESSION)'。"
  (let* ((annotation-id (plist-get meta :annotation-id))
         (subject-id (plist-get meta :subject-id))
         (referer
          (if annotation-id
              (format
               "https://www.douban.com/topic/%s/edit"
               annotation-id)
            (douban--annotation-create-url subject-id)))
         (api-session
          (douban--topic-api-session 'annotation referer))
         (_subject
          (douban--annotation-require-subject
           api-session subject-id))
         (page
          (when (or annotation-id images-p)
            (douban--topic-page-context
             'annotation referer
             (douban--session-ck api-session)
             images-p
             (if annotation-id
                 "读书笔记编辑页"
               "读书笔记创建页"))))
         (state
          (and annotation-id
               (douban--annotation-edit-state
                (cdr page) meta)))
         (hobbit-tag
          (douban--annotation-hobbit-tag
           api-session subject-id)))
    (when hobbit-tag
      (setq state (plist-put state :hobbit-tag hobbit-tag)))
    (setf (douban--session-state api-session) state)
    (cons api-session (and images-p (car page)))))

(defun douban--annotation-result-not-checkpointed (detail)
  "以 DETAIL 报告读书笔记已提交但无法可靠写回 ID。"
  (signal
   'douban-published-but-not-checkpointed
   (list
    (concat
     "豆瓣已经接受读书笔记创建请求，但响应中没有可用的 topic ID，"
     "或 topic ID 与 URL 不一致，"
     "因此没有写回源稿。请到该图书的读书笔记列表确认并记录链接，"
     "不要重复发布。原错误：" detail))))

(defun douban--annotation-create-result (response)
  "校验新式读书笔记创建 RESPONSE 并返回 topic 结果。"
  (douban--require-mutation-success
   response t "读书笔记发布"
   "请先到该图书的读书笔记列表检查。")
  (let* ((json (plist-get response :json))
         (response-id
          (and
           (listp json)
           (douban--value-string (plist-get json :id))))
         (url-value
          (and
           (listp json)
           (douban--value-string (plist-get json :url))))
         (url-result
          (and url-value
               (douban--content-result-from-url
                'annotation url-value
                "https://www.douban.com/")))
         (url-id (plist-get url-result :id))
         (valid-response-id
          (and
           response-id
           (string-match-p
            "\\`[1-9][0-9]*\\'" response-id)
           response-id))
         (id (or valid-response-id url-id)))
    (unless
        (and
         id
         (or (not response-id) valid-response-id)
         (or (not url-value) url-id)
         (or
          (not (and response-id url-value))
          (equal valid-response-id url-id)))
      (douban--annotation-result-not-checkpointed
       (douban--response-detail response)))
    (list
     :id id
     :url (douban--canonical-annotation-url id))))

(defun douban--annotation-update-result (response meta)
  "校验读书笔记更新 RESPONSE，并返回 META 中已有的 topic 标识。"
  (douban--require-mutation-success
   response nil "读书笔记更新" nil)
  (unless
      (douban--javascript-truthy-response-body-p
       (plist-get response :body))
    (error
     "douban: 读书笔记更新返回空数据，无法确认更新成功"))
  (let* ((id (plist-get meta :annotation-id))
         (json (plist-get response :json))
         (response-id
          (and
           (listp json)
           (douban--value-string (plist-get json :id))))
         (response-url
          (and
           (listp json)
           (douban--value-string (plist-get json :url))))
         (url-result
          (and
           response-url
           (douban--content-result-from-url
            'annotation response-url
            "https://www.douban.com/"))))
    (when
        (or
         (and response-id (not (equal response-id id)))
         (and response-url
              (not (equal (plist-get url-result :id) id))))
      (error
       "douban: 读书笔记更新响应指向了其它 topic，无法确认更新成功"))
    (list
     :id id
     :url (douban--canonical-annotation-url id))))

(defun douban--submit-annotation (meta session raw)
  "通过 SESSION 根据 META 和 RAW 创建或更新新式读书笔记。"
  (let* ((annotation-id (plist-get meta :annotation-id))
         (endpoint
          (if annotation-id
              (format
               douban--topic-update-endpoint-format
               (url-hexify-string annotation-id))
            douban--topic-post-endpoint))
         (body
          (douban--annotation-request-body
           raw meta (douban--session-state session)))
         (response
          (douban--content-mutation-request
           session endpoint body
           "application/json;charset=utf-8"
           `(("Accept" . "application/json")
             ("X-CSRF-TOKEN" . ,(douban--session-ck session))
             ("Referer" . ,(douban--session-referer session))
             ("Origin" . "https://www.douban.com"))
           :create-p (not annotation-id)
           :unknown-message
           (concat
            "豆瓣读书笔记创建请求中断，结果可能已经成功。"
            "请先到该图书的读书笔记列表检查，"
            "确认没有新笔记后再重试。"))))
    (if annotation-id
        (douban--annotation-update-result response meta)
      (douban--annotation-create-result response))))

;;;; Review mutation

(defun douban--review-form-fields (meta raw session title)
  "根据 META、RAW、SESSION 和 TITLE 构造当前网页编辑器的表单字段。"
  (let ((fields
         (list
            (cons "is_rich" "1")
            (cons "review[subject_id]"
                  (plist-get meta :subject-id))
            (cons "review[title]" title)
            (cons "review[introduction]"
                  (or (plist-get meta :introduction) ""))
            (cons "review[text]"
                  (json-serialize
                   raw
                   :null-object :json-null
                   :false-object :json-false))
            (cons "review[rating]"
                  (if-let* ((rating (plist-get meta :rating)))
                      (number-to-string rating)
                    ""))
            (cons "review[spoiler]"
                  (if (plist-get meta :spoiler) "on" ""))
            (cons "review[donate]"
                  (if (plist-get meta :donate) "on" ""))
            (cons "review[original]"
                  (if douban-default-original "on" ""))
            (cons "review[explanation_types]"
                  (or
                   (douban--metadata-protocol-value
                    :explanation-types
                    (plist-get meta :explanation-types))
                   ""))
            (cons "ck" (douban--session-ck session)))))
      (if
          (string-equal
           (douban--session-state-get session :app-name)
           "game")
          (let ((rtype
                 (if-let* ((source-rtype (plist-get meta :rtype)))
                     (douban--metadata-protocol-value
                      :rtype source-rtype)
                   (douban--session-state-get
                    session :rtype))))
            (unless (member rtype '("R" "G"))
              (user-error
               (concat
                "douban: 游戏评论缺少有效 rtype；"
                "请在 metadata 中设置 review 或 guide")))
            (setq
             fields
             (append
              fields
              (list (cons "review[rtype]" rtype))
              (mapcar
               (lambda (platform)
                 (cons "review[platforms]" platform))
               (plist-get meta :platforms)))))
        (when
            (or
             (plist-get meta :rtype)
             (plist-get meta :platforms))
          (user-error
           "douban: rtype/platforms 只适用于 game 评论")))
    fields))

(defun douban--json-error-value-p (value)
  "当 JSON VALUE 表示实际错误时返回非 nil。"
  (and
   (not (memq value '(nil :json-false :json-null)))
   (not (and (stringp value) (string-empty-p value)))))

(defun douban--review-mutation-result
    (response session expected-id)
  "根据 SESSION 和 EXPECTED-ID 校验 RESPONSE。
返回 `(:id ID :url URL)'。"
  (douban--require-mutation-success
   response (not expected-id)
   (if expected-id "评论更新" "评论创建")
   "请先检查是否已经发布。")
  (let* ((json (plist-get response :json))
         (detail (douban--response-detail response))
         (raw-errors (and json (plist-get json :errors)))
         (errors
          (and
           (douban--json-error-value-p raw-errors)
           raw-errors)))
    (when errors
      (user-error "douban: 发布失败：%s" errors))
    (let* ((url-result
            (douban--content-result-from-url
             'review
             (plist-get json :url)
             (douban--session-referer session)))
           (url (plist-get url-result :url))
           (id (plist-get url-result :id)))
      (unless url-result
        (if expected-id
            (error
             "douban: 更新响应缺少规范评论 URL：%S"
             (plist-get json :url))
          (douban--signal-create-result-unknown
           (concat
            "豆瓣评论创建响应缺少规范评论 URL；"
            "请先检查是否已经发布。响应：" detail))))
      (when (and expected-id (not (equal expected-id id)))
        (error
         "douban: 发布响应 URL 的 ID 不匹配（期望 %s，收到 %s）"
         expected-id id))
      (list :id id :url url))))

(defun douban--submit-review (meta raw session title)
  "通过 SESSION 提交带 TITLE 的 META 和 RAW。
创建或更新一篇评论，并返回变更结果 plist。"
  (let* ((review-id (plist-get meta :review-id))
         (path
          (if review-id
              (format
               "/j/review/%s/update"
               (url-hexify-string review-id))
            "/j/review/create"))
         (url
          (concat
           "https://" (douban--session-host session) path))
         (body
          (douban--form-encode
           (douban--review-form-fields
            meta raw session title)))
         (response
          (douban--content-mutation-request
           session url body
           "application/x-www-form-urlencoded; charset=UTF-8"
           (douban--mutation-headers session)
           :create-p (not review-id)
           :unknown-message
           (concat
            "豆瓣评论创建请求中断，结果可能已经成功。"
            "请先到自己的豆瓣主页检查，"
            "确认没有新长评后再重试。"))))
    (douban--review-mutation-result response session review-id)))

;;;; Note protocol

(defconst douban--note-publish-endpoint
  "https://www.douban.com/j/note/publish"
  "发布预分配日记的端点。")

(defun douban--canonical-note-url (note-id)
  "返回 NOTE-ID 的规范公开 URL。"
  (format "https://www.douban.com/note/%s/" note-id))

(defun douban--note-page-action (form html)
  "返回 FORM 或 HTML 中的日记提交 action。"
  (let ((case-fold-search nil))
    (or
     (douban--dom-input-value form "action")
     (when
         (string-match
          (concat "_POST_PARAMS[ \t\r\n]*=[ \t\r\n]*{[^;]*?"
                  "\\(?:[\"']action[\"']\\|action\\)[ \t\r\n]*:"
                  "[ \t\r\n]*[\"']\\([^\"']+\\)[\"']")
          html)
       (douban--value-string (match-string 1 html))))))

(defun douban--note-editor-state (html editor-url)
  "从 HTML 解析 EDITOR-URL 的日记编辑状态。"
  (let* ((document (douban--parse-html html))
         (form
          (cl-find-if
           (lambda (candidate)
             (douban--dom-inputs candidate "note_id"))
           (dom-by-tag document 'form)))
         (note-id
          (and form
               (douban--metadata-id
                "编辑页 note-id" (douban--dom-input-value form "note_id"))))
         (ck (and form (douban--dom-input-value form "ck")))
         (default-privacy
          (and form
               (douban--dom-input-choice-value form "note_privacy")))
         (action (and form (douban--note-page-action form html)))
         (credential (douban--upload-credential html)))
    (unless (and note-id ck default-privacy action)
      (user-error
       (concat "douban: 页面不是可用的日记编辑页；"
               "缺少 note_id、ck、选中的 note_privacy 或 action：%s")
       editor-url))
    (list
     :note-id note-id
     :ck ck
     :default-privacy default-privacy
     :action action
     :upload-field (car credential)
     :upload-token (cdr credential))))

(defun douban--note-session (meta)
  "根据规范化 META 读取并绑定日记编辑页会话。"
  (let* ((expected-id (plist-get meta :note-id))
         (editor-url
          (if expected-id
              (format "https://www.douban.com/note/%s/edit" expected-id)
            "https://www.douban.com/note/create"))
         (session (douban--browser-session 'note editor-url))
         (state
          (douban--note-editor-state
           (douban--read-html-page editor-url session "日记编辑页")
           editor-url))
         (note-id (plist-get state :note-id))
         (action (plist-get state :action))
         (ck (plist-get state :ck)))
    (when (and expected-id (not (equal note-id expected-id)))
      (user-error
       "douban: 日记编辑页 ID 不匹配（期望 %s，收到 %s）"
       expected-id note-id))
    (unless (or expected-id (string-equal action "new"))
      (user-error
       "douban: 日记创建页返回了更新 action=%s" action))
    (setf
     (douban--session-ck session) ck
     (douban--session-state session) state
     (douban--session-cookies session)
     (douban--cookie-put (douban--session-cookies session) "ck" ck))
    session))

(defun douban--require-title (title label maximum-length)
  "校验并返回 LABEL 内容的 TITLE。
标题按 UTF-16 code unit 计数，不能超过 MAXIMUM-LENGTH。"
  (unless
      (and
       (stringp title)
       (not (string-empty-p title)))
    (user-error "douban: %s缺少标题" label))
  (when
      (> (douban--utf16-length title) maximum-length)
    (user-error
     "douban: %s标题不能超过 %d 字"
     label maximum-length))
  title)

(defun douban--note-privacy-value (meta session)
  "返回 META 指定或 SESSION 编辑页默认的日记可见范围。"
  (or
   (douban--metadata-protocol-value
    :note-privacy (plist-get meta :note-privacy))
   (douban--session-state-get session :default-privacy)))

(defun douban--note-form-fields (meta raw session title privacy)
  "根据 META、RAW、SESSION、TITLE 和 PRIVACY 构建发布字段。"
  (let ((action
         (douban--session-state-get session :action)))
    (list
     (cons "is_rich" "1")
     (cons
      "note_id"
      (douban--session-state-get session :note-id))
     (cons "note_title" title)
     (cons
      "note_text"
      (json-serialize
       raw :null-object :json-null :false-object :json-false))
     (cons "introduction" "")
     (cons "note_privacy" privacy)
     (cons "cannot_reply"
           (if (plist-get meta :cannot-reply) "on" ""))
     (cons
      "author_tags"
      (mapconcat
       #'identity (or (plist-get meta :author-tags) nil) " "))
     (cons "accept_donation" "")
     (cons "donation_notice" "")
     (cons "is_original" "")
     (cons "ck" (douban--session-ck session))
     (cons "action" action))))

(defun douban--note-response-result
    (response session update-p)
  "校验 SESSION 对应的日记 RESPONSE。
UPDATE-P 非 nil 表示这次写操作更新的是已发布日记。"
  (douban--require-mutation-success
   response (not update-p)
   (if update-p "日记更新" "日记发布")
   "请打开对应日记或草稿检查。")
  (let* ((json (plist-get response :json))
         (detail (douban--response-detail response))
         (r-present (and json (plist-member json :r)))
         (r (and json (plist-get json :r)))
         (note-id
          (douban--session-state-get session :note-id))
         (returned
          (and
           json
           (douban--content-result-from-url
            'note
            (plist-get json :url)
            (douban--session-referer session))))
         (returned-url (plist-get returned :url)))
    (when (and
           r-present
           (not (memq r '(0 :json-false))))
      (user-error
       "douban: 日记发布失败：%s"
       (or
        (plist-get json :message)
        (plist-get json :err)
        r)))
    (unless (and r-present (memq r '(0 :json-false)))
      (if update-p
          (error
           "douban: 日记更新响应缺少合法的 r 成功标记：%s"
           detail)
        (douban--signal-create-result-unknown
         (concat
          "豆瓣日记发布响应缺少合法的 r 成功标记；"
          "请打开对应日记或草稿检查。响应："
          detail))))
    (list
     :id note-id
     :url (or returned-url (douban--canonical-note-url note-id)))))

(defun douban--submit-note (meta raw session title privacy)
  "通过 SESSION 创建或更新日记，并返回 ID 与 URL。"
  (let* ((update-p
          (not
           (string-equal
            (douban--session-state-get session :action)
            "new")))
         (body
          (douban--form-encode
           (douban--note-form-fields
            meta raw session title privacy)))
         (response
          (douban--content-mutation-request
           session douban--note-publish-endpoint body
           "application/x-www-form-urlencoded; charset=UTF-8"
           (douban--mutation-headers session)
           :create-p (not update-p)
           :unknown-message
           (concat
            "豆瓣日记发布请求中断，结果可能已经成功。note-id="
            (douban--session-state-get session :note-id)
            "；请打开对应日记或草稿检查，不要创建另一篇。"))))
    (douban--note-response-result response session update-p)))

;;;; Source files and publishing workflow

(defconst douban--anthology-create-endpoint
  "https://m.douban.com/rexxar/api/v2/doulist/create"
  "创建文集的当前 topic 编辑器端点。")

(defun douban--refresh-file-buffer (file)
  "刷新正在访问 FILE 且未修改的缓冲区。"
  (when-let* ((buffer
              (find-buffer-visiting (expand-file-name file))))
    (with-current-buffer buffer
      (unless (buffer-modified-p)
        (revert-buffer t t t)))))

(defun douban--checkpoint-meta (file meta)
  "把 META 持久化到 FILE，并刷新其访问缓冲区。"
  (douban--write-meta file meta)
  (douban--refresh-file-buffer file))

(defun douban--current-user-id ()
  "从当前浏览器豆瓣会话取得登录用户 ID。"
  (let* ((session
          (douban--browser-session
           'anthology douban--ck-bootstrap-url))
         (response
          (douban--http
           "GET" douban--ck-bootstrap-url
           :session session
           :allow-redirect-response t))
         (status (plist-get response :status))
         (location
          (cdr
           (assoc-string
            "location" (plist-get response :headers) t)))
         (url
          (and
           location
           (url-expand-file-name
            location douban--ck-bootstrap-url))))
    (unless
        (and
         (memq status '(301 302 303 307 308))
         (stringp url)
         (string-match
          (concat
           "\\`https://www\\.douban\\.com/people/"
           "\\([^/?#[:space:]]+\\)/?\\'")
          url))
      (user-error
       "douban: 无法从当前浏览器会话识别登录用户（HTTP %s）"
       status))
    (match-string 1 url)))

(defun douban--anthology-list-url (user-id start count &optional ck)
  "返回读取 USER-ID 从 START 开始 COUNT 个文集的 URL。
CK 非空时按当前网页请求附加同名查询参数。"
  (concat
   (format
    (concat
     "https://m.douban.com/rexxar/api/v2/user/%s/"
     "anthologies?start=%d&count=%d")
    (url-hexify-string user-id)
    start count)
   (when (and (stringp ck) (not (string-empty-p ck)))
     (concat "&ck=" (url-hexify-string ck)))))

(defun douban--anthologies (user-id &optional cookies)
  "读取 USER-ID 的全部豆瓣文集。
COOKIES 非 nil 时随请求发送，以便列出当前用户的私有文集。"
  (let ((ck
         (and
          cookies
          (cdr (assoc-string "ck" cookies))))
        (start 0)
        (count 50)
        total
        result)
    (while (or (null total) (< start total))
      (let* ((response
              (douban--read-json-endpoint
               (douban--anthology-list-url
                user-id start count ck)
               "https://www.douban.com/"
               :cookies cookies))
             (status (plist-get response :status))
             (json (plist-get response :json))
             (page (and json (plist-get json :doulists)))
             (response-start
              (and json (plist-get json :start)))
             (response-count
              (and json (plist-get json :count))))
        (unless (<= 200 status 299)
          (error "douban: 文集列表读取失败（HTTP %s）" status))
        (unless
            (and
             (integerp (plist-get json :total))
             (>= (plist-get json :total) 0)
             (integerp response-start)
             (= response-start start)
             (integerp response-count)
             (>= response-count 0)
             (listp page))
          (error "douban: 文集列表响应无效"))
        (setq total (plist-get json :total))
        (dolist (item page)
          (let ((id
                 (douban--value-string
                  (plist-get item :id)))
                (title
                 (douban--metadata-text
                  "文集标题" (plist-get item :title)))
                (items-count
                 (plist-get item :items_count)))
            (when
                (and
                 id
                 (string-match-p "\\`[1-9][0-9]*\\'" id)
                 title)
              (setq result
                    (append
                     result
                     (list
                      (list
                       :id id
                       :title title
                       :items-count
                       (if (integerp items-count)
                           items-count
                         0))))))))
        (when
            (and
             (< start total)
             (or (zerop response-count)
                 (null page)))
          (error "douban: 文集列表分页响应没有推进"))
        (setq start (+ response-start response-count))))
    result))

(defun douban--anthology-title (value)
  "校验并返回文集名称 VALUE。"
  (let ((title (douban--metadata-text "文集名称" value)))
    (unless title
      (user-error "douban: 文集名称不能为空"))
    (when (> (douban--utf16-length title) 20)
      (user-error "douban: 文集名称不能超过 20 个字符"))
    title))

(defun douban--anthology-cover (file)
  "读取文集封面 FILE，并返回 `(NAME MIME BYTES)'。"
  (let ((file (expand-file-name file)))
    (unless (and
             (file-regular-p file)
             (file-readable-p file))
      (user-error "douban: 文集封面必须是可读的普通文件：%s" file))
    (let* ((bytes (douban--read-file-bytes file))
           (mime
            (douban--image-mime
             (mailcap-file-name-to-mime-type file)
             bytes
             file)))
      (when (string-empty-p bytes)
        (user-error "douban: 文集封面不能为空：%s" file))
      (unless
          (and
           (equal mime "image/jpeg")
           (>= (length bytes) 3)
           (= (aref bytes 0) #xff)
           (= (aref bytes 1) #xd8)
           (= (aref bytes 2) #xff))
        (user-error
         "douban: 文集封面必须是 JPEG 图片：%s"
         file))
      (list
       (file-name-nondirectory file)
       mime bytes))))

(defun douban--anthology-create-session ()
  "建立创建文集所需的 m.douban.com 登录会话。"
  (let ((session
         (douban--cookie-session
          'anthology
          douban--anthology-create-endpoint)))
    (setf
     (douban--session-referer session)
     douban--status-home-url)
    session))

(defun douban--anthology-create-result (response expected-title)
  "校验文集创建 RESPONSE，并返回规范结果。
EXPECTED-TITLE 是提交的文集名称。"
  (douban--require-mutation-success
   response t "文集创建"
   "请先到自己的文集列表检查，不要直接重试。")
  (let* ((json (plist-get response :json))
         (detail (douban--response-detail response))
         (id
          (and
           (listp json)
           (douban--value-string
            (plist-get json :id)))))
    (unless
        (and id (string-match-p "\\`[1-9][0-9]*\\'" id))
      (douban--signal-create-result-unknown
       (concat
        "豆瓣文集创建响应没有可写回的 ID，结果可能已经成功。"
        "请先到自己的文集列表检查，不要直接重试。响应："
        detail)))
    (list
     :id id
     :title expected-title
     :url (format "https://www.douban.com/doulist/%s/" id)
     :cover-url
     (and (listp json) (plist-get json :cover_url)))))

(defun douban--invalidate-anthology-completion-caches ()
  "让所有现存源稿 buffer 下次补全时重新读取文集。"
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when
          (local-variable-p
           'douban--anthology-completion-cache
           buffer)
        (douban--reset-anthology-completion-cache)))))

(defun douban--create-anthology (title cover-file)
  "创建名为 TITLE、封面为 COVER-FILE 的公开豆瓣文集。
返回包含新文集 ID、名称和 URL 的规范结果 plist。"
  (let* ((title (douban--anthology-title title))
         (cover (douban--anthology-cover cover-file))
         (session (douban--anthology-create-session))
         (multipart
          (douban--multipart-body
           `(("title" . ,title)
             ("desc" . "")
             ("is_private" . "false")
             ("type" . "anthology"))
           :file-field "header_bg_image"
           :file-name (nth 0 cover)
           :file-mime (nth 1 cover)
           :file-bytes (nth 2 cover)))
         (request
          (lambda ()
            (douban--http-json
             "POST"
             douban--anthology-create-endpoint
             :body (cdr multipart)
             :content-type (car multipart)
             :extra-headers
             `(("Accept" . "application/json")
               ("X-CSRF-TOKEN" .
                ,(douban--session-ck session))
               ("Referer" . ,douban--status-home-url)
               ("Origin" . "https://www.douban.com"))
             :session session
             :raw-body t
             :allow-redirect-response t)))
         (response
          (douban--create-request
           request
           (concat
            "豆瓣文集创建请求中断，结果可能已经成功。"
            "请先到自己的文集列表检查，确认没有新文集后再重试。")))
         (result
          (douban--anthology-create-result
           response title)))
    (douban--invalidate-anthology-completion-caches)
    result))

;;;###autoload
(defun douban-new-anthology (title cover-file)
  "创建名称为 TITLE、封面为 COVER-FILE 的公开豆瓣文集。
豆瓣网页要求文集封面为方图，并把交互裁剪结果上传为 800×800 JPEG；
本命令直接上传 COVER-FILE，因此应预先准备 800×800 方形 JPEG。
成功时返回新文集的数字 ID。"
  (interactive
   (list
    (read-string "文集名称（最多 20 个字符）: ")
    (read-file-name
     "文集封面（800×800 方形 JPEG）: "
     default-directory nil t)))
  (let* ((result
          (douban--create-anthology
           title cover-file))
         (id (plist-get result :id))
         (url (plist-get result :url)))
    (message "douban: 已创建文集 %s" url)
    id))

(defconst douban--anthology-completion-cache-unloaded
  (make-symbol "douban-anthology-completion-cache-unloaded")
  "表示当前 buffer 尚未读取文集补全候选的哨兵。")

(defvar-local douban--anthology-completion-cache
  douban--anthology-completion-cache-unloaded
  "当前 buffer 缓存的登录账号文集；哨兵表示尚未读取。")

(defun douban--reset-anthology-completion-cache ()
  "清空当前 buffer 的文集补全缓存。"
  (setq
   douban--anthology-completion-cache
   douban--anthology-completion-cache-unloaded))

(defun douban--cached-anthologies ()
  "返回当前登录账号的文集，并在源稿 buffer 中缓存。"
  (if
      (eq
       douban--anthology-completion-cache
       douban--anthology-completion-cache-unloaded)
      (let ((user-id (douban--current-user-id)))
        (setq
         douban--anthology-completion-cache
         (douban--anthologies
          user-id
          (douban--read-browser-cookies
           (douban--anthology-list-url user-id 0 50)))))
    douban--anthology-completion-cache))

(defun douban--anthology-completion-candidates (anthologies)
  "把 ANTHOLOGIES 转为以文集名称为主的补全候选。
返回 `(LABEL . ANTHOLOGY)' 列表。重名文集才在名称后附加 ID，以便用户
能够选择确定的目标；篇数由补全旁注显示。"
  (let ((title-counts (make-hash-table :test 'equal))
        (raw-titles (make-hash-table :test 'equal))
        (used-labels (make-hash-table :test 'equal)))
    (dolist (anthology anthologies)
      (let ((title (plist-get anthology :title)))
        (puthash
         title
         (1+ (gethash title title-counts 0))
         title-counts)
        (puthash title t raw-titles)))
    (mapcar
     (lambda (anthology)
       (let ((title (plist-get anthology :title))
             (id (plist-get anthology :id))
             label)
         (setq
          label
          (if (> (gethash title title-counts 0) 1)
              (format "%s [%s]" title id)
            title))
         ;; 为所有不重名标题保留其原文。重名标题的区分后缀若恰好撞上另一个
         ;; 真实标题，就继续追加 ID，直到补全字符串全局唯一。
         (when (> (gethash title title-counts 0) 1)
           (while
               (or
                (gethash label raw-titles)
                (gethash label used-labels))
             (setq label (format "%s [%s]" label id))))
         (puthash label t used-labels)
         (cons label anthology)))
     anthologies)))

(defvar-local douban--subject-completion-cache nil
  "当前 buffer 最近一次条目补全搜索的缓存。
值为 `(:subject-type TYPE :query QUERY :subjects SUBJECTS)'；nil 表示尚未
搜索。空结果也保存在该结构中。")

(defvar-local douban--platform-completion-cache nil
  "当前 buffer 最近一次游戏平台补全的缓存。
值为 `(:subject-id ID :platforms PLATFORMS)'；nil 表示尚未读取。空结果也
保存在该结构中。")

(defun douban--reset-remote-metadata-completion-caches ()
  "清空当前 buffer 的条目与游戏平台补全缓存。"
  (setq
   douban--subject-completion-cache nil
   douban--platform-completion-cache nil))

(defun douban--cached-subjects (subject-type query)
  "返回 SUBJECT-TYPE 中 QUERY 的条目搜索结果，并在当前 buffer 缓存。
QUERY 必须已经规范化为非空字符串。"
  (if
      (and
       (equal
        (plist-get douban--subject-completion-cache :subject-type)
        subject-type)
       (equal
        (plist-get douban--subject-completion-cache :query)
        query))
      (plist-get douban--subject-completion-cache :subjects)
    (let ((subjects
           (douban--search-subjects query subject-type)))
      (setq
       douban--subject-completion-cache
       (list
        :subject-type subject-type
        :query query
        :subjects subjects))
      subjects)))

(defun douban--cached-game-platforms (subject-id)
  "返回游戏 SUBJECT-ID 的平台，并在当前 buffer 缓存。"
  (if
      (equal
       (plist-get douban--platform-completion-cache :subject-id)
       subject-id)
      (plist-get douban--platform-completion-cache :platforms)
    (let ((platforms (douban--game-platforms subject-id)))
      (setq
       douban--platform-completion-cache
       (list :subject-id subject-id :platforms platforms))
      platforms)))

(defun douban--subject-completion-candidates (subjects)
  "把 SUBJECTS 转换为 `(LABEL . SUBJECT)' 条目。"
  (mapcar
   (lambda (subject)
     (cons
      (douban--subject-candidate-label subject)
      subject))
   subjects))

(defun douban--platform-display-name (platform)
  "返回 PLATFORM 最适合展示给用户的名称。"
  (or
   (douban--metadata-text
    "platform.cn-name"
    (plist-get platform :cn-name))
   (douban--metadata-text
    "platform.name"
    (plist-get platform :name))
   (douban--metadata-text
    "platform.abbreviation"
    (plist-get platform :abbreviation))))

(defun douban--platform-completion-candidates (platforms)
  "把 PLATFORMS 转换为名称优先且标签唯一的补全条目。"
  (let ((counts (make-hash-table :test 'equal))
        (used (make-hash-table :test 'equal)))
    (dolist (platform platforms)
      (let ((name (douban--platform-display-name platform)))
        (puthash name (1+ (gethash name counts 0)) counts)))
    (mapcar
     (lambda (platform)
       (let* ((name (douban--platform-display-name platform))
              (id (plist-get platform :id))
              (label
               (if (> (gethash name counts 0) 1)
                   (format "%s [%s]" name id)
                 name)))
         (while (gethash label used)
           (setq label (format "%s [%s]" label id)))
         (puthash label t used)
         (cons label platform)))
     platforms)))

(defun douban--markdown-frontmatter-bounds ()
  "返回当前 buffer 的 Markdown front matter 内容边界。"
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "---[ \t]*\r?$")
      (forward-line 1)
      (let ((start (point)))
        (when (re-search-forward "^---[ \t]*\r?$" nil t)
          (cons start (line-beginning-position)))))))

(defun douban--metadata-source-index-entry (index kind)
  "返回 INDEX 中 KIND 的字段快照条目。"
  (cdr
   (assq
    kind
    (douban--metadata-source-index-entries index))))

(defun douban--metadata-source-index-ensure-entry (index kind)
  "返回 INDEX 中 KIND 的字段条目，必要时新建空条目。"
  (or
   (assq
    kind
    (douban--metadata-source-index-entries index))
   (let ((entry (list kind :fields nil :values nil)))
     (setf
      (douban--metadata-source-index-entries index)
      (append
       (douban--metadata-source-index-entries index)
       (list entry)))
     entry)))

(defun douban--metadata-source-index-add-kind (index kind)
  "把 KIND 及其空字段条目加入 INDEX。"
  (unless
      (memq kind (douban--metadata-source-index-kinds index))
    (setf
     (douban--metadata-source-index-kinds index)
     (append
      (douban--metadata-source-index-kinds index)
      (list kind))))
  (douban--metadata-source-index-ensure-entry index kind))

(defun douban--metadata-source-index-add-field
    (index kind field value-present-p value)
  "把 KIND 的 FIELD 和简单标量 VALUE 记录到 INDEX。
VALUE-PRESENT-P 为 nil 时只记录字段存在。重复字段沿用第一次出现的简单
标量；Markdown 的空值因而可以由后续非空重复字段补足。"
  (let* ((entry
          (douban--metadata-source-index-ensure-entry
           index kind))
         (properties (cdr entry))
         (fields (plist-get properties :fields))
         (values (plist-get properties :values)))
    (unless (memq field fields)
      (setq
       properties
       (plist-put
        properties :fields
        (append fields (list field)))))
    (when
        (and
         value-present-p
         (not (assq field values)))
      (setq
       properties
       (plist-put
        properties :values
        (append values (list (cons field value))))))
    (setcdr entry properties)))

(defun douban--metadata-source-index-fields (index kind)
  "返回 INDEX 中 KIND 已有的内部 metadata 字段。"
  (plist-get
   (douban--metadata-source-index-entry index kind)
   :fields))

(defun douban--metadata-source-index-field-value
    (index kind field)
  "返回 INDEX 中 KIND 的内部 FIELD 简单标量值。"
  (cdr
   (assq
    field
    (plist-get
     (douban--metadata-source-index-entry index kind)
     :values))))

(defun douban--yaml-simple-scalar (start end)
  "返回 START 与 END 之间简单 YAML 标量的可读文本。"
  (let ((text
         (string-trim
          (buffer-substring-no-properties start end))))
    (when
        (and
         (> (length text) 1)
         (memq (aref text 0) '(?\' ?\"))
         (eq
          (aref text 0)
          (aref text (1- (length text)))))
      (setq text (substring text 1 -1)))
    (unless (string-empty-p text)
      text)))

(defun douban--markdown-douban-buffer-region ()
  "返回当前 Markdown buffer 中 `douban' mapping 的绝对位置区间。"
  (when-let* ((frontmatter-bounds
               (douban--markdown-frontmatter-bounds))
              (frontmatter
               (buffer-substring-no-properties
                (car frontmatter-bounds)
                (cdr frontmatter-bounds)))
              (douban-region
               (condition-case nil
                   (douban--md-douban-region frontmatter)
                 (error nil))))
    (cons
     (+ (car frontmatter-bounds) (car douban-region))
     (+ (car frontmatter-bounds) (cdr douban-region)))))

(defun douban--markdown-direct-metadata-indentation (region)
  "返回 Markdown 的豆瓣 metadata 在 REGION 中使用的最小缩进。"
  (save-excursion
    (goto-char (car region))
    (forward-line 1)
    (let (indentation)
      (while (< (point) (cdr region))
        (unless
            (looking-at
             "[ \t]*\\(?:#\\|\r?$\\)")
          (let ((current (current-indentation)))
            (when
                (or (null indentation)
                    (< current indentation))
              (setq indentation current))))
        (forward-line 1))
      indentation)))

(defun douban--markdown-kind-containers (region indentation)
  "返回 REGION 中位于 INDENTATION 的可识别类型容器。"
  (when indentation
    (let (containers)
      (save-excursion
        (goto-char (car region))
        (forward-line 1)
        (while (< (point) (cdr region))
          (when
              (and
               (= (current-indentation) indentation)
               (looking-at
                (concat
                 "^[ \t]+\\("
                 (mapconcat
                  #'symbol-name
                  douban--metadata-source-kinds
                  "\\|")
                 "\\)[ \t]*:")))
            (push
             (list
              :kind
              (intern
               (match-string-no-properties 1))
              :line-start (line-beginning-position)
              :body-start (line-beginning-position 2)
              :end nil
              :indentation indentation
              :child-indentation nil)
             containers))
          (forward-line 1)))
      (setq containers (nreverse containers))
      (cl-loop
       for tail on containers
       for container = (car tail)
       for next = (cadr tail)
       do
       (setf
        (plist-get container :end)
        (if next
            (plist-get next :line-start)
          (cdr region))))
      containers)))

(defun douban--markdown-container-child-indentation (container)
  "返回 Markdown 类型 CONTAINER 的直接子字段缩进。"
  (if-let* ((cached
            (plist-get container :child-indentation)))
      cached
    (let ((parent
           (plist-get container :indentation))
          indentation)
      (save-excursion
        (goto-char (plist-get container :body-start))
        (while (< (point) (plist-get container :end))
          (unless
              (looking-at "[ \t]*\\(?:#\\|\r?$\\)")
            (let ((current (current-indentation)))
              (when
                  (and
                   (> current parent)
                   (or
                    (null indentation)
                    (< current indentation)))
                (setq indentation current))))
          (forward-line 1)))
      (let ((result (or indentation (+ parent 2))))
        (plist-put container :child-indentation result)
        result))))

(defun douban--markdown-metadata-source-index (containers)
  "一次扫描 CONTAINERS，返回 Markdown metadata 源稿快照。"
  (let ((index (douban--make-metadata-source-index)))
    (dolist (container containers)
      (let ((kind (plist-get container :kind))
            (indentation
             (douban--markdown-container-child-indentation
              container)))
        (douban--metadata-source-index-add-kind index kind)
        (save-excursion
          (goto-char (plist-get container :body-start))
          (while (< (point) (plist-get container :end))
            (when
                (and
                 (= (current-indentation) indentation)
                 (looking-at
                  "^[ \t]+\\([[:alnum:]-]+\\)[ \t]*:"))
              (let* ((source-field
                      (intern
                       (concat
                        ":"
                        (match-string-no-properties 1))))
                     (field
                      (douban--metadata-internal-field
                       kind source-field)))
                (when field
                  (let* ((start (match-end 0))
                         (value
                          (douban--yaml-simple-scalar
                           start (line-end-position))))
                    (douban--metadata-source-index-add-field
                     index kind field
                     (and value t) value)))))
            (forward-line 1)))))
    index))

(defun douban--markdown-container-at-line (containers line-start)
  "返回 CONTAINERS 中包含 LINE-START 的唯一容器。"
  (when (= (length containers) 1)
    (let ((container (car containers)))
      (and
       (>= line-start
           (plist-get container :body-start))
       (< line-start (plist-get container :end))
       container))))

(defun douban--markdown-scalar-context
    (replace-start line-end origin)
  "返回 Markdown 简单标量在 ORIGIN 处的补全与替换边界。"
  (let* ((scalar-start
          (progn
            (goto-char replace-start)
            (skip-chars-forward " \t" line-end)
            (point)))
         (scalar-end
          (progn
            (goto-char line-end)
            (skip-chars-backward " \t" scalar-start)
            (point)))
         (delimiter
          (and
           (< scalar-start scalar-end)
           (char-after scalar-start)))
         (quoted-p
          (and
           (memq delimiter '(?\' ?\"))
           (> scalar-end (1+ scalar-start))
           (eq (char-before scalar-end) delimiter)))
         (completion-start
          (if quoted-p (1+ scalar-start) scalar-start))
         (completion-end
          (if quoted-p (1- scalar-end) scalar-end)))
    (when
        (and
         (<= completion-start origin)
         (<= origin completion-end))
      (list
       :completion-start completion-start
       :completion-end completion-end
       :replace-start replace-start
       :replace-end line-end))))

(defun douban--markdown-platform-item-parent-line
    (container line-start item-indentation)
  "返回 CONTAINER 中 LINE-START 平台列表项的父字段行首。
ITEM-INDENTATION 是当前 `- VALUE' 行的缩进。只有 `platforms' 的直接
block-sequence 项才会返回非 nil。"
  (let ((child-indentation
         (douban--markdown-container-child-indentation
          container))
        (body-start (plist-get container :body-start))
        parent
        scanning)
    (when (> item-indentation child-indentation)
      (setq scanning t)
      (save-excursion
        (goto-char line-start)
        (while
            (and
             scanning
             (> (line-beginning-position) body-start))
          (forward-line -1)
          (cond
           ((looking-at "[ \t]*\\(?:#\\|\r?$\\)"))
           ((and
             (= (current-indentation) item-indentation)
             (looking-at
              "^[ \t]+-\\(?:[ \t]+\\|[ \t]*\r?$\\)")))
           ((and
             (= (current-indentation) child-indentation)
             (looking-at
              (concat
               "^[ \t]+platforms[ \t]*:[ \t]*"
               "\\(?:#.*\\)?\r?$")))
            (setq
             parent (line-beginning-position)
             scanning nil))
           (t (setq scanning nil))))))
    parent))

(defun douban--markdown-platform-item-context
    (container origin line-start line-end)
  "返回 CONTAINER 中光标所在平台 block-sequence 项的补全上下文。
ORIGIN 是原光标位置，LINE-START 与 LINE-END 是当前行边界。"
  (save-excursion
    (goto-char line-start)
    (when
        (and
         (eq (plist-get container :kind) 'review)
         (looking-at
          "^[ \t]+-\\(?:[ \t]+\\|[ \t]*\r?$\\)"))
      (let* ((item-indentation (current-indentation))
             (replace-start (match-end 0))
             (parent-line
              (douban--markdown-platform-item-parent-line
               container line-start item-indentation)))
        (when parent-line
          (when-let*
              ((bounds
                (douban--markdown-scalar-context
                 replace-start line-end origin)))
            (append
             (list
               :format 'markdown
               :slot 'value
               :kind 'review
               :container container
               :field :platforms
               :source-field :platforms)
             bounds
             (list
               :sequence-item-p t
               :platform-parent-line parent-line
               :item-indentation item-indentation))))))))

(defun douban--markdown-metadata-context ()
  "返回光标所在 Markdown metadata 字段名或值槽的上下文。"
  (when
      (and
       buffer-file-name
       (eq (douban--file-format buffer-file-name) 'markdown))
    (let ((origin (point))
          (line-start (line-beginning-position))
          (line-end (line-end-position)))
      (save-excursion
        (goto-char line-start)
        (when-let* ((region
                    (douban--markdown-douban-buffer-region)))
          (let* ((root-indentation
                  (douban--markdown-direct-metadata-indentation
                   region))
                 (containers
                  (douban--markdown-kind-containers
                   region root-indentation))
                 (source-index
                  (douban--markdown-metadata-source-index
                   containers))
                 (container
                  (douban--markdown-container-at-line
                   containers line-start))
                 (child-indentation
                  (and
                   container
                   (douban--markdown-container-child-indentation
                    container)))
                 (platform-item-context
                  (and
                   container
                   (when-let*
                       ((info
                         (douban--markdown-platform-item-context
                          container origin line-start line-end)))
                     (plist-put
                      info :source-index source-index))))
                 (slot
                  (cond
                   ((and
                     root-indentation
                     (= (current-indentation)
                        root-indentation))
                    'kind)
                   ((and
                     container
                     (= (current-indentation)
                        child-indentation))
                    'field))))
            (or
             platform-item-context
             (when
                 (and
                  slot
                  (looking-at
                   "^[ \t]+\\([[:alnum:]-]*\\)\\([ \t]*\\)\\(:\\)?"))
               (let* ((name-start (match-beginning 1))
                     (name-end (match-end 1))
                     (name
                      (buffer-substring-no-properties
                       name-start name-end))
                     (kind
                      (and
                       container
                       (plist-get container :kind)))
                     (source-field
                      (and
                       (eq slot 'field)
                       (not (string-empty-p name))
                       (intern (concat ":" name))))
                     (field
                      (and
                       source-field
                       (douban--metadata-internal-field
                        kind source-field)))
                     (current-kind
                      (and
                       (eq slot 'kind)
                       (member
                        name
                        (mapcar
                         #'symbol-name
                         douban--metadata-source-kinds))
                       (intern name)))
                     (colon-p (match-beginning 3))
                     (replace-start
                      (and colon-p (match-end 3))))
                (cond
                 ((and
                   (<= name-start origin)
                   (<= origin name-end))
                  (list
                   :format 'markdown
                   :slot slot
                   :kind kind
                   :current-kind current-kind
                   :container container
                   :source-index source-index
                   :field field
                   :source-field source-field
                   :completion-start name-start
                   :completion-end name-end
                   :colon-p (and colon-p t)))
                 ((and
                   (eq slot 'field)
                   colon-p
                   field
                   (<= replace-start origin))
                  (when-let*
                      ((bounds
                        (douban--markdown-scalar-context
                         replace-start line-end origin)))
                    (append
                     (list
                       :format 'markdown
                       :slot 'value
                       :kind kind
                       :container container
                       :source-index source-index
                       :field field
                       :source-field source-field)
                     bounds)))))))))))))

(defun douban--metadata-context ()
  "返回当前源稿光标所在豆瓣 metadata 的补全上下文。"
  (douban--markdown-metadata-context))

(defun douban--metadata-context-fields (info)
  "返回 INFO 所在类型容器已有的内部 metadata 字段。"
  (when-let* ((index (plist-get info :source-index))
             (kind (plist-get info :kind)))
    (douban--metadata-source-index-fields
     index kind)))

(defun douban--metadata-context-field-value (info field)
  "返回 INFO 类型容器中内部 metadata FIELD 的简单标量值。"
  (when-let* ((index (plist-get info :source-index))
             (kind (plist-get info :kind)))
    (douban--metadata-source-index-field-value
     index kind field)))

(defun douban--metadata-field-applicable-p (info field)
  "内部 FIELD 适用于 INFO 所在源稿时返回非 nil。"
  (when-let* ((descriptor
              (douban--metadata-field-descriptor
               (plist-get info :kind) field)))
    (let ((applicability
           (plist-get descriptor :applicability)))
      (or
       (null applicability)
       (equal
        (douban--metadata-context-field-value
         info (car applicability))
        (cdr applicability))))))

(defun douban--metadata-field-valid-in-context-p (info)
  "INFO 中的值字段属于当前明确类型时返回非 nil。"
  (let ((kind (plist-get info :kind))
        (field (plist-get info :field)))
    (and
     kind
     field
     (douban--metadata-source-field kind field)
     (douban--metadata-field-applicable-p
      info field))))

(defun douban--metadata-field-candidates (info)
  "返回 INFO 字段名槽应当提供的类型或内部字段。"
  (pcase (plist-get info :slot)
    ('kind
     (let ((current
            (plist-get info :current-kind))
           (present
            (when-let*
                ((index
                  (plist-get info :source-index)))
              (douban--metadata-source-index-kinds
               index))))
       (cl-remove-if
        (lambda (kind)
          (and
           (memq kind present)
           (not (eq kind current))))
        (copy-sequence
         douban--metadata-source-kinds))))
    ('field
     (let* ((kind (plist-get info :kind))
            (current (plist-get info :field))
            (present
             (douban--metadata-context-fields info))
            (fields
             (mapcar
              #'douban--metadata-descriptor-internal-field
              (douban--metadata-field-descriptors
               kind))))
       (cl-remove-if
        (lambda (field)
          (or
           (not
            (douban--metadata-field-applicable-p
             info field))
           (and
            (memq field present)
            (not (eq field current)))))
        fields)))))

(defun douban--metadata-field-source-name
    (object info)
  "返回类型或内部字段 OBJECT 在 INFO 中的补全字符串。"
  (let ((colon-p (plist-get info :colon-p))
        (kind (plist-get info :kind))
        name)
    (setq
     name
     (pcase (plist-get info :slot)
       ('kind (symbol-name object))
       ('field
        (let ((source-field
               (douban--metadata-source-field
                kind object)))
          (substring
           (symbol-name source-field) 1)))))
  (concat
   name
   (unless colon-p ":"))))

(defun douban--metadata-object-annotation (object info)
  "返回 INFO 中类型或字段 OBJECT 的补全旁注。"
  (when-let* ((description
              (pcase (plist-get info :slot)
                ('kind
                 (plist-get
                  (douban--kind-spec object)
                  :description))
                ('field
                 (plist-get
                  (douban--metadata-field-descriptor
                   (plist-get info :kind)
                   object)
                  :description)))))
    (concat "  " description)))

(defun douban--markdown-expand-empty-container
    (info status)
  "字段补全结束时把 INFO 的 Markdown 空 mapping 展开。
只有 STATUS 为 `finished' 且父容器仍写成 `{}` 时才删除 flow mapping。"
  (when
      (and
       (eq status 'finished)
       (eq (plist-get info :format) 'markdown)
       (eq (plist-get info :slot) 'field))
    (when-let* ((container
                (plist-get info :container)))
      (save-excursion
        (goto-char
         (plist-get container :line-start))
        (when
            (re-search-forward
             ":[ \t]*\\({}\\)[ \t]*\\(?:#.*\\)?$"
             (line-end-position) t)
          (delete-region
           (match-beginning 1)
           (match-end 1)))))))

(defun douban--metadata-field-capf (info)
  "根据字段名或类型容器上下文 INFO 返回 CAPF 数据。"
  (let* ((objects
          (douban--metadata-field-candidates info))
         (entries
          (mapcar
           (lambda (object)
             (cons
              (douban--metadata-field-source-name
               object info)
              object))
           objects)))
    (when entries
      (list
       (plist-get info :completion-start)
       (plist-get info :completion-end)
       (mapcar #'car entries)
       :exclusive t
       :annotation-function
       (lambda (candidate)
         (when-let* ((entry
                     (assoc-string
                      candidate entries)))
           (douban--metadata-object-annotation
            (cdr entry) info)))
       :exit-function
       (lambda (_candidate status)
         (douban--markdown-expand-empty-container
          info status))))))

(defun douban--metadata-value-annotation (field candidate)
  "返回 FIELD 值补全 CANDIDATE 的说明旁注。"
  (when-let* ((option
              (assoc-string
               candidate
               (douban--metadata-options-for-field field))))
    (when-let* ((annotation
                (plist-get (cdr option) :annotation)))
      (concat "  " annotation))))

(defun douban--metadata-completion-session-create
    (info value-function)
  "根据 INFO 创建 metadata 值补全会话。
VALUE-FUNCTION 把候选 label 和对象转换为规范值；字段 descriptor 的 codec
负责将该值写成当前源稿语法。"
  (douban--make-metadata-completion-session
   :source-buffer (current-buffer)
   :start-marker
   (copy-marker (plist-get info :replace-start))
   :end-marker
   (copy-marker (plist-get info :replace-end) t)
   :format (plist-get info :format)
   :sequence-item-p (plist-get info :sequence-item-p)
   :kind (plist-get info :kind)
   :field (plist-get info :field)
   :seen (make-hash-table :test 'equal)
   :value-function value-function))

(defun douban--metadata-completion-session-remember
    (session entries)
  "把 ENTRIES 累积到 SESSION 的候选表中，并原样返回 ENTRIES。"
  (dolist (entry entries)
    (let ((label
           (substring-no-properties (car entry))))
      (puthash
       label
       (cons label (cdr entry))
       (douban--metadata-completion-session-seen
        session))))
  entries)

(defun douban--metadata-completion-session-entry
    (session candidate)
  "返回 SESSION 中 CANDIDATE 对应的完整候选条目。"
  (gethash
   (substring-no-properties candidate)
   (douban--metadata-completion-session-seen session)))

(defun douban--metadata-completion-session-release (session)
  "释放 SESSION 持有的全部 source marker。"
  (dolist
      (marker
       (list
        (douban--metadata-completion-session-start-marker
         session)
        (douban--metadata-completion-session-end-marker
         session)))
    (set-marker marker nil)))

(defun douban--metadata-completion-source (session value)
  "按照 SESSION 字段的 codec 把规范 VALUE 格式化为源稿文本。"
  (let* ((kind
          (douban--metadata-completion-session-kind
           session))
         (field
          (douban--metadata-completion-session-field
           session))
         (descriptor
          (douban--metadata-field-descriptor kind field))
         (codec
          (and descriptor
               (plist-get descriptor :codec))))
    (unless descriptor
      (error
       "douban: %S metadata 不接受字段 %S"
       kind field))
    (unless
        (eq
         (douban--metadata-completion-session-format session)
         'markdown)
      (error
       "douban: 不支持的 metadata 源稿格式 %S"
       (douban--metadata-completion-session-format session)))
    (let ((scalar
           (pcase codec
             ((or 'id 'text 'list)
              (douban--yaml-string value))
             ((or 'rating 'boolean 'enum)
              (format "%s" value))
             (_
              (error
               "douban: metadata 字段 %S 没有可补全的 codec"
               field)))))
      (concat
       (unless
           (douban--metadata-completion-session-sequence-item-p
            session)
         " ")
       scalar))))

(defun douban--metadata-completion-finish
    (session candidate status)
  "在 STATUS 完成时把 SESSION 的显示 CANDIDATE 提交为规范源稿值。"
  (when (eq status 'finished)
    (unwind-protect
        (when-let*
            ((live-source
              (and
               (buffer-live-p
                (douban--metadata-completion-session-source-buffer
                 session))
               (douban--metadata-completion-session-source-buffer
                session)))
             (start-marker
              (douban--metadata-completion-session-start-marker
               session))
             (end-marker
              (douban--metadata-completion-session-end-marker
               session))
             (same-start-buffer
              (eq live-source
                  (marker-buffer start-marker)))
             (same-end-buffer
              (eq live-source
                  (marker-buffer end-marker)))
             (entry
              (douban--metadata-completion-session-entry
               session candidate))
             (start (marker-position start-marker))
             (end (marker-position end-marker))
             (ordered (<= start end))
             (value
              (funcall
               (douban--metadata-completion-session-value-function
                session)
               (car entry) (cdr entry)))
             (source
              (and
               value
               (douban--metadata-completion-source
                session value))))
          (with-current-buffer live-source
            (save-excursion
              (delete-region start end)
              (goto-char start)
              (insert source))))
      (douban--metadata-completion-session-release
       session))))

(defun douban--metadata-static-value-capf (info)
  "根据值槽上下文 INFO 返回静态枚举 CAPF 数据。"
  (let* ((kind (plist-get info :kind))
         (field (plist-get info :field))
         (descriptor
          (douban--metadata-field-descriptor
           kind field))
         (options (douban--metadata-options-for-field field)))
    (when
        (and
             (eq
              (douban--metadata-descriptor-completion
               descriptor)
              'static)
             options
             (douban--metadata-field-valid-in-context-p info))
      (let ((session
             (douban--metadata-completion-session-create
              info
              (lambda (label _option) label))))
        (douban--metadata-completion-session-remember
         session
         (mapcar
          (lambda (option)
            (cons (car option) option))
          options))
        (list
         (plist-get info :completion-start)
         (plist-get info :completion-end)
         (mapcar #'car options)
         :exclusive t
         ;; Static value sets are bounded and cheap.  Company-compatible
         ;; frontends such as Corfu interpret `t' as an explicit request to
         ;; offer them even when the value slot has an empty prefix.
         :company-prefix-length t
         :annotation-function
         (lambda (candidate)
           (douban--metadata-value-annotation
            field candidate))
         :exit-function
         (lambda (candidate status)
           (douban--metadata-completion-finish
            session candidate status)))))))

(defun douban--anthology-completion-entries (source-buffer)
  "在 SOURCE-BUFFER 中返回文集名称补全条目。"
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (douban--anthology-completion-candidates
       (douban--cached-anthologies)))))

(defun douban--anthology-completion-annotation
    (session candidate)
  "返回 SESSION 中 CANDIDATE 文集的篇数旁注。"
  (when-let*
      ((entry
        (douban--metadata-completion-session-entry
         session candidate))
       (anthology (cdr entry)))
    (format
     "  （%d 篇）"
     (plist-get anthology :items-count))))

(defun douban--anthology-value-capf (info)
  "根据 `anthology-id' 值槽 INFO 返回动态文集 CAPF 数据。"
  (when (douban--metadata-field-valid-in-context-p info)
    (let* ((source-buffer (current-buffer))
           (start
            (plist-get info :completion-start))
           (end
            (plist-get info :completion-end))
           (session
            (douban--metadata-completion-session-create
             info
             (lambda (_label anthology)
               (plist-get anthology :id))))
           (table
            (completion-table-dynamic
             (lambda (_prefix)
               (mapcar
                #'car
                (douban--metadata-completion-session-remember
                 session
                 (douban--anthology-completion-entries
                  source-buffer)))))))
      (list
       start end table
       :exclusive t
       :annotation-function
       (lambda (candidate)
         (douban--anthology-completion-annotation
          session candidate))
       :exit-function
       (lambda (candidate status)
         (douban--metadata-completion-finish
          session candidate status))))))

(defun douban--remote-metadata-completion-table
    (entries-function category)
  "返回远端补全表。
ENTRIES-FUNCTION 接收前端传入的查询字符串并返回 `(LABEL . OBJECT)' 条目。
CATEGORY 是 completion metadata 类别。远端搜索已经完成相关性筛选，因此
本表不会再要求 LABEL 以原查询开头。"
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata
          (category . ,category)
          (display-sort-function . identity)
          (cycle-sort-function . identity))
      (let ((labels
             (mapcar
              #'car
              (funcall entries-function string))))
        (when predicate
          (setq
           labels
           (cl-remove-if-not
            (lambda (candidate)
              (funcall predicate candidate))
            labels)))
        (cond
         ((eq action t) labels)
         ((eq action 'lambda)
          (and
           (member
            (substring-no-properties string)
            labels)
           t))
         ((and
           (consp action)
           (eq (car action) 'boundaries))
          '(boundaries 0 . 0))
         ((null labels) nil)
         ((member
           (substring-no-properties string)
           labels)
          t)
         ((null (cdr labels)) (car labels))
         ;; 多个远端相关性结果通常没有共同的本地前缀。保持用户的搜索词，
         ;; 让标准前端随后展示 `all-completions'，不能把值槽清空。
         (t string))))))

(defun douban--subject-completion-entries-for-query
    (source-buffer subject-type query)
  "在 SOURCE-BUFFER 中返回 SUBJECT-TYPE 和 QUERY 的条目补全项。"
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (let* ((query
              (string-trim
               (substring-no-properties query)))
             (loaded
              (and
               douban--subject-completion-cache
               (equal
                (plist-get
                 douban--subject-completion-cache
                 :subject-type)
                subject-type)
               (douban--subject-completion-candidates
                (plist-get
                 douban--subject-completion-cache
                 :subjects)))))
        (cond
         ((assoc-string query loaded) loaded)
         ((or
           (string-empty-p query)
           (string-match-p "\\`[1-9][0-9]*\\'" query))
          nil)
         (t
          (douban--subject-completion-candidates
           (douban--cached-subjects
            subject-type query))))))))

(defun douban--subject-id-value-capf (info)
  "根据 `subject-id' 值槽 INFO 返回条目搜索 CAPF。"
  (let* ((kind (plist-get info :kind))
         (subject-type
          (if (eq kind 'annotation)
              "book"
            (douban--metadata-context-field-value
             info :subject-type)))
         (content-id
          (pcase kind
            ('review
             (douban--metadata-context-field-value
              info :review-id))
            ('annotation
             (douban--metadata-context-field-value
              info :annotation-id))))
         (current
          (string-trim
           (buffer-substring-no-properties
            (plist-get info :completion-start)
            (plist-get info :completion-end)))))
    (when
        (and
         (douban--metadata-field-valid-in-context-p info)
         (memq kind '(review annotation))
         (assoc-string
          subject-type
          (douban--metadata-options-for-field
           :subject-type))
         (null content-id)
         (not
          (string-match-p
           "\\`[1-9][0-9]*\\'" current)))
      (let* ((source-buffer (current-buffer))
             (session
             (douban--metadata-completion-session-create
               info
               (lambda (_label subject)
                 (plist-get subject :subject-id))))
             (table
              (douban--remote-metadata-completion-table
               (lambda (query)
                 (douban--metadata-completion-session-remember
                  session
                  (douban--subject-completion-entries-for-query
                   source-buffer subject-type query)))
               'douban-subject)))
        (list
         (plist-get info :completion-start)
         (plist-get info :completion-end)
         table
         :exclusive t
         :exit-function
         (lambda (candidate status)
           (douban--metadata-completion-finish
            session candidate status)))))))

(defun douban--markdown-platform-other-values (info)
  "返回 Markdown 平台列表 INFO 中当前项以外的值。"
  (when
      (plist-get info :sequence-item-p)
    (let ((parent-line
           (plist-get info :platform-parent-line))
          (item-indentation
           (plist-get info :item-indentation))
          (current-line (line-beginning-position))
          (container-end
           (plist-get
            (plist-get info :container)
            :end))
          values
          scanning)
      (save-excursion
        (goto-char parent-line)
        (forward-line 1)
        (setq scanning t)
        (while
            (and
             scanning
             (< (point) container-end))
          (cond
           ((looking-at "[ \t]*\\(?:#\\|\r?$\\)"))
           ((<=
             (current-indentation)
             (douban--markdown-container-child-indentation
              (plist-get info :container)))
            (setq scanning nil))
           ((and
             (= (current-indentation) item-indentation)
             (looking-at
              "^[ \t]+-\\(?:[ \t]+\\|[ \t]*\r?$\\)"))
            (unless (= (line-beginning-position) current-line)
              (let* ((start (match-end 0))
                     (value
                      (douban--yaml-simple-scalar
                       start (line-end-position))))
                (when value
                  (push value values))))))
          (forward-line 1)))
      (nreverse values))))

(defun douban--platform-other-values (info)
  "返回 INFO 当前编辑项以外已经选择的平台协议值。"
  (douban--markdown-platform-other-values info))

(defun douban--platform-query-matches-p
    (query label platform)
  "QUERY 是否与 LABEL 或 PLATFORM 的可读属性匹配。"
  (let ((query (downcase query)))
    (or
     (string-empty-p query)
     (string-match-p "\\`[1-9][0-9]*\\'" query)
     (string-equal query (downcase label))
     (cl-some
      (lambda (value)
        (and
         (stringp value)
         (string-search
          query (downcase value))))
      (list
       label
       (plist-get platform :name)
       (plist-get platform :cn-name)
       (plist-get platform :abbreviation))))))

(defun douban--platform-completion-entries-for-query
    (source-buffer subject-id excluded query)
  "返回游戏 SUBJECT-ID 的平台补全项。
EXCLUDED 是当前项以外已经选择的平台 ID，QUERY 是前端输入。"
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (let* ((query
              (string-trim
               (substring-no-properties query)))
             (entries
              (douban--platform-completion-candidates
               (douban--cached-game-platforms
                subject-id)))
             (loaded-entry
              (assoc-string query entries)))
        (cl-remove-if-not
         (lambda (entry)
           (let ((platform (cdr entry)))
             (and
              (not
               (member
                (plist-get platform :id)
                excluded))
              (or
               loaded-entry
               (douban--platform-query-matches-p
                query (car entry) platform)))))
         entries)))))

(defun douban--platform-completion-annotation
    (session candidate)
  "返回平台 CANDIDATE 在 SESSION 中的缩写和协议 ID 旁注。"
  (when-let* ((entry
               (douban--metadata-completion-session-entry
                session candidate))
              (platform (cdr entry))
              (id (plist-get platform :id)))
    (let ((abbreviation
           (plist-get platform :abbreviation)))
      (format
       "  %sID %s"
       (if
           (and
            abbreviation
            (not
             (string-equal
              abbreviation
              (car entry))))
           (concat abbreviation " · ")
         "")
       id))))

(defun douban--platforms-value-capf (info)
  "根据游戏平台值槽 INFO 返回名称到协议 ID 的动态 CAPF。"
  (let* ((subject-type
          (douban--metadata-context-field-value
           info :subject-type))
         (subject-id
          (ignore-errors
            (douban--metadata-id
             "subject-id"
             (douban--metadata-context-field-value
              info :subject-id))))
         (current
          (string-trim
           (buffer-substring-no-properties
            (plist-get info :completion-start)
            (plist-get info :completion-end))))
         (unsupported-markdown-scalar-p
          (and
           (eq (plist-get info :format) 'markdown)
           (not (plist-get info :sequence-item-p))
           (or
            (string-prefix-p "[" current)
            (string-prefix-p "{" current)
            (string-search "," current)))))
    (when
        (and
         (douban--metadata-field-valid-in-context-p info)
         (equal subject-type "game")
         subject-id
         (not unsupported-markdown-scalar-p))
      (let* ((source-buffer (current-buffer))
             (excluded
              (douban--platform-other-values info))
             (session
             (douban--metadata-completion-session-create
               info
               (lambda (_label platform)
                 (plist-get platform :id))))
             (table
              (douban--remote-metadata-completion-table
               (lambda (query)
                 (douban--metadata-completion-session-remember
                  session
                  (douban--platform-completion-entries-for-query
                   source-buffer subject-id excluded query)))
               'douban-platform)))
        (list
         (plist-get info :completion-start)
         (plist-get info :completion-end)
         table
         :exclusive t
         :annotation-function
         (lambda (candidate)
           (douban--platform-completion-annotation
            session candidate))
         :exit-function
         (lambda (candidate status)
           (douban--metadata-completion-finish
            session candidate status)))))))

(defun douban--metadata-value-capf (info)
  "按照 INFO 字段 descriptor 的 provider 返回值补全数据。"
  (let* ((descriptor
          (douban--metadata-field-descriptor
           (plist-get info :kind)
           (plist-get info :field)))
         (provider
          (and descriptor
               (douban--metadata-descriptor-completion
                descriptor))))
    (pcase provider
      ('static
       (douban--metadata-static-value-capf info))
      ('subject
       (douban--subject-id-value-capf info))
      ('platform
       (douban--platforms-value-capf info))
      ('anthology
       (douban--anthology-value-capf info)))))

;;;###autoload
(defun douban-metadata-completion-at-point ()
  "补全当前 Markdown 源稿中的豆瓣 metadata。
字段名候选会按当前稿件类型过滤；枚举值使用可读源稿值并显示中文旁注。
`subject-id'、`platforms' 和 `anthology-id' 的动态候选都以名称显示，最终
只把对应的协议 ID 写入源稿。"
  (save-restriction
    (widen)
    (when-let* ((info (douban--metadata-context)))
      (pcase (plist-get info :slot)
        ((or 'kind 'field)
         (douban--metadata-field-capf info))
        ('value
         (douban--metadata-value-capf info))))))

(defun douban--reset-completion-caches ()
  "清空当前 buffer 的所有豆瓣补全缓存。"
  (setq douban--user-mention-completion-cache nil)
  (douban--reset-anthology-completion-cache)
  (douban--reset-remote-metadata-completion-caches))

(defun douban--source-editing-format ()
  "返回当前 buffer 可由 `douban-mode' 编辑的源稿格式。
文件扩展名必须受支持，且当前 major mode 必须与扩展名对应。"
  (when buffer-file-name
    (and
     (eq (douban--file-format buffer-file-name) 'markdown)
     (derived-mode-p 'markdown-mode)
     'markdown)))

(defvar douban-mode-map (make-sparse-keymap)
  "`douban-mode' 的按键映射。
本包不预设按键，以免覆盖 Markdown 的上下文命令。")

(defun douban--disable-editing-support ()
  "从当前 buffer 移除豆瓣编辑辅助并清空缓存。"
  (remove-hook
   'completion-at-point-functions
   #'douban-metadata-completion-at-point
   t)
  (remove-hook
   'completion-at-point-functions
   #'douban-user-mention-completion-at-point
   t)
  (remove-hook
   'after-revert-hook
   #'douban--reset-completion-caches
   t)
  (dolist
      (variable
       '(douban--user-mention-completion-cache
         douban--anthology-completion-cache
         douban--subject-completion-cache
         douban--platform-completion-cache))
    (kill-local-variable variable)))

;;;###autoload
(define-minor-mode douban-mode
  "在当前 Markdown 豆瓣源稿中启用编辑辅助。
本 mode 管理 metadata 补全、正文 `@' 用户补全、补全缓存和
revert 后的缓存清理。启用时不要求 metadata 已经完整。

发布仍由 `douban-publish' 显式执行，并不依赖本 mode。"
  :init-value nil
  :lighter " 豆"
  :keymap douban-mode-map
  :group 'douban
  (if douban-mode
      (progn
        (unless (douban--source-editing-format)
          (setq douban-mode nil)
          (douban--disable-editing-support)
          (user-error
           "douban: 当前 buffer 不是 Markdown 源稿文件"))
        (douban--reset-completion-caches)
        (add-hook
         'completion-at-point-functions
         #'douban-metadata-completion-at-point
         nil t)
        (add-hook
         'completion-at-point-functions
         #'douban-user-mention-completion-at-point
         nil t)
        (add-hook
         'after-revert-hook
         #'douban--reset-completion-caches
         nil t))
    (douban--disable-editing-support)))

(defun douban--parse-subject (input)
  "把规范条目 URL INPUT 解析为 ID 和类型 metadata。"
  (let ((input (string-trim input)))
    (cond
     ((string-match
       (concat
        "\\`https://www\\.douban\\.com/game/"
        "\\([1-9][0-9]*\\)/?\\'")
       input)
      (list
       :subject-id (match-string 1 input)
       :subject-type "game"))
     ((string-match
       (concat
        "\\`https://\\([^/]+\\)/subject/"
        "\\([1-9][0-9]*\\)/?\\'")
       input)
      (let* ((host (downcase (match-string 1 input)))
             (id (match-string 2 input))
             (type
              (cond
               ((string-equal host "book.douban.com") "book")
               ((string-equal host "movie.douban.com") "movie")
               ((string-equal host "music.douban.com") "music")
               (t
                (user-error
                 "douban: 不是支持的豆瓣条目 URL：%s" input)))))
        (list :subject-id id :subject-type type)))
     (t
     (user-error "douban: 不认识的条目 URL：%s" input)))))

(defconst douban--review-subject-type-choices
  '(("图书" . "book")
    ("电影" . "movie")
    ("剧集" . "tv")
    ("音乐" . "music")
    ("游戏" . "game"))
  "新评论条目品类的补全选项。")

(defun douban--read-review-subject-type ()
  "读取新评论的条目品类。"
  (cdr
   (assoc
    (completing-read
     "长评品类: "
     douban--review-subject-type-choices nil t)
    douban--review-subject-type-choices)))

(defun douban--review-subject-from-url (subject-type input)
  "返回 SUBJECT-TYPE 和规范 URL INPUT 对应的评论 metadata。"
  (unless
      (member subject-type
              '("book" "movie" "tv" "music" "game"))
    (user-error
     "douban: 品类必须是 book、movie、tv、music 或 game"))
  (unless (stringp input)
    (user-error "douban: 条目必须是规范 URL"))
  (let* ((parsed (douban--parse-subject input))
         (url-type (plist-get parsed :subject-type)))
    (unless
        (or
         (string-equal subject-type url-type)
         (and
          (member subject-type '("movie" "tv"))
          (string-equal url-type "movie")))
      (user-error
       "douban: 所选品类 %s 与条目 URL 不一致"
       subject-type))
    (list
     :subject-id (plist-get parsed :subject-id)
     :subject-type subject-type)))

(defun douban--search-subjects (query subject-type)
  "在评论品类 SUBJECT-TYPE 中搜索符合 QUERY 的豆瓣条目。"
  (setq query (string-trim query))
  (when (string-empty-p query)
    (user-error "douban: 条目搜索词不能为空"))
  (let* ((search-type
          (pcase subject-type
            ((or "book" "music") subject-type)
            ((or "movie" "tv") "movie")
            ("game" "ilmen")))
         (response
          (douban--read-json-endpoint
           (format
            (concat
             "https://m.douban.com/rexxar/api/v2/search/subjects"
             "?q=%s&type=%s&start=0&count=20&sort=relevance")
            (url-hexify-string query)
            search-type)
           "https://www.douban.com/search"))
         (status (plist-get response :status))
         (json (plist-get response :json))
         (seen (make-hash-table :test 'equal)))
    (unless (<= 200 status 299)
      (error "douban: 条目搜索失败（HTTP %s）" status))
    (unless json
      (error "douban: 条目搜索返回了无效 JSON"))
    (cl-labels
        ((text
          (value)
          (when (stringp value)
            (let ((value
                   (string-trim
                    (replace-regexp-in-string
                     "[[:space:]]+" " " value))))
              (unless (string-empty-p value) value)))))
      (cl-loop
       for item in
       (plist-get (plist-get json :subjects) :items)
       for target = (plist-get item :target)
       for id = (douban--value-string
                 (plist-get item :target_id))
       for type = (douban--value-string
                   (plist-get item :target_type))
       for title = (text (plist-get target :title))
       when (and
             (equal (plist-get item :layout) "subject")
             id
             (string-match-p "\\`[1-9][0-9]*\\'" id)
             (string-equal type subject-type)
             title
             (not (gethash id seen)))
       do (puthash id t seen)
       and collect
       (list
        :subject-id id
        :subject-type type
        :title title
        :summary
        (text (plist-get target :card_subtitle)))))))

(defun douban--game-platforms (subject-id)
  "匿名读取游戏 SUBJECT-ID 实际支持的平台。
返回保持服务端顺序的 plist 列表；其中平台 ID 统一为正十进制字符串，重复
ID 只保留第一次出现的条目。"
  (setq subject-id
        (or
         (douban--metadata-id "subject-id" subject-id)
         (error "douban: 游戏平台补全缺少 subject-id")))
  (let* ((url
          (format
           "https://m.douban.com/rexxar/api/v2/game/%s"
           subject-id))
         (response
          (douban--read-json-endpoint
           url
           (format
            "https://www.douban.com/game/%s/"
            subject-id)))
         (status (plist-get response :status))
         (json (plist-get response :json)))
    (unless (<= 200 status 299)
      (error "douban: 读取游戏平台失败（HTTP %s）" status))
    (unless
        (and
         (listp json)
         (equal
          (douban--value-string (plist-get json :id))
          subject-id)
         (equal (plist-get json :type) "game")
         (plist-member json :platforms)
         (listp (plist-get json :platforms)))
      (error "douban: 游戏平台接口返回了无效 JSON"))
    (let ((seen (make-hash-table :test 'equal))
          platforms)
      (dolist (item (plist-get json :platforms))
        (let* ((id
                (and
                 (listp item)
                 (douban--value-string
                  (plist-get item :id))))
               (name
                (and
                 (stringp (plist-get item :name))
                 (douban--metadata-text
                  "platform.name"
                  (plist-get item :name))))
               (cn-name
                (and
                 (stringp (plist-get item :cn_name))
                 (douban--metadata-text
                  "platform.cn_name"
                  (plist-get item :cn_name))))
               (abbreviation
                (and
                 (stringp (plist-get item :abbreviation))
                 (douban--metadata-text
                  "platform.abbreviation"
                  (plist-get item :abbreviation)))))
          (unless
              (and
               id
               (string-match-p "\\`[1-9][0-9]*\\'" id)
               (or name cn-name abbreviation))
            (error "douban: 游戏平台接口包含无效平台条目"))
          (unless (gethash id seen)
            (puthash id t seen)
            (push
             (list
              :id id
              :name name
              :cn-name cn-name
              :abbreviation abbreviation)
             platforms))))
      (nreverse platforms))))

(defun douban--subject-candidate-label (subject)
  "把 SUBJECT 格式化为补全标签。"
  (let* ((type (plist-get subject :subject-type))
         (type-label
         (pcase type
            ("book" "图书")
            ("movie" "电影")
            ("tv" "剧集")
            ("music" "音乐")
            ("game" "游戏")
            (_ type)))
         (summary (plist-get subject :summary)))
    (concat
     (plist-get subject :title)
     (when summary
       (concat
        " — "
        (truncate-string-to-width summary 60 nil nil "…")))
     (format
      " [%s · %s]"
      type-label (plist-get subject :subject-id)))))

;;;###autoload
(defun douban-search-subject (subject-type &optional input)
  "在 SUBJECT-TYPE 中按规范 URL 或名称查找条目并返回规范 URL。
SUBJECT-TYPE 是 `book'、`movie'、`tv'、`music' 或 `game'。INPUT 为 nil
时读取 URL 或名称。交互调用时先读取品类，并把选定 URL 插入光标处。"
  (interactive (list (douban--read-review-subject-type)))
  (let* ((input
          (string-trim
           (or input (read-string "豆瓣条目 URL 或名称: "))))
         (url
          (if (string-match-p "\\`https?://" input)
              (let ((subject
                     (douban--review-subject-from-url
                      subject-type input)))
                (douban--subject-url
                 subject-type (plist-get subject :subject-id)))
            (let* ((subjects
                    (douban--search-subjects input subject-type))
                   (candidates
                    (mapcar
                     (lambda (subject)
                       (cons
                        (douban--subject-candidate-label subject)
                        (douban--subject-url
                         subject-type
                         (plist-get subject :subject-id))))
                     subjects)))
              (unless candidates
                (user-error "douban: 没有找到条目：%s" input))
              (cdr
               (assoc
                (completing-read "豆瓣条目: " candidates nil t)
                candidates))))))
    (when (called-interactively-p 'interactive)
      (insert url)
      (message "douban: 已插入条目 URL：%s" url))
    url))

(defun douban--new-source-content (meta)
  "返回 META 对应的初始 Markdown 源稿文本。"
  (let* ((kind (plist-get meta :kind))
         (title-p
          (plist-get (douban--kind-spec kind) :title-p))
         (title (or (plist-get meta :title) "")))
    (concat
     "---\n"
     (when title-p
       (format "title: %s\n" (douban--yaml-string title)))
     (douban--format-yaml-meta meta)
     "---\n\n")))

(defun douban--create-source-file (file meta)
  "为 META 创建源稿 FILE，并在新 tab 中启用 `douban-mode'。
缺少的父目录会一并创建。"
  (let* ((file (expand-file-name file))
         (content (douban--new-source-content meta)))
    (douban--require-source-format file)
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (insert content)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region
         (point-min) (point-max) file
         nil 'silent nil 'excl)))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (unless (douban--source-editing-format)
          (require 'markdown-mode)
          (markdown-mode))
        (douban-mode 1))
      (tab-new)
      (switch-to-buffer buffer))
    (message "douban: 已创建 %s" file)
    file))

(defun douban--ensure-review-directory ()
  "返回规范化的 `douban-review-directory'，并确保它存在。"
  (let ((directory
         (file-name-as-directory
          (expand-file-name douban-review-directory))))
    (make-directory directory t)
    directory))

;;;###autoload
(defun douban-new-review (subject-type subject file)
  "创建豆瓣长评源稿 FILE。
SUBJECT-TYPE 是 `book'、`movie'、`tv'、`music' 或 `game'，SUBJECT 是
对应的规范豆瓣条目或游戏 URL。交互调用时先选择 SUBJECT-TYPE，再输入
规范 URL 或豆瓣条目名称，并从 `douban-review-directory' 读取 FILE。
FILE 必须使用 `.md' 或 `.markdown' 扩展名。创建成功后在新 tab 中打开源稿并
启用 `douban-mode'。本命令不打开网页编辑器，也不推断标题。"
  (interactive
   (let ((subject-type (douban--read-review-subject-type)))
     (list
      subject-type
      (douban-search-subject subject-type)
      (read-file-name
       "评论源稿（.md/.markdown）: "
       (douban--ensure-review-directory) nil nil))))
  (let* ((parsed
          (douban--review-subject-from-url
           subject-type subject))
         (meta
          (douban--meta-from-plist
           (list :review parsed)
           nil)))
    (douban--create-source-file file meta)))

(defun douban--append-cc-statement (html)
  "按 `douban-cc-statement' 在 HTML 末尾追加 CC 声明引用。"
  (let* ((spec
          (pcase douban-cc-statement
            ('nil nil)
            ('cc0 'cc0)
            ('by '("by" "署名"))
            ('by-sa '("by-sa" "署名—相同方式共享"))
            ('by-nd '("by-nd" "署名—禁止演绎"))
            ('by-nc '("by-nc" "署名—非商业性使用"))
            ('by-nc-sa
             '("by-nc-sa" "署名—非商业性使用—相同方式共享"))
            ('by-nc-nd
             '("by-nc-nd" "署名—非商业性使用—禁止演绎"))
            (_
             (error "douban: 无效的 douban-cc-statement：%S"
                    douban-cc-statement))))
         (statement
          (pcase spec
            ('nil nil)
            ('cc0
             (concat
              "<blockquote><p>除另有声明外，本文中的原创内容已通过 "
              "<a href=\"https://creativecommons.org/publicdomain/zero/"
              "1.0/deed.zh-hans\" rel=\"license\">CC0 1.0 通用</a>"
              "，在法律允许的范围内贡献至公共领域。</p></blockquote>"))
            (`(,slug ,name)
             (format
              (concat
               "<blockquote><p>除另有声明外，本文中的原创内容采用 "
               "<a href=\"https://creativecommons.org/licenses/%s/4.0/"
               "deed.zh-hans\" rel=\"license\">CC %s 4.0"
               "（%s 4.0 协议国际版）</a> 许可。</p></blockquote>")
              slug (upcase slug) name)))))
    (if statement
        (concat
         html
         (unless
             (or
              (string-empty-p html)
              (string-suffix-p "\n" html))
           "\n")
         statement)
      html)))

(defun douban--prepare-draft (file kind validator)
  "编译 FILE，并返回 `(RAW CHARACTER-COUNT BASE-DIRECTORY)'。
KIND 决定进度名称和是否追加 CC 声明。
VALIDATOR 只检查源稿正文对应的 Draft.js RAW，并返回正文字符数；生成的
CC 声明不参与非空或最低字数校验。"
  (let* ((label
          (pcase kind
            ('review nil)
            ('note "日记")
            ('annotation "读书笔记")
            ('status "普通广播")
            (_ (error "douban: 不支持的稿件类型：%S" kind))))
         (format (douban--file-format file))
         (base-directory
          (file-name-directory (expand-file-name file))))
    (message
     "douban: 编译 %s%s..."
     format
     (if label (concat " " label) ""))
    (let* ((source-html (douban--source-html file))
           (html
            (if (memq kind '(review note))
                (douban--append-cc-statement source-html)
              source-html))
           (source-raw (douban--html-to-draft source-html))
           (character-count (funcall validator source-raw))
           (raw
            (if (eq html source-html)
                source-raw
              (douban--html-to-draft html)))
           (raw
            (douban--rewrite-draft-cards raw)))
      (list raw character-count base-directory))))

(defun douban--publish-review-file (file meta)
  "按照 META 发布评论 FILE，并返回评论 ID。"
  (pcase-let*
      ((title
        (douban--require-title
         (plist-get meta :title) "评论" 200))
       (`(,raw ,character-count ,base-directory)
        (douban--prepare-draft
         file 'review #'douban--validate-draft))
       (session
        (if (not (douban--draft-has-image-p raw))
            (progn
              (message "douban: 从浏览器 Cookie 建立评论发布会话...")
              (douban--review-direct-session meta))
          (progn
            (message "douban: 读取评论发布上下文...")
            (douban--review-editor-session meta)))))
    (message "douban: 正文 %d 字，处理图片..." character-count)
    (douban--rewrite-draft-images raw session base-directory)
    (message
     "douban: %s评论..."
     (if (plist-get meta :review-id) "更新" "创建"))
    (let ((result
           (douban--submit-review meta raw session title)))
      (if (plist-get meta :review-id)
          (progn
            (message "douban: 已更新 %s" (plist-get result :url))
            (plist-get result :id))
        (let ((review-id
               (douban--checkpoint-published-content
                file meta result "评论")))
          (unless douban-review-send-broadcast
            (condition-case err
                (douban--remove-created-review-broadcast
                 session review-id)
              ((error quit)
               (signal
                'douban-review-broadcast-cleanup-failed
                (list
                 (format
                  (concat
                   "评论已经发布且 ID %s 已写回源稿，但对应广播未能确认删除。"
                   "不要重新发布评论；请到豆瓣主页检查并手动删除广播。"
                   "原错误：%s")
                  review-id (error-message-string err)))))))
          review-id)))))

(defun douban--checkpoint-published-content
    (file meta result label)
  "把 LABEL 所指发布 RESULT 检查点写入 FILE 的 META。"
  (let* ((kind (plist-get meta :kind))
         (id-field (douban--metadata-id-field kind))
         (content-id (plist-get result :id))
         (content-url (plist-get result :url)))
    (setq meta (plist-put meta id-field content-id))
    (condition-case err
        (douban--checkpoint-meta file meta)
      (error
       (signal
        'douban-published-but-not-checkpointed
        (list
         (format
          (concat
           "%s已经发布%s（ID %s），但源稿 metadata 写回失败。"
           "请立即手动记录该 ID，修复文件后再操作；"
           "不要重新创建。原错误：%s")
          label
          (if content-url (format "为 %s" content-url) "")
          content-id
          (error-message-string err))))))
    (if content-url
        (message "douban: 已发布 %s" content-url)
      (message "douban: 已发布%s（ID %s）" label content-id))
    content-id))

(defun douban--publish-note-file (file meta)
  "按照 META 发布日记 FILE，并返回日记 ID。"
  (pcase-let*
      ((title
        (douban--require-title
         (plist-get meta :title) "日记" 100))
       (`(,raw ,character-count ,base-directory)
        (douban--prepare-draft
         file 'note
         (lambda (raw)
           (douban--validate-content-draft raw "日记"))))
       (existing-id (plist-get meta :note-id)))
    (message "douban: 读取日记发布上下文...")
    (let* ((session (douban--note-session meta))
           (note-id
            (douban--session-state-get session :note-id))
           (update-p
            (not
             (string-equal
              (douban--session-state-get session :action)
              "new")))
           (privacy (douban--note-privacy-value meta session)))
      (setq meta (copy-sequence meta))
      (unless existing-id
        ;; The create page has already durably allocated a draft ID.
        ;; Save it before the first upload or mutation.
        (setq meta (plist-put meta :note-id note-id))
        (condition-case err
            (douban--checkpoint-meta file meta)
          (error
           (error
            (concat
             "douban: 日记编辑器已分配 note-id=%s，"
             "但写回源稿失败；尚未上传或发布。原错误：%s")
            note-id (error-message-string err)))))
      (message "douban: 日记正文 %d 字，处理图片..."
               character-count)
      (douban--rewrite-draft-images raw session base-directory)
      (message
       "douban: %s日记..."
       (if update-p "更新" "发布"))
      (let ((result
             (douban--submit-note
              meta raw session title privacy)))
        (if update-p
            (progn
              (message
               "douban: 已更新 %s"
               (douban--canonical-note-url note-id))
              (plist-get result :id))
          (message "douban: 已发布 %s" (plist-get result :url))
          (plist-get result :id))))))

(defun douban--publish-annotation-file (file meta)
  "按照 META 发布或更新新式读书笔记 FILE，并返回 topic ID。"
  (pcase-let*
      ((_title
        (douban--require-title
         (plist-get meta :title) "读书笔记" 70))
       (`(,raw ,_character-count ,base-directory)
        (douban--prepare-draft
         file 'annotation
         (lambda (raw)
           (douban--validate-content-draft
            raw "读书笔记"))))
       (update-p (plist-get meta :annotation-id))
       (images-p (douban--draft-has-image-p raw)))
    (pcase-let
        ((`(,api-session . ,upload-session)
          (douban--annotation-sessions meta images-p)))
      (when images-p
        (message "douban: 处理读书笔记图片...")
        (douban--rewrite-draft-images
         raw upload-session base-directory))
      (message
       "douban: %s读书笔记..."
       (if update-p "更新" "发布"))
      (let ((result
             (douban--submit-annotation
              meta api-session raw)))
        (if update-p
            (progn
              (message
               "douban: 已更新 %s"
               (douban--canonical-annotation-url
                (plist-get meta :annotation-id)))
              (plist-get result :id))
          (douban--checkpoint-published-content
           file meta result "读书笔记"))))))

(defun douban--publish-status-file (file meta)
  "按照 META 发布普通广播 FILE，并返回其 ID。"
  (pcase-let*
      ((`(,raw ,_character-count ,base-directory)
        (douban--prepare-draft
         file 'status
         (lambda (raw)
           (douban--validate-content-draft
            raw "普通广播"))))
       (update-p (plist-get meta :status-id))
       (images-p (douban--draft-has-image-p raw)))
    (pcase-let
        ((`(,api-session . ,upload-session)
          (douban--status-sessions meta images-p)))
      (when images-p
        (message "douban: 处理普通广播图片...")
        (douban--rewrite-draft-images
         raw upload-session base-directory))
      (message
       "douban: %s普通广播..."
       (if update-p "更新" "发布"))
      (let ((result
             (douban--submit-status meta api-session raw)))
        (if update-p
            (progn
              (message
               "douban: 已更新广播 ID %s"
               (plist-get meta :status-id))
              (plist-get result :id))
          (douban--checkpoint-published-content
           file meta result "普通广播"))))))

(defun douban--publish-file (file meta)
  "按照规范化 META 发布豆瓣内容 FILE。"
  (pcase (plist-get meta :kind)
    ('review (douban--publish-review-file file meta))
    ('note (douban--publish-note-file file meta))
    ('annotation (douban--publish-annotation-file file meta))
    ('status (douban--publish-status-file file meta))))

;;;###autoload
(defun douban-publish ()
  "保存当前豆瓣源稿，并按照唯一类型子映射发布或更新内容。"
  (interactive)
  (pcase-let ((`(,file ,meta) (douban--current-source)))
    (douban--publish-file file meta)))

(provide 'douban)
;;; douban.el ends here
