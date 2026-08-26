;;; douban-test.el --- Tests for douban.el  -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(setq load-prefer-newer t)
(require 'douban)

(defmacro douban-test--with-temp-file (suffix contents &rest body)
  "将 `file' 绑定到一个后缀为 SUFFIX、内容为 CONTENTS 的临时文件。"
  (declare (indent 2) (debug t))
  `(let ((file (make-temp-file "douban-test-" nil ,suffix ,contents)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p file)
         (delete-file file))
       (when-let* ((buffer (find-buffer-visiting file)))
         (kill-buffer buffer)))))

(defun douban-test--long-text ()
  "返回达到豆瓣长评长度要求的中文测试文本。"
  (apply #'concat (make-list 15 "这是一段用于测试豆瓣长评发布流程的正文。")))

(defun douban-test--review-meta (value title)
  "补齐必需字段，并规范化长评元数据 VALUE 和标题 TITLE。"
  (douban--meta-from-plist
   (list
    :review
    (if (plist-member value :subject-type)
        value
      (append value '(:subject-type "book"))))
   title))

(defun douban-test--review-editor-html
    (subject-id app-name &optional review-id rtype before-form)
  "返回 SUBJECT-ID 和 APP-NAME 对应的评论编辑器 HTML。
REVIEW-ID 非 nil 时模拟更新页；RTYPE 是游戏页选中的评论类型。
BEFORE-FORM 原样插入目标表单之前，可用于构造同名字段干扰项。"
  (concat
   "<html><body>"
   before-form
   "<form id=\"review-editor-form\">"
   "<input name=\"ck\" value=\"page-ck\">"
   (format
    "<input name=\"review[subject_id]\" value=\"%s\">"
    subject-id)
   (when (string-equal app-name "game")
     (format
      (concat
       "<input type=\"radio\" name=\"review[rtype]\" value=\"R\"%s>"
       "<input type=\"radio\" name=\"review[rtype]\" value=\"G\"%s>")
      (if (equal rtype "R") " checked" "")
      (if (equal rtype "G") " checked" "")))
   "</form><script>\n"
   (format "_APP_NAME = '%s';\n" app-name)
   (when review-id
     (format "_REVIEW_ID = %s;\n" review-id))
   (concat
    "_POST_PARAMS = {siteCookie: {name: 'upload_auth_token', "
    "value: 'dummy-token'}};\n"
    "</script></body></html>")))


(defun douban-test--status-raw (&rest lines)
  "返回以 LINES 为普通段落的广播 Draft.js raw 内容。"
  (list
   :blocks
   (vconcat
    (cl-loop
     for line in lines
     for index from 0
     collect
     (list
      :key (format "status-%d" index)
      :text line
      :type "unstyled"
      :depth 0
      :inlineStyleRanges []
      :entityRanges []
      :data nil)))
   :entityMap (make-hash-table :test 'equal)))

(defun douban-test--toc-marker (depth)
  "返回 DEPTH 对应的内部目录 HTML 标记。"
  (format
   "<div %s=\"%d\"></div>"
   douban--toc-marker-attribute depth))

(defun douban-test--topic-image-raw (&rest ids)
  "返回按 IDS 次序包含 topic 图片的 Draft.js raw 内容。"
  (let ((entities (make-hash-table :test 'equal))
        blocks)
    (cl-loop
     for id in ids
     for index from 0
     do
     (puthash
      (number-to-string index)
      (list
       :type "IMAGE"
       :mutability "IMMUTABLE"
       :data
       (list
        :id id
        :src
        (format
         "https://img1.doubanio.com/view/group_topic/l/public/p%s.webp"
         id)
        :raw_src
        (format
         "https://img1.doubanio.com/view/group_topic/l/public/p%s.webp"
         id)
        :caption ""))
      entities)
     (push
      (list
       :key (format "image-%d" index)
       :text " "
       :type "atomic"
       :depth 0
       :inlineStyleRanges []
       :entityRanges
       (vector
        (list :offset 0 :length 1 :key index))
       :data nil)
      blocks))
    (list :blocks (vconcat (nreverse blocks))
          :entityMap entities)))

(defun douban-test--entity-raw (entries &rest block-keys)
  "返回按 BLOCK-KEYS 引用 ENTRIES 的 Draft.js raw 内容。
ENTRIES 是 `(STRING-KEY . ENTITY)' 列表，并按列表顺序插入 entityMap；
BLOCK-KEYS 中每个列表表示一个 block 的 entity range 次序。"
  (let ((entities (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (puthash (car entry) (cdr entry) entities))
    (list
     :blocks
     (vconcat
      (cl-loop
       for keys in block-keys
       for block-index from 0
       collect
       (list
        :key (format "entity-block-%d" block-index)
        :text (make-string (length keys) ?\s)
        :type "unstyled"
        :depth 0
        :inlineStyleRanges []
        :entityRanges
        (vconcat
         (cl-loop
          for key in keys
          for offset from 0
          collect
          (list
           :offset offset
           :length 1
           :key
           (if (stringp key)
               (string-to-number key)
             key))))
        :data nil)))
     :entityMap entities)))

(defun douban-test--first-draft-entity (raw)
  "返回 Draft RAW 中编号最小的 entity。"
  (gethash "0" (plist-get raw :entityMap)))

(defun douban-test--block-first-entity (raw block)
  "返回 RAW 中 BLOCK 的第一个 entity；没有 range 时返回 nil。"
  (when-let* ((range
              (and
               (> (length (plist-get block :entityRanges)) 0)
               (aref (plist-get block :entityRanges) 0))))
    (gethash
     (number-to-string (plist-get range :key))
     (plist-get raw :entityMap))))

(ert-deftest douban-test-metadata-schema-is-self-consistent ()
  (let ((codecs
         '(id text rating boolean enum list))
        (providers
         '(nil static subject platform anthology)))
    (dolist (kind-spec douban--kind-specs)
      (let* ((kind (car kind-spec))
             (spec (cdr kind-spec))
             (descriptors
              (douban--metadata-field-descriptors kind))
             (sources
              (mapcar
               (lambda (descriptor)
                 (plist-get descriptor :source))
               descriptors))
             (internals
              (mapcar
               #'douban--metadata-descriptor-internal-field
               descriptors)))
        (should (stringp (plist-get spec :description)))
        (should descriptors)
        (should
         (= (length sources)
            (length
             (delete-dups
              (copy-sequence sources)))))
        (should
         (= (length internals)
            (length
             (delete-dups
              (copy-sequence internals)))))
        (should
         (eq
          (douban--metadata-id-field kind)
          (douban--metadata-internal-field kind :id)))
        (dolist (descriptor descriptors)
          (let ((source
                 (plist-get descriptor :source))
                (internal
                 (douban--metadata-descriptor-internal-field
                  descriptor))
                (codec
                 (plist-get descriptor :codec))
                (description
                 (plist-get descriptor :description))
                (provider
                 (douban--metadata-descriptor-completion
                  descriptor))
                (applicability
                 (plist-get descriptor :applicability)))
            (should (keywordp source))
            (should (keywordp internal))
            (should (memq codec codecs))
            (when (plist-member descriptor :internal)
              (should-not (eq source internal)))
            (when (plist-member descriptor :completion)
              (should
               (memq
                provider
                '(subject platform anthology))))
            (should
             (or
              (null description)
              (stringp description)))
            (should (memq provider providers))
            (should
             (eq
              descriptor
              (douban--metadata-field-descriptor
               kind internal)))
            (should
             (eq
              descriptor
              (douban--metadata-source-field-descriptor
               kind source)))
            (when (eq provider 'static)
              (should
               (douban--metadata-options-for-field
                internal)))
            (when (plist-get descriptor :allow-empty)
              (should (eq codec 'enum)))
            (when (plist-get descriptor :nonempty-if-present)
              (should (eq codec 'id)))
            (when applicability
              (should (keywordp (car applicability)))
              (should
               (douban--metadata-field-descriptor
                kind (car applicability)))
              (should (stringp (cdr applicability))))))))))

(ert-deftest douban-test-metadata-schema-codecs-roundtrip-all-fields ()
  (dolist
      (meta
       '((:kind review
          :subject-id "1"
          :review-id "11"
          :subject-type "game"
          :title "长评标题"
          :introduction "长评导语"
          :rating 5
          :spoiler t
          :donate t
          :explanation-types "ai-generated"
          :rtype "guide"
          :platforms ("100" "200"))
         (:kind note
          :note-id "22"
          :title "日记标题"
          :note-privacy "friends"
          :cannot-reply t
          :author-tags ("随笔" "生活"))
         (:kind annotation
          :annotation-id "23"
          :subject-id "2"
          :title "读书笔记标题"
          :annotation-privacy "private"
         :explanation-types "personal-opinion")
         (:kind status
          :status-id "303"
          :anthology-id "44"
          :explanation-types "repost")))
    (let* ((title (plist-get meta :title))
           (markdown
            (concat
             (when title
               (format
                "title: %s\n"
                (douban--yaml-string title)))
             (douban--format-yaml-meta meta)))
           (markdown-meta
            (douban--md-frontmatter-meta markdown)))
      (dolist
          (field
           (append
            '(:kind :title)
            (mapcar
             #'douban--metadata-descriptor-internal-field
             (douban--metadata-field-descriptors
              (plist-get meta :kind)))))
        (should
         (eq
          (and (plist-member markdown-meta field) t)
          (and (plist-member meta field) t)))
        (should
         (equal
          (plist-get markdown-meta field)
          (plist-get meta field)))))))

(ert-deftest douban-test-markdown-enums-serialize-as-plain-scalars ()
  (let ((yaml
         (douban--format-yaml-meta
          '(:kind review
            :review-id "17747705"
            :subject-id "37451892"
            :subject-type "music"
            :introduction "导语"))))
    (should (string-match-p "    id: '17747705'" yaml))
    (should (string-match-p "    subject-id: '37451892'" yaml))
    (should (string-match-p "    subject-type: music" yaml))
    (should-not (string-match-p "subject-type: 'music'" yaml))
    (should (string-match-p "    introduction: '导语'" yaml))))

(ert-deftest douban-test-meta-normalization ()
  (let ((meta
         (douban-test--review-meta
          (list
           :subject-id "123"
           :id "456"
           :subject-type "game"
           :rating "5"
           :spoiler "true"
           :platforms '("1" "2"))
          " 标题 ")))
    (should (equal (plist-get meta :subject-id) "123"))
    (should (equal (plist-get meta :review-id) "456"))
    (should (equal (plist-get meta :title) "标题"))
    (should (= (plist-get meta :rating) 5))
    (should (eq (plist-get meta :spoiler) t))
    (should (equal (plist-get meta :platforms) '("1" "2")))
    (should-not (plist-get meta :rtype))))

(ert-deftest douban-test-annotation-meta-normalization-and-isolation ()
  (let ((meta
         (douban--meta-from-plist
          '(:annotation
            (:subject-id "123"
             :id "456"
             :privacy "private"
             :explanation-types "none"))
          "摘录与感想")))
    (should (eq (plist-get meta :kind) 'annotation))
    (should (equal (plist-get meta :subject-id) "123"))
    (should (equal (plist-get meta :annotation-id) "456"))
    (should (equal (plist-get meta :title) "摘录与感想"))
    (should (equal (plist-get meta :annotation-privacy) "private"))
    (should (equal (plist-get meta :explanation-types) "none")))
  (should-error
   (douban--meta-from-plist
    '(:annotation (:subject-id "123" :subject-type "book"))
    "标题")
   :type 'error)
  (should-error
   (douban--meta-from-plist
    '(:annotation (:subject-id "123")
      :status (:id "456" :topic-id "789"))
    "标题")
   :type 'error))

(ert-deftest douban-test-meta-normalizes-game-rtype ()
  (dolist (rtype '("review" "guide"))
    (should
     (equal
      (douban--normalize-metadata-field
       'review :rtype rtype)
      rtype)))
  (dolist (empty '(nil "" " \t"))
    (should-not
     (douban--normalize-metadata-field
      'review :rtype empty)))
  (dolist (bad '("R" "G" "r" "g" "article" 1))
    (should-error
     (douban--normalize-metadata-field
      'review :rtype bad)
     :type 'error)))

(ert-deftest douban-test-source-formats-are-markdown-only ()
  (should (eq (douban--file-format "review.md") 'markdown))
  (should (eq (douban--file-format "review.markdown") 'markdown))
  (should-not (douban--file-format "review.org"))
  (should-not (douban--file-format "review.typ"))
  (should-error
   (douban--require-source-format
    (expand-file-name
     "review.typ" temporary-file-directory))
   :type 'user-error)
  (with-temp-buffer
    (setq buffer-file-name
          (expand-file-name "review.typ" temporary-file-directory))
    (should-not (douban-metadata-completion-at-point))
    (should-error (douban-publish) :type 'user-error)))

(ert-deftest douban-test-source-format-allows-tramp-paths ()
  (let ((remote "/ssh:example.invalid:/srv/notes/review.md"))
    (should
     (eq
      (douban--require-source-format remote)
      'markdown))))

(ert-deftest douban-test-markdown-null-spellings-are-text-where-allowed ()
  (let ((meta
         (douban--md-frontmatter-meta
          (concat
           "title: 评论\n"
           "douban:\n"
           "  review:\n"
           "    subject-id: '123'\n"
           "    subject-type: game\n"
           "    introduction: null\n"
           "    platforms:\n"
           "      - null\n"
           "      - '~'\n"))))
    (should (equal (plist-get meta :introduction) "null"))
    (should (equal (plist-get meta :platforms) '("null" "~"))))
  ;; 放宽 YAML 文本并不放宽 ID；裸 null 仍不是正整数。
  (should-error
   (douban--md-frontmatter-meta
    (concat
     "title: 评论\n"
     "douban:\n"
     "  review:\n"
     "    subject-id: '123'\n"
     "    subject-type: book\n"
     "    id: null\n"))
   :type 'error))

(ert-deftest douban-test-meta-id-no-longer-requires-url ()
  (let ((meta
         (douban-test--review-meta
          (list :subject-id "1" :id "2")
          "title")))
    (should (equal (plist-get meta :review-id) "2"))))

(ert-deftest douban-test-meta-rating-range ()
  (dolist (bad '(0 3 6 "0" "6" 2.5))
    (should-error (douban--metadata-rating bad)))
  (dolist (empty '(nil "" " \t"))
    (should-not (douban--metadata-rating empty)))
  (should (= (douban--metadata-rating "3") 3)))

(ert-deftest douban-test-metadata-ids-must-be-positive ()
  (dolist (empty '(nil "" " \t\n"))
    (should-not (douban--metadata-id "subject-id" empty)))
  (should (equal (douban--metadata-id "subject-id" " 42 ") "42"))
  (dolist
      (bad
       '(0 1 -1 1.0 t :json-null (1)
         "0" "01" "-1" "+1" "1.0" "1 2" "１２" "1/../../evil"))
    (should-error (douban--metadata-id "subject-id" bad)))
  (should (equal (douban--metadata-id "subject-id" "42") "42")))

(ert-deftest douban-test-metadata-bools-are-explicit ()
  (dolist (empty '(nil "" " \t"))
    (should-not (douban--metadata-bool "spoiler" empty)))
  (should (eq (douban--metadata-bool "spoiler" " TRUE ") t))
  (should-not (douban--metadata-bool "spoiler" " False "))
  (dolist (bad '("yes" t 0 :json-null))
    (should-error
     (douban--metadata-bool "spoiler" bad)
     :type 'error)))

(ert-deftest douban-test-metadata-text-must-be-single-line ()
  (should-error
   (douban--metadata-text "introduction" "第一行\n第二行")
   :type 'error)
  (should-error
   (douban--metadata-text "title" "标题\t注入")
   :type 'error)
  (should-error
   (douban--metadata-list
    "platforms" '("PC\n#+TITLE: injected"))
   :type 'error)
  (should-error
   (douban--metadata-list
    "platforms" "PC\n#+TITLE: injected")
   :type 'error)
  (should-error
   (douban--metadata-list
    "platforms" '("PC,掌机" "x"))
   :type 'error))

(ert-deftest douban-test-parse-subject-supports-game-url ()
  (should
   (equal
    (douban--parse-subject
     "https://www.douban.com/game/36932396/")
    '(:subject-id "36932396" :subject-type "game")))
  (should
   (equal
    (douban--parse-subject
     "https://book.douban.com/subject/1008145/")
    '(:subject-id "1008145" :subject-type "book")))
  (should-error
   (douban--parse-subject
    "https://evil.example/game/36932396/")
   :type 'user-error)
  (should-error (douban--parse-subject "36932396")
                :type 'user-error)
  (should-error
   (douban--parse-subject
    "http://book.douban.com/subject/1008145/")
   :type 'user-error)
  (should-error
   (douban--parse-subject
    "https://book.douban.com/subject/1008145/?from=test")
   :type 'user-error))

(ert-deftest douban-test-search-subjects-stays-within-selected-type ()
  (let (requests)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (_method url &rest options)
            (let* ((headers (plist-get options :headers))
                   (type
                    (and
                     (string-match "[?&]type=\\([^&]+\\)" url)
                     (match-string 1 url)))
                   (items
                    (pcase type
                      ("book"
                       '(((layout . "subject")
                          (target_id . 1)
                          (target_type . "book")
                          (target
                           (title . "同名条目")
                           (card_subtitle . "相同副标题")))
                         ((layout . "subject")
                          (target_id . 1)
                          (target_type . "book")
                          (target
                           (title . "同名条目")
                           (card_subtitle . "重复项")))))
                      ("movie"
                       '(((layout . "subject")
                          (target_id . 1)
                          (target_type . "movie")
                          (target
                           (title . "同名条目")
                           (card_subtitle . "相同副标题")))
                         ((layout . "subject")
                          (target_id . 1)
                          (target_type . "tv")
                          (target
                           (title . "同名条目")
                           (card_subtitle . "相同副标题")))
                         ((layout . "subject")
                          (target_id . 1)
                          (target_type . "tv")
                          (target
                           (title . "同名条目")
                           (card_subtitle . "重复剧集")))))
                      ("music"
                       '(((layout . "subject")
                          (target_id . 1)
                          (target_type . "music")
                          (target
                           (title . "同名条目")
                           (card_subtitle . "相同副标题")))
                         ((layout . "subject")
                          (target_id . 1)
                          (target_type . "music")
                          (target
                           (title . "同名条目")
                           (card_subtitle . "重复项")))))
                      ("ilmen"
                       '(((layout . "subject")
                          (target_id . 1)
                          (target_type . "game")
                          (target
                           (title . "同名条目")
                           (card_subtitle . "相同副标题")))
                         ((layout . "subject")
                          (target_id . 1)
                          (target_type . "game")
                          (target
                           (title . "同名条目")
                           (card_subtitle . "重复项"))))))))
              (push (list type headers url) requests)
              (make-plz-response
               :status 200
               :body
               (json-encode
                `((subjects
                   . ((items . ,(vconcat items)))))))))))
      (dolist
          (case
           '(("book" "book")
             ("movie" "movie")
             ("tv" "movie")
             ("music" "music")
             ("game" "ilmen")))
        (let* ((subject-type (car case))
               (subjects
                (douban--search-subjects
                 "同名条目" subject-type)))
          (should (= (length subjects) 1))
          (should
           (equal
            (list
             (plist-get (car subjects) :subject-type)
             (plist-get (car subjects) :subject-id))
            (list subject-type "1")))))
      (should
       (equal
        (mapcar #'car (nreverse requests))
        '("book" "movie" "movie" "music" "ilmen")))
      (dolist (request requests)
        (let ((headers (cadr request))
              (url (caddr request)))
          (should (string-match-p "[?&]count=20\\(?:&\\|\\'\\)" url))
          (should-not (string-match-p "[?&]count=5\\(?:&\\|\\'\\)" url))
          (should
           (equal
            (cdr (assoc-string "Referer" headers t))
            "https://www.douban.com/search"))
          (should-not (assoc-string "Cookie" headers t)))))))

(ert-deftest douban-test-review-subject-type-completion-is-explicit ()
  (dolist
      (case
       '(("图书" "book")
         ("电影" "movie")
         ("剧集" "tv")
         ("音乐" "music")
         ("游戏" "game")))
    (let (prompt collection require-match)
      (cl-letf
          (((symbol-function 'completing-read)
            (lambda
                (text choices _predicate match &rest _arguments)
              (setq prompt text
                    collection choices
                    require-match match)
              (car case))))
        (should
         (equal
          (douban--read-review-subject-type)
          (cadr case))))
      (should (equal prompt "长评品类: "))
      (should
       (equal collection
              douban--review-subject-type-choices))
      (should require-match))))

(ert-deftest douban-test-search-subject-searches-numeric-title-once ()
  (let (query type require-match selected)
    (cl-letf
        (((symbol-function 'read-string)
          (lambda (&rest _arguments) " 2046 "))
         ((symbol-function 'douban--search-subjects)
          (lambda (input subject-type)
            (setq query input
                  type subject-type)
            (list
             (list
              :subject-id "1291546"
              :subject-type "movie"
              :title "2046"
              :summary "中国香港 / 剧情 爱情"))))
         ((symbol-function 'completing-read)
          (lambda
              (_prompt collection _predicate match &rest _arguments)
            (setq require-match match)
            (caar collection))))
      (setq selected
            (douban-search-subject "movie")))
    (should (equal query "2046"))
    (should (equal type "movie"))
    (should require-match)
    (should
     (equal selected
            "https://movie.douban.com/subject/1291546/"))))

(ert-deftest douban-test-search-subject-interactive-inserts-url ()
  (with-temp-buffer
    (insert "原有正文")
    (let (reported events)
      (cl-letf
          (((symbol-function 'douban--read-review-subject-type)
            (lambda ()
              (push 'type events)
              "book"))
           ((symbol-function 'read-string)
            (lambda (&rest _arguments)
              (push 'input events)
              "https://book.douban.com/subject/1008145"))
           ((symbol-function 'douban--search-subjects)
            (lambda (&rest _arguments)
              (ert-fail "规范 URL 不应触发名称搜索")))
           ((symbol-function 'message)
            (lambda (format-string &rest arguments)
              (push 'message events)
              (setq reported
                    (apply #'format format-string arguments)))))
        (should
         (equal
          (let ((noninteractive nil))
            (call-interactively #'douban-search-subject))
          "https://book.douban.com/subject/1008145/")))
      (should
       (equal reported
              (concat
               "douban: 已插入条目 URL："
               "https://book.douban.com/subject/1008145/")))
      (should (equal (nreverse events) '(type input message)))
      (should
       (equal
        (buffer-string)
        (concat
         "原有正文"
         "https://book.douban.com/subject/1008145/"))))))

(ert-deftest douban-test-search-subject-canonical-url-skips-search ()
  (let ((input "https://book.douban.com/subject/1008145/"))
    (cl-letf
        (((symbol-function 'read-string)
          (lambda (&rest _arguments) input))
         ((symbol-function 'douban--search-subjects)
          (lambda (&rest _arguments)
            (error "canonical URL must not search")))
         ((symbol-function 'completing-read)
          (lambda (&rest _arguments)
            (error "canonical URL needs no completion"))))
      (should
       (equal
        (douban-search-subject "book")
        input))))
  (let ((input "https://movie.douban.com/subject/25754848/"))
    (cl-letf
        (((symbol-function 'read-string)
          (lambda (&rest _arguments) input)))
      (should
       (equal
        (douban-search-subject "tv")
        input))
      (should
       (equal
        (douban--review-subject-from-url "tv" input)
        '(:subject-id "25754848" :subject-type "tv")))))
  (cl-letf
      (((symbol-function 'read-string)
        (lambda (&rest _arguments)
          "https://music.douban.com/subject/1/"))
       ((symbol-function 'douban--search-subjects)
        (lambda (&rest _arguments)
          (error "URL-shaped input must not search"))))
    (should-error
     (douban-search-subject "book")
     :type 'user-error))
  (cl-letf
      (((symbol-function 'read-string)
        (lambda (&rest _arguments)
          "https://evil.example/subject/1/"))
       ((symbol-function 'douban--search-subjects)
        (lambda (&rest _arguments)
          (error "URL-shaped input must not search"))))
    (should-error
     (douban-search-subject "book")
     :type 'user-error)))

(ert-deftest douban-test-new-review-reuses-search-subject ()
  (let* ((directory (make-temp-file "douban-review-url-" t))
         (file (expand-file-name "剧集长评.md" directory))
         (douban-review-directory directory)
         events)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'completing-read)
                (lambda
                    (prompt _collection _predicate require-match
                            &rest _arguments)
                  (should (string-prefix-p "长评品类" prompt))
                  (should require-match)
                  (push 'type events)
                  "剧集"))
               ((symbol-function 'douban-search-subject)
                (lambda (subject-type &optional input)
                  (should (equal subject-type "tv"))
                  (should-not input)
                  (push 'subject events)
                  "https://movie.douban.com/subject/25754848/"))
               ((symbol-function 'read-file-name)
                (lambda (_prompt default-directory &rest _arguments)
                  (should
                   (equal
                    default-directory
                    (file-name-as-directory directory)))
                  (push 'file events)
                  file))
               ((symbol-function 'tab-new)
                (lambda () (push 'tab events))))
            (call-interactively #'douban-new-review))
          (should
           (equal (nreverse events) '(type subject file tab)))
          (should (equal (buffer-file-name) file))
          (should douban-mode)
          (let ((meta (douban--read-meta file)))
            (should
             (equal
              (plist-get meta :subject-id) "25754848"))
            (should
             (equal (plist-get meta :subject-type) "tv"))))
      (when-let* ((buffer (find-buffer-visiting file)))
        (kill-buffer buffer))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest douban-test-searched-subject-title-stays-out-of-review-source ()
  (let* ((directory (make-temp-file "douban-search-title-" t))
         (file (expand-file-name "文件名也不是标题.md" directory))
         (douban-review-directory directory)
         events)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'read-string)
                (lambda (&rest _arguments)
                  (push 'input events)
                  "琅琊榜"))
               ((symbol-function 'douban--search-subjects)
                (lambda (query subject-type)
                  (push
                   (list 'search query subject-type)
                   events)
                  (list
                   (list
                    :subject-id "25754848"
                    :subject-type "tv"
                    :title "琅琊榜"
                    :summary "中国大陆 / 剧情 古装"))))
               ((symbol-function 'completing-read)
                (lambda
                    (prompt collection _predicate require-match
                             &rest _arguments)
                  (should require-match)
                  (if (string-prefix-p "长评品类" prompt)
                      (progn
                        (push 'type events)
                        "剧集")
                    (push 'subject events)
                    (caar collection))))
               ((symbol-function 'read-file-name)
                (lambda (_prompt default-directory &rest _arguments)
                  (should
                   (equal
                    default-directory
                    (file-name-as-directory directory)))
                  (push 'file events)
                  file))
               ((symbol-function 'tab-new)
                (lambda () (push 'tab events))))
            (call-interactively #'douban-new-review))
          (should
           (equal
            (nreverse events)
            '(type
              input
              (search "琅琊榜" "tv")
              subject
              file
              tab)))
          (should douban-mode)
          (let ((meta (douban--read-meta file))
                (text
                 (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
            (should
             (equal
              (plist-get meta :subject-id) "25754848"))
            (should
             (equal (plist-get meta :subject-type) "tv"))
            (should-not (plist-get meta :title))
            (should-not
             (string-match-p (regexp-quote "琅琊榜") text))
            (should-not
             (string-match-p
              (regexp-quote "文件名也不是标题") text))))
      (when-let* ((buffer (find-buffer-visiting file)))
        (kill-buffer buffer))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest douban-test-editor-lengths-use-utf16 ()
  (let ((emoji-70 (apply #'concat (make-list 70 "😀")))
        (emoji-71 (apply #'concat (make-list 71 "😀")))
        (emoji-100 (apply #'concat (make-list 100 "😀")))
        (emoji-101 (apply #'concat (make-list 101 "😀"))))
    (should
     (douban-test--review-meta
     (list
       :subject-id "1"
       :introduction emoji-70)
      "标题"))
    (should-error
     (douban-test--review-meta
     (list
       :subject-id "1"
       :introduction emoji-71)
      "标题")
     :type 'error)
    (should
     (equal
      (douban--require-title emoji-100 "评论" 200)
      emoji-100))
    (should-error
     (douban--require-title emoji-101 "评论" 200)
     :type 'user-error)
    (should
     (= 2
        (douban--draft-character-count
         (douban--html-to-draft "<p>😀</p>"))))))

(ert-deftest douban-test-markdown-meta-roundtrip-preserves-document ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\n"
    "title: \"我的评论\"\n"
    "banner: \"./images/banner.jpg\"\n"
    "tags: [one, two]\n"
    "douban:\n"
    "  review:\n"
    "    subject-id: \"123\"\n"
    "    subject-type: book\n"
    "    rating: 4\n"
    "    spoiler: true\n"
    "---\n\n"
    "# 正文\n\n内容。\n")
   (let ((meta (douban--read-meta file)))
     (should (equal (plist-get meta :subject-id) "123"))
     (should (equal (plist-get meta :title) "我的评论"))
     (should (= (plist-get meta :rating) 4))
     (should (plist-get meta :spoiler))
     (setq meta (plist-put meta :review-id "987"))
     (douban--write-meta file meta))
   (let ((text
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string)))
         (roundtrip (douban--read-meta file)))
     (should (string-match-p "tags: \\[one, two\\]" text))
     (should
      (string-match-p
       "banner: \"\\./images/banner\\.jpg\"" text))
     (should (string-match-p "# 正文\n\n内容。" text))
     (should
     (= (length (split-string text "^douban:" t)) 2))
     (should (equal (plist-get roundtrip :review-id) "987")))))

(ert-deftest douban-test-metadata-write-preserves-dos-eol-and-mode ()
  (let ((file (make-temp-file "douban-test-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert
             "---\n"
             "title: \"换行测试\"\n"
             "douban:\n"
             "  review:\n"
             "    subject-id: \"123\"\n"
             "    subject-type: book\n"
             "---\n\n正文\n")
            (let ((coding-system-for-write 'utf-8-dos))
              (write-region
               (point-min) (point-max) file nil 'silent)))
          (set-file-modes file #o640)
          (let ((meta (douban--read-meta file)))
            (setq meta (plist-put meta :review-id "456"))
            (douban--write-meta file meta))
          (should (= (logand (file-modes file) #o777) #o640))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file)
            (let ((bytes (buffer-string)))
              (should (string-match-p "\r\n" bytes))
              (should-not
               (string-match-p "\\(?:^\\|[^\r]\\)\n" bytes)))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest douban-test-markdown-yaml-strings-roundtrip-backslashes ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\n"
    "title: '路径 C:\\书库 和作者 O''Neil'\n"
    "douban:\n"
    "  review:\n"
    "    subject-id: '123'\n"
    "    subject-type: book\n"
    "    introduction: 'a\\b 与 O''Neil'\n"
    "---\n\n正文\n")
   (let ((meta (douban--read-meta file)))
     (should
      (equal
       (plist-get meta :title)
       "路径 C:\\书库 和作者 O'Neil"))
     (should
      (equal
       (plist-get meta :introduction)
       "a\\b 与 O'Neil"))
     (douban--write-meta file meta))
   (let ((roundtrip (douban--read-meta file)))
     (should
      (equal
       (plist-get roundtrip :title)
       "路径 C:\\书库 和作者 O'Neil"))
     (should
      (equal
       (plist-get roundtrip :introduction)
       "a\\b 与 O'Neil")))))

(ert-deftest douban-test-markdown-rejects-quoted-douban-key ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\n"
    "title: 'quoted key'\n"
    "'douban':\n"
    "  review:\n"
    "    subject-id: '1'\n"
    "    subject-type: book\n"
    "---\n\n正文\n")
   (let ((err (should-error (douban--read-meta file) :type 'error)))
     (should
      (string-match-p
       "必须直接写成未加引号的顶层 douban:"
       (error-message-string err))))))





(ert-deftest douban-test-draft-inline-ranges-use-utf16 ()
  (let* ((raw
          (douban--html-to-draft
           "<p>A<strong>😀B</strong><a href=\"https://example.com\">链</a></p>"))
         (block (aref (plist-get raw :blocks) 0))
         (style (aref (plist-get block :inlineStyleRanges) 0))
         (entity-range (aref (plist-get block :entityRanges) 0))
         (entity
          (gethash "0" (plist-get raw :entityMap))))
    (should (equal (plist-get block :text) "A😀B链"))
    (should (= (plist-get style :offset) 1))
    ;; Emoji is two UTF-16 code units, followed by B.
    (should (= (plist-get style :length) 3))
    (should (= (plist-get entity-range :offset) 4))
    (should (= (plist-get entity-range :length) 1))
    (should (equal (plist-get entity :type) "LINK"))
    (should
     (equal
      (plist-get (plist-get entity :data) :url)
      "https://example.com"))))

(ert-deftest douban-test-block-write-maintains-cached-utf16-length ()
  (let* ((draft (douban--new-draft))
         (block
          (douban--draft-add-block draft "unstyled")))
    (should (= (douban--block-utf16-length block) 0))
    (should (= (douban--block-offset block) 0))
    (douban--block-write block "A😀")
    (should (equal (douban--block-text block) "A😀"))
    (should (= (douban--block-utf16-length block) 3))
    (douban--block-write block "中\n")
    (should (equal (douban--block-text block) "A😀中\n"))
    (should (= (douban--block-utf16-length block) 5))
    (cl-letf
        (((symbol-function 'douban--utf16-length)
          (lambda (_text)
            (ert-fail
             "`douban--block-offset' 不应重新扫描 block text"))))
      (should (= (douban--block-offset block) 5)))
    (should-error
     (macroexpand
      '(setf (douban--block-text block) "绕过写入入口"))
     :type 'error)
    (should-error
     (macroexpand
      '(setf (douban--block-utf16-length block) 999))
     :type 'error)))

(ert-deftest douban-test-draft-user-mention-is-native-user-entity ()
  (let* ((raw
          (douban--html-to-draft
           (concat
            "<p>前<a "
            "href=\"https://www.douban.com/people/example/\" "
            "title=\"douban-user-mention:42\">@张😀</a>后</p>")))
         (block (aref (plist-get raw :blocks) 0))
         (range (aref (plist-get block :entityRanges) 0))
         (entity (douban-test--first-draft-entity raw))
         (data (plist-get entity :data)))
    (should (equal (plist-get block :text) "前@张😀后"))
    (should (= (plist-get range :offset) 1))
    (should (= (plist-get range :length) 4))
    (should (equal (plist-get entity :type) "USER"))
    (should (equal (plist-get entity :mutability) "IMMUTABLE"))
    (should
     (equal
      data
      '(:url "https://www.douban.com/people/example/"
        :name "张😀"
        :display "inline"
        :id "42")))))

(ert-deftest douban-test-draft-user-mention-rejects-invalid-markers ()
  (dolist
      (html
       '("<p><a href=\"https://www.douban.com/people/test/\" title=\"douban-user-mention:nope\">@用户</a></p>"
         "<p><a href=\"https://example.com/people/test/\" title=\"douban-user-mention:42\">@用户</a></p>"
         "<p><a href=\"https://www.douban.com/people/test/\" title=\"douban-user-mention:42\">用户</a></p>"))
    (should-error
     (douban--html-to-draft html)
     :type 'user-error))
  (dolist
      (html
       '("<p>裸写 @用户</p>"
         "<p><a href=\"https://www.douban.com/people/test/\">@用户</a></p>"))
    (let ((raw (douban--html-to-draft html)))
      (should-not
       (douban--draft-has-entity-type-p raw "USER")))))

(ert-deftest douban-test-inline-highlight-uses-mark-style ()
  (let* ((raw
          (douban--html-to-draft
           (concat
            "<p>甲<mark>"
            "乙<strong>😀丙</strong></mark>丁"
            "<mark>戊</mark></p>")))
         (block (aref (plist-get raw :blocks) 0))
         (ranges
          (append (plist-get block :inlineStyleRanges) nil))
         (first-mark
          (cl-find-if
           (lambda (range)
             (and
              (equal (plist-get range :style) "MARK")
              (= (plist-get range :offset) 1)))
           ranges))
         (second-mark
          (cl-find-if
           (lambda (range)
             (and
              (equal (plist-get range :style) "MARK")
              (= (plist-get range :offset) 6)))
           ranges))
         (bold
          (cl-find-if
           (lambda (range)
             (equal (plist-get range :style) "BOLD"))
           ranges)))
    (should (equal (plist-get block :type) "unstyled"))
    (should (equal (plist-get block :text) "甲乙😀丙丁戊"))
    (should (= (length ranges) 3))
    (should (= (plist-get first-mark :length) 4))
    (should (= (plist-get second-mark :length) 1))
    (should (= (plist-get bold :offset) 2))
    (should (= (plist-get bold :length) 3))))

(ert-deftest douban-test-annotation-source-blockquotes-remain-ordinary ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (case
       '((".md" . "> 第一段\n>\n> 第二段\n")))
    (douban-test--with-temp-file (car case) (cdr case)
      (pcase-let
          ((`(,raw ,count ,_directory)
            (douban--prepare-draft
             file 'annotation (lambda (_raw) 6))))
        (let ((blocks (append (plist-get raw :blocks) nil)))
          (should (= count 6))
          (should
           (equal
            (mapcar (lambda (block) (plist-get block :type)) blocks)
            '("blockquote" "blockquote")))
          (should
           (equal
            (mapcar (lambda (block) (plist-get block :text)) blocks)
            '("第一段" "第二段")))
          (should-not
           (cl-find
            "original_quote" blocks :test #'equal
            :key (lambda (block) (plist-get block :type)))))))))

(ert-deftest douban-test-draft-raw-serialization-is-idempotent ()
  (let* ((draft (douban--new-draft))
         (first (douban--draft-add-block draft "unstyled"))
         (entity
          (douban--draft-add-entity
           draft "LINK" "MUTABLE"
           (list :url "https://example.com")))
         second first-raw second-raw)
    (douban--block-write first "甲乙")
    (douban--block-add-inline-range first "BOLD" 0 1)
    (douban--block-add-inline-range first "ITALIC" 1 1)
    (douban--block-add-entity-range first entity 0 1)
    (douban--block-add-entity-range first entity 1 1)
    (setq second (douban--draft-add-block draft "blockquote"))
    (douban--block-write second "丙")
    (setq first-raw (douban--draft-raw draft)
          second-raw (douban--draft-raw draft))
    (should (equal first-raw second-raw))
    (should
     (equal
      (mapcar
       (lambda (block) (plist-get block :text))
       (append (plist-get second-raw :blocks) nil))
      '("甲乙" "丙")))
    (let ((raw-block (aref (plist-get second-raw :blocks) 0)))
      (should (= (length (plist-get raw-block :inlineStyleRanges)) 2))
      (should (= (length (plist-get raw-block :entityRanges)) 2))
      (should
       (equal
        (mapcar
         (lambda (range) (plist-get range :style))
         (append (plist-get raw-block :inlineStyleRanges) nil))
        '("BOLD" "ITALIC"))))))

(ert-deftest douban-test-draft-block-types-and-list-depth ()
  (let* ((raw
          (douban--html-to-draft
           (concat
            "<h1>一级</h1><h3>三级</h3>"
            "<ul><li>甲<ul><li>乙</li></ul></li></ul>"
            "<pre><code>x &lt; y</code></pre>")))
         (blocks (append (plist-get raw :blocks) nil)))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :type)) blocks)
      '("header-two" "header-three"
        "unordered-list-item" "unordered-list-item"
        "code-block")))
    (should (= (plist-get (nth 2 blocks) :depth) 0))
    (should (= (plist-get (nth 3 blocks) :depth) 1))
    (should (equal (plist-get (nth 4 blocks) :text) "x < y"))))

(ert-deftest douban-test-draft-loose-list-keeps-paragraph-boundaries ()
  (let* ((raw
          (douban--html-to-draft
           (concat
            "<ul><li><p>甲<strong>一</strong></p><p>乙</p>"
            "<ol><li><p>丙</p><p>丁</p></li></ol>"
            "<p>戊</p></li></ul>")))
         (blocks (append (plist-get raw :blocks) nil)))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :text)) blocks)
      '("甲一" "乙" "丙" "丁" "戊")))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :type)) blocks)
      '("unordered-list-item" "unordered-list-item"
        "ordered-list-item" "ordered-list-item"
        "unordered-list-item")))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :depth)) blocks)
      '(0 0 1 1 0)))
    (let ((range
           (aref (plist-get (car blocks) :inlineStyleRanges) 0)))
      (should (= (plist-get range :offset) 1))
      (should (= (plist-get range :length) 1))
      (should (equal (plist-get range :style) "BOLD")))))

(ert-deftest douban-test-draft-blockquote-keeps-nested-blocks-and-lists ()
  (let* ((raw
          (douban--html-to-draft
           (concat
            "<blockquote>引<p>甲</p>"
            "<div><p>乙</p><p>丙</p></div>"
            "<ul><li><p>丁</p><p>戊</p></li></ul>"
            "<p>尾</p></blockquote>")))
         (blocks (append (plist-get raw :blocks) nil)))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :text)) blocks)
      '("引" "甲" "乙" "丙" "丁" "戊" "尾")))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :type)) blocks)
      '("blockquote" "blockquote" "blockquote" "blockquote"
        "unordered-list-item" "unordered-list-item"
        "blockquote")))))

(ert-deftest douban-test-pandoc-highlight-block-markers ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (case
       '(("markdown+mark"
          . "==第一段==\n\n==第二段==\n")))
    (let* ((html
            (douban--pandoc-to-html (car case) (cdr case)))
           (raw (douban--html-to-draft html))
           (blocks (append (plist-get raw :blocks) nil)))
      (should
       (string-match-p
        "<div data-douban-highlight-block=\"true\">"
        html))
      (should
       (equal
        (mapcar (lambda (block) (plist-get block :type)) blocks)
        '("highlight-block" "highlight-block")))
      (should
       (equal
        (mapcar (lambda (block) (plist-get block :text)) blocks)
        '("第一段" "第二段")))
      (dolist (block blocks)
        (should
         (equal (plist-get block :data) '(:align "")))
        (should
         (= (length (plist-get block :inlineStyleRanges)) 0))))))

(ert-deftest douban-test-legacy-highlight-containers-no-longer-create-blocks ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (case
       '(("markdown+mark"
          . "::: douban-highlight\n第一段\n\n第二段\n:::\n")))
    (let* ((html
            (douban--pandoc-to-html (car case) (cdr case)))
           (raw (douban--html-to-draft html))
           (blocks (append (plist-get raw :blocks) nil)))
      (should
       (equal
        (mapcar (lambda (block) (plist-get block :type)) blocks)
        '("unstyled" "unstyled")))
      (dolist (block blocks)
        (should
         (= (length (plist-get block :inlineStyleRanges)) 0))))))

(ert-deftest douban-test-pandoc-inline-highlight-markers ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (case
       '((".md" . "普通 ==高亮文字== 结尾。\n")))
    (douban-test--with-temp-file
        (car case) (cdr case)
      (let* ((html (douban--source-html file))
             (raw (douban--html-to-draft html))
             (block (aref (plist-get raw :blocks) 0))
             (range
              (aref (plist-get block :inlineStyleRanges) 0)))
        (should
         (string-match-p
          "<mark>高亮文字</mark>"
          html))
        (should (equal (plist-get block :type) "unstyled"))
        (should (equal (plist-get range :style) "MARK"))
        (should (= (plist-get range :offset) 3))
        (should (= (plist-get range :length) 4))))))


(ert-deftest douban-test-highlight-block-keeps-inline-ranges-and-utf16 ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (case
       '(("markdown+mark"
          . "==A😀**粗B**[链😀](https://example.com)==\n\n==第二段==\n")))
    (let* ((html
            (douban--pandoc-to-html (car case) (cdr case)))
           (raw (douban--html-to-draft html))
           (blocks (plist-get raw :blocks))
           (first (aref blocks 0))
           (second (aref blocks 1))
           (style (aref (plist-get first :inlineStyleRanges) 0))
           (entity-range (aref (plist-get first :entityRanges) 0))
           (entity (douban-test--first-draft-entity raw)))
      (should (= (length blocks) 2))
      (should (equal (plist-get first :type) "highlight-block"))
      (should (equal (plist-get first :data) '(:align "")))
      (should (equal (plist-get first :text) "A😀粗B链😀"))
      (should (equal (plist-get style :style) "BOLD"))
      (should (= (plist-get style :offset) 3))
      (should (= (plist-get style :length) 2))
      (should (= (plist-get entity-range :offset) 5))
      (should (= (plist-get entity-range :length) 3))
      (should (equal (plist-get entity :type) "LINK"))
      (should (equal (plist-get entity :mutability) "MUTABLE"))
      (should
       (equal
        (plist-get (plist-get entity :data) :url)
        "https://example.com"))
      (should (equal (plist-get second :type) "highlight-block"))
      (should (equal (plist-get second :data) '(:align "")))
      (should (equal (plist-get second :text) "第二段")))))

(ert-deftest douban-test-legacy-highlight-classes-are-transparent ()
  (let* ((raw
          (douban--html-to-draft
           (concat
            "<blockquote><p>普通引用</p></blockquote>"
            "<div class=\"douban-highlight\"><p>普通容器零</p></div>"
            "<div class=\"douban-highlighted\"><p>普通容器一</p></div>"
            "<div class=\"not-douban-highlight\"><p>普通容器二</p></div>")))
         (blocks (append (plist-get raw :blocks) nil)))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :type)) blocks)
      '("blockquote" "unstyled" "unstyled" "unstyled")))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :text)) blocks)
      '("普通引用" "普通容器零" "普通容器一" "普通容器二")))))

(ert-deftest douban-test-highlight-block-rejects-invalid-generated-content ()
  (dolist
      (contents
       (list
        ""
        " \n\t "
        "<p> </p>"
        "<p><img src=\"photo.png\" alt=\"图片\"></p>"
        (concat
         "<p>文字<img src=\"photo.png\" alt=\"图片\"></p>")
        (douban-test--card-html)
        "<p><span><div>嵌套区块</div></span></p>"))
    (should-error
     (douban--html-to-draft
      (format
       "<div data-douban-highlight-block=\"true\">%s</div>"
       contents))
     :type 'user-error)))

(ert-deftest douban-test-full-mark-only-promotes-top-level-paragraphs ()
  (skip-unless (executable-find "pandoc"))
  (let* ((html
          (douban--pandoc-to-html
           "markdown+mark"
           (concat
            "- ==列表==\n\n"
            "> ==引用==\n\n"
            "## ==标题==\n\n"
            "<div style=\"text-align: center\">\n\n"
            "==居中==\n\n"
            "</div>\n")))
         (raw (douban--html-to-draft html))
         (blocks (append (plist-get raw :blocks) nil)))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :type)) blocks)
      '("unordered-list-item" "blockquote" "header-two" "unstyled")))
    (should
     (equal
      (mapcar (lambda (block) (plist-get block :text)) blocks)
      '("列表" "引用" "标题" "居中")))
    (dolist (block blocks)
      (let ((range
             (aref (plist-get block :inlineStyleRanges) 0)))
        (should (equal (plist-get range :style) "MARK"))))
    (should
     (equal
      (plist-get (nth 3 blocks) :data)
      '(:align "center")))))

(ert-deftest douban-test-raw-html-mark-remains-inline ()
  (skip-unless (executable-find "pandoc"))
  (let* ((html
          (douban--pandoc-to-html
           "markdown+mark"
           "<mark>HTML 高亮</mark>\n"))
         (raw (douban--html-to-draft html))
         (block (aref (plist-get raw :blocks) 0))
         (range
          (aref (plist-get block :inlineStyleRanges) 0)))
    (should (equal (plist-get block :type) "unstyled"))
    (should (equal (plist-get range :style) "MARK"))
    (should (= (plist-get range :offset) 0))
    (should (= (plist-get range :length) 7))))

(ert-deftest douban-test-multiple-marks-remain-inline ()
  (let* ((raw
          (douban--html-to-draft
           "<p><mark>第一处</mark>与<mark>第二处</mark></p>"))
         (block (aref (plist-get raw :blocks) 0))
         (ranges (plist-get block :inlineStyleRanges)))
    (should (equal (plist-get block :type) "unstyled"))
    (should (= (length ranges) 2))
    (dotimes (index 2)
      (should
       (equal
        (plist-get (aref ranges index) :style)
        "MARK")))))

(ert-deftest douban-test-markdown-center-keeps-blocks-and-inline-content ()
  (skip-unless (executable-find "pandoc"))
  (let* ((html
          (douban--pandoc-to-html
           "markdown+mark"
           (concat
            "<div style=\"text-align: center\">\n\n"
            "甲 **粗体** [链接](https://example.com)\n\n"
            "## 标题 *斜体*\n\n"
            "</div>\n")))
         (raw (douban--html-to-draft html))
         (blocks (append (plist-get raw :blocks) nil))
         (paragraph (nth 0 blocks))
         (heading (nth 1 blocks))
         (paragraph-styles
          (append (plist-get paragraph :inlineStyleRanges) nil))
         (heading-styles
          (append (plist-get heading :inlineStyleRanges) nil))
         (link-range (aref (plist-get paragraph :entityRanges) 0))
         (link
          (gethash
           (number-to-string (plist-get link-range :key))
           (plist-get raw :entityMap))))
    (should (= (length blocks) 2))
    (should (equal (plist-get paragraph :type) "unstyled"))
    (should (equal (plist-get paragraph :text) "甲 粗体 链接"))
    (should (equal (plist-get paragraph :data) '(:align "center")))
    (should
     (cl-find-if
      (lambda (range)
        (equal (plist-get range :style) "BOLD"))
      paragraph-styles))
    (should (equal (plist-get link :type) "LINK"))
    (should
     (equal
      (plist-get (plist-get link :data) :url)
      "https://example.com"))
    (should (equal (plist-get heading :type) "header-two"))
    (should (equal (plist-get heading :text) "标题 斜体"))
    (should (equal (plist-get heading :data) '(:align "center")))
    (should
     (cl-find-if
      (lambda (range)
        (equal (plist-get range :style) "ITALIC"))
      heading-styles))))

(ert-deftest douban-test-markdown-center-recognizes-css-declarations ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (style
       '("text-align:center"
         "color: red; TEXT-ALIGN : CENTER;"))
    (let* ((html
            (douban--pandoc-to-html
             "markdown+mark"
             (format
              "<div style=\"%s\">\n\n居中\n\n</div>\n"
              style)))
           (raw (douban--html-to-draft html))
           (block (aref (plist-get raw :blocks) 0)))
      (should
       (equal
        (plist-get block :data)
        '(:align "center"))))))

(ert-deftest douban-test-center-rejects-other-css-meanings ()
  (dolist
      (html
       (list
        "<div style=\"text-align: left\"><p>左对齐</p></div>"
        "<div style=\"x-text-align: center\"><p>其他属性</p></div>"
        "<div style=\"text-align: centered\"><p>其他值</p></div>"
        (concat
          "<div style=\"text-align: center; text-align: left\">"
          "<p>最后声明优先</p></div>")
        "<section style=\"text-align: center\"><p>其他元素</p></section>"
        "<p style=\"text-align: center\">其他元素</p>"))
    (let* ((raw (douban--html-to-draft html))
           (block (aref (plist-get raw :blocks) 0))
           (data (plist-get block :data)))
      (should (hash-table-p data))
      (should (= (hash-table-count data) 0)))))

(ert-deftest douban-test-markdown-center-no-longer-uses-douban-class ()
  (skip-unless (executable-find "pandoc"))
  (let* ((html
          (douban--pandoc-to-html
           "markdown+mark"
           "::: douban-center\n居中\n:::\n"))
         (raw (douban--html-to-draft html))
         (block (aref (plist-get raw :blocks) 0))
         (data (plist-get block :data)))
    (should (hash-table-p data))
    (should (= (hash-table-count data) 0))))


(ert-deftest douban-test-center-keeps-standalone-images-atomic ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (case
       '(("markdown+mark"
          . "<div style=\"text-align: center\">\n\n![图注](photo.png)\n\n</div>\n")))
    (let* ((html (douban--pandoc-to-html (car case) (cdr case)))
           (raw (douban--html-to-draft html))
           (blocks (plist-get raw :blocks))
           (block (aref blocks 0))
           (entity (douban-test--first-draft-entity raw)))
      (should (= (length blocks) 1))
      (should (equal (plist-get block :type) "atomic"))
      (should (equal (plist-get entity :type) "IMAGE")))))

(ert-deftest douban-test-center-rejects-invalid-child-blocks ()
  (dolist
      (contents
       (list
        "<pre><code>代码</code></pre>"
        "<table><tr><td>表格</td></tr></table>"
        "<ul><li>列表</li></ul>"
        "<blockquote><p>引用</p></blockquote>"
        (concat
          "<div style=\"text-align: center\">"
          "<p>嵌套居中</p></div>")))
    (should-error
     (douban--html-to-draft
      (format
       "<div style=\"text-align: center\">%s</div>"
       contents))
     :type 'user-error)))

(ert-deftest douban-test-center-rejects-list-and-quote-placement ()
  (dolist
      (html
       (list
        (concat
         "<ul><li><div style=\"text-align: center\">"
         "<p>居中</p></div></li></ul>")
        (concat
          "<blockquote><div style=\"text-align: center\">"
          "<p>居中</p></div></blockquote>")))
    (should-error
     (douban--html-to-draft html)
     :type 'user-error)))

(ert-deftest douban-test-draft-figure-image-and-caption ()
  (let* ((raw
          (douban--html-to-draft
           (concat
            "<figure><img src=\"photo.png\" alt=\"\">"
            "<figcaption>图注</figcaption></figure>")))
         (block (aref (plist-get raw :blocks) 0))
         (entity
          (gethash "0" (plist-get raw :entityMap))))
    (should (equal (plist-get block :type) "atomic"))
    (should (equal (plist-get block :text) " "))
    (should (equal (plist-get entity :type) "IMAGE"))
    (should
     (equal
      (plist-get (plist-get entity :data) :caption)
      "图注"))))

(ert-deftest douban-test-draft-separator-is-atomic-entity ()
  (let* ((raw
          (douban--html-to-draft
           "<p>前文</p><hr><p>后文</p>"))
         (blocks (append (plist-get raw :blocks) nil))
         (block (nth 1 blocks))
         (range (aref (plist-get block :entityRanges) 0))
         (entity
          (gethash
           (number-to-string (plist-get range :key))
           (plist-get raw :entityMap))))
    (should
     (equal
      (mapcar
       (lambda (item) (plist-get item :text))
       blocks)
      '("前文" " " "后文")))
    (should (equal (plist-get block :type) "atomic"))
    (should (= (plist-get block :depth) 0))
    (should (= (length (plist-get block :inlineStyleRanges)) 0))
    (should (= (length (plist-get block :entityRanges)) 1))
    (should (= (plist-get range :offset) 0))
    (should (= (plist-get range :length) 1))
    (should (equal (plist-get entity :type) "SEPARATOR"))
    (should (equal (plist-get entity :mutability) "IMMUTABLE"))
    (should (hash-table-p (plist-get entity :data)))
    (should (= (hash-table-count (plist-get entity :data)) 0))))

(ert-deftest douban-test-draft-inline-images-degrade-to-alt-text ()
  (dolist
      (case
       '(("<p>前<img src=\"photo.png\" alt=\"替代文字\">后</p>"
          . ("unstyled" "前替代文字后"))
         ("<h2>标题<img src=\"photo.png\" alt=\"插图\"></h2>"
          . ("header-two" "标题插图"))
         ("<table><tr><td>表格<img src=\"photo.png\" alt=\"图示\"></td></tr></table>"
          . ("unstyled" "表格图示"))))
    (let* ((raw (douban--html-to-draft (car case)))
           (blocks (plist-get raw :blocks))
           (block (aref blocks 0)))
      (should (= (length blocks) 1))
      (should (equal (plist-get block :type) (cadr case)))
      (should (equal (plist-get block :text) (caddr case)))
      (should (= (hash-table-count (plist-get raw :entityMap)) 0))))
  (let* ((raw
          (douban--html-to-draft
           (concat
            "<p>甲"
            "<img src=\"empty.png\" alt=\"\">"
            "<img src=\"blank.png\" alt=\"   \">"
            "<img src=\"missing.png\">"
            "乙</p>")))
         (block (aref (plist-get raw :blocks) 0)))
    (should (equal (plist-get block :text) "甲乙"))
    (should (= (hash-table-count (plist-get raw :entityMap)) 0))))

(ert-deftest douban-test-draft-standalone-images-remain-image-entities ()
  (dolist
      (html
       '("<img src=\"photo.png\">"
         "<p><img src=\"photo.png\"></p>"
         "<p><a href=\"https://example.org/\"><img src=\"photo.png\"></a></p>"))
    (let* ((raw (douban--html-to-draft html))
           (blocks (plist-get raw :blocks))
           (entity (douban-test--first-draft-entity raw)))
      (should (= (length blocks) 1))
      (should (equal (plist-get (aref blocks 0) :type) "atomic"))
      (should (equal (plist-get entity :type) "IMAGE")))))


(ert-deftest douban-test-search-users-uses-authenticated-current-endpoint ()
  (let ((cookies '(("ck" . "search-ck")
                   ("dbcl2" . "login-cookie")))
        cookie-url)
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (setq cookie-url url)
            cookies))
         ((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (should (equal method "GET"))
            (should
             (equal cookie-url douban--user-search-endpoint))
            (should
             (equal
              url
              (concat
               douban--user-search-endpoint
               (concat
                "?q=%E5%BC%A0%20%E4%B8%89&start=0&count=10"
                "&ck=search-ck"))))
            (let ((session (plist-get arguments :session)))
              (should
               (equal
                (douban--session-cookies session)
                cookies))
              (should
               (equal
                (douban--session-referer session)
                "https://www.douban.com/")))
            (should
             (equal
              (plist-get arguments :extra-headers)
              '(("Accept" . "application/json")
                ("Referer" . "https://www.douban.com/"))))
            (list
             :status 200
             :json
             '(:users
               ((:id 42
                 :name "张三"
                 :followed t
                 :url
                 "https://www.douban.com/people/example/")
                (:id 43
                 :name "未关注"
                 :followed :json-false
                 :url
                 "https://www.douban.com/people/not-followed/")
                (:id 44
                 :name 123
                 :followed t
                 :url
                 "https://www.douban.com/people/malformed/")
                (:id "bad"
                 :name "无效候选"
                 :followed t
                 :url "https://example.com/")))))))
      (should
       (equal
        (douban--search-users " 张 三 ")
        '((:id "42"
           :name "张三"
           :url "https://www.douban.com/people/example/"))))))
  (cl-letf
      (((symbol-function 'douban--read-browser-cookies)
        (lambda (_url)
          '(("ck" . "search-ck")
            ("dbcl2" . "login-cookie"))))
       ((symbol-function 'douban--http-json)
        (lambda (&rest _arguments)
          '(:status 400
            :json
            (:code 121 :localized_message "请登录")))))
    (should-error
     (douban--search-users "张三")
     :type 'user-error)))

(ert-deftest douban-test-user-profile-url-is-strict ()
  (should
   (douban--user-profile-url-p
    "https://www.douban.com/people/example_1.2-3~/"))
  (dolist
      (url
       '("https://www.douban.com/people/../"
         "https://www.douban.com/people/./"
         "https://www.douban.com/people/example"
         "http://www.douban.com/people/example/"
         "https://www.douban.com/people/example/more/"
         "https://www.douban.com/people/example%2Fmore/"))
    (should-not (douban--user-profile-url-p url))))

(ert-deftest douban-test-insert-user-mention-supports-all-sources ()
  (let ((user
         '(:id "42"
           :name "张三"
           :url "https://www.douban.com/people/example/"))
        (searches 0)
        (selections 0))
    (cl-letf
        (((symbol-function 'douban--search-users)
          (lambda (query)
            (cl-incf searches)
            (should (equal query "张"))
            (list user)))
         ((symbol-function 'completing-read)
          (lambda (_prompt collection &rest _arguments)
            (cl-incf selections)
            (should
             (equal
              (mapcar #'car collection)
              '("张三 (42)")))
            "张三 (42)")))
      (dolist
          (case
           '(("/tmp/status.md"
              "---\ndouban:\n  status: {}\n---\n\n前后"
              "<a href=\"https://www.douban.com/people/example/\" title=\"douban-user-mention:42\">&#x40;&#x5F20;&#x4E09;</a>")
             ("/tmp/note.md"
              "---\ntitle: 日记\ndouban:\n  note: {}\n---\n\n前后"
              "<a href=\"https://www.douban.com/people/example/\" title=\"douban-user-mention:42\">&#x40;&#x5F20;&#x4E09;</a>")
             ("/tmp/review.md"
              "---\ntitle: 长评\ndouban:\n  review:\n    subject-id: '1'\n    subject-type: book\n---\n\n前后"
              "<a href=\"https://www.douban.com/people/example/\" title=\"douban-user-mention:42\">&#x40;&#x5F20;&#x4E09;</a>")))
        (with-temp-buffer
          (setq buffer-file-name (nth 0 case))
          (insert (nth 1 case))
          (goto-char (1- (point-max)))
          (douban-insert-user-mention "张")
          (should
           (string-suffix-p
            (concat "前" (nth 2 case) "后")
            (buffer-string))))))
    (should (= searches 3))
    (should (= selections 3))))

(ert-deftest douban-test-markdown-user-mention-completion-at-point ()
  (let ((user
         '(:id "42"
           :name "张三"
           :url "https://www.douban.com/people/example/"))
        (searches 0))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/status.md")
      (insert "---\ndouban:\n  status: {}\n---\n\n正文 @张")
      (cl-letf
          (((symbol-function 'douban--search-users)
            (lambda (query)
              (cl-incf searches)
              (should (equal query "张"))
              (list user))))
        (let* ((capf
                (douban-user-mention-completion-at-point))
               (start (nth 0 capf))
               (end (nth 1 capf))
               (candidates (nth 2 capf))
               (properties (nthcdr 3 capf))
               (exit-function
                (plist-get properties :exit-function))
               (candidate "张三 (42)"))
          (should (= searches 1))
          (should (equal (buffer-substring start end) "张"))
          (should (equal candidates `((,candidate . ,user))))
          (should (eq (plist-get properties :exclusive) t))
          ;; Simulate completion-at-point replacing QUERY with CANDIDATE.
          (delete-region start end)
          (goto-char start)
          (insert candidate)
          (funcall exit-function candidate 'finished)
          (should
           (string-suffix-p
            (concat
             "正文 "
             "<a href=\"https://www.douban.com/people/example/\" "
             "title=\"douban-user-mention:42\">"
             "&#x40;&#x5F20;&#x4E09;</a>")
            (buffer-string)))
          ;; Exact-query results are buffer-local and cached.
          (should (equal (douban--cached-user-completions "张")
                         (list user)))
          (should (= searches 1)))))))

(ert-deftest douban-test-markdown-user-mention-bounds-exclude-code-and-metadata ()
  (dolist
      (case
       '(("---\ntitle: \"@张\"\ndouban:\n  status: {}\n---\n\n正文"
          . nil)
         ("---\ndouban:\n  status: {}\n---\n\n邮箱 me@张"
          . nil)
         ("---\ndouban:\n  status: {}\n---\n\n行内代码 `@张`"
          . nil)
         ("---\ndouban:\n  status: {}\n---\n\n```\n@张\n```"
          . nil)
         ("---\ndouban:\n  status: {}\n---\n\n<div>\n@张\n</div>"
          . nil)
         ("---\ndouban:\n  status: {}\n---\n\n<a title=\"@张\">链接</a>"
          . nil)
         ("---\ndouban:\n  status: {}\n---\n\n正文：@张"
          . t)
         ("---\ndouban:\n  status: {}\n---\n\n(@张"
          . t)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/status.md")
      (insert (car case))
      (goto-char (point-min))
      (search-forward "@张")
      (let ((bounds
             (douban--markdown-user-mention-bounds)))
        (if (cdr case)
            (progn
              (should bounds)
              (should
               (equal
                (buffer-substring
                 (car bounds) (cdr bounds))
                "@张")))
          (should-not bounds))))))

(ert-deftest douban-test-user-mention-capf-cycles-and-edits-source-buffer ()
  (let* ((first
          '(:id "42"
            :name "张三"
            :url "https://www.douban.com/people/first/"))
         (second
          '(:id "43"
            :name "张四"
            :url "https://www.douban.com/people/second/"))
         (source (generate-new-buffer " *douban-capf-source*")))
    (unwind-protect
        (with-current-buffer source
          (setq buffer-file-name "/tmp/status.md")
          (insert
           "---\ndouban:\n  status: {}\n---\n\n正文 @张")
          (cl-letf
              (((symbol-function 'douban--search-users)
                (lambda (_query) (list first second))))
            (let* ((capf
                    (douban-user-mention-completion-at-point))
                   (start (nth 0 capf))
                   (end (nth 1 capf))
                   (properties (nthcdr 3 capf))
                   (exit-function
                    (plist-get properties :exit-function))
                   (first-label "张三 (42)")
                   (second-label "张四 (43)"))
              (delete-region start end)
              (goto-char start)
              (insert first-label)
              ;; `sole' means cycling can continue, not final selection.
              (funcall exit-function first-label 'sole)
              (should
               (string-suffix-p
                (concat "正文 @" first-label)
                (buffer-string)))
              (delete-region start (point-max))
              (goto-char start)
              (insert second-label)
              ;; Some completion frontends invoke the callback elsewhere.
              (with-temp-buffer
                (funcall exit-function second-label 'finished))
              (should
               (string-suffix-p
                (concat
                 "正文 "
                 "<a href=\"https://www.douban.com/people/second/\" "
                 "title=\"douban-user-mention:43\">"
                 "&#x40;&#x5F20;&#x56DB;</a>")
                (buffer-string))))))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest douban-test-user-mention-capf-is-exclusive-without-candidates ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/status.md")
    (insert "---\ndouban:\n  status: {}\n---\n\n正文 @无人")
    (cl-letf
        (((symbol-function 'douban--search-users)
          (lambda (_query) nil)))
      (let ((capf
             (douban-user-mention-completion-at-point)))
        (should capf)
        (should-not (nth 2 capf))
        (should (eq (plist-get (nthcdr 3 capf) :exclusive) t))))))

(ert-deftest douban-test-douban-mode-manages-markdown-editing-state ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/status.md")
    (setq major-mode 'markdown-mode)
    ;; 手动启用不要求 metadata 已经完整；否则最需要补全的草稿反而无法
    ;; 使用本 mode。
    (insert "---\ndouban:\n  status:\n---\n\n正文")
    (douban-mode 1)
    (should douban-mode)
    (should
     (memq
      #'douban-metadata-completion-at-point
      completion-at-point-functions))
    (should
     (memq
      #'douban-user-mention-completion-at-point
      completion-at-point-functions))
    (should
     (memq
      #'douban--reset-completion-caches
      after-revert-hook))
    (setq
     douban--anthology-completion-cache
     '((:id "1" :title "文集"))
     douban--subject-completion-cache
     '(:subject-type "book" :query "局外人" :subjects ())
     douban--platform-completion-cache
     '(:subject-id "2" :platforms ())
     douban--user-mention-completion-cache
     '(("张" (:id "3" :name "张三"))))
    (run-hooks 'after-revert-hook)
    (should
     (eq
      douban--anthology-completion-cache
      douban--anthology-completion-cache-unloaded))
    (should-not douban--subject-completion-cache)
    (should-not douban--platform-completion-cache)
    (should-not douban--user-mention-completion-cache)
    (setq
     douban--anthology-completion-cache
     '((:id "1" :title "文集"))
     douban--subject-completion-cache
     '(:subject-type "book" :query "局外人" :subjects ())
     douban--platform-completion-cache
     '(:subject-id "2" :platforms ())
     douban--user-mention-completion-cache
     '(("张" (:id "3" :name "张三"))))
    (douban-mode -1)
    (should-not douban-mode)
    (should-not
     (memq
      #'douban-metadata-completion-at-point
      completion-at-point-functions))
    (should-not
     (memq
      #'douban-user-mention-completion-at-point
      completion-at-point-functions))
    (should
     (eq
      douban--anthology-completion-cache
      douban--anthology-completion-cache-unloaded))
    (should-not douban--subject-completion-cache)
    (should-not douban--platform-completion-cache)
    (should-not douban--user-mention-completion-cache)
    (dolist
        (variable
         '(douban--anthology-completion-cache
           douban--subject-completion-cache
           douban--platform-completion-cache
           douban--user-mention-completion-cache))
      (should-not (local-variable-p variable)))
    ;; 关闭后 revert hook 也必须卸载；重新赋值后运行 hook 不应再清缓存。
    (setq
     douban--anthology-completion-cache 'anthology-after-disable
     douban--subject-completion-cache 'subject-after-disable
     douban--platform-completion-cache 'platform-after-disable
     douban--user-mention-completion-cache 'mention-after-disable)
    (run-hooks 'after-revert-hook)
    (should
     (eq
      douban--anthology-completion-cache
      'anthology-after-disable))
    (should
     (eq
      douban--subject-completion-cache
      'subject-after-disable))
    (should
     (eq
      douban--platform-completion-cache
      'platform-after-disable))
      (should
       (eq
        douban--user-mention-completion-cache
        'mention-after-disable))))

(ert-deftest douban-test-douban-mode-requires-matching-source-mode-and-file ()
  (dolist
      (case
       '(("/tmp/status.md" fundamental-mode)
         ("/tmp/status.org" markdown-mode)
         ("/tmp/status.md" org-mode)
         ("/tmp/status.txt" text-mode)
         (nil markdown-mode)))
    (with-temp-buffer
      (setq buffer-file-name (car case))
      (setq major-mode (cadr case))
      (should-error (douban-mode 1) :type 'user-error)
      (should-not douban-mode)
      (should-not
       (memq
        #'douban-metadata-completion-at-point
        completion-at-point-functions))
      (should-not
       (memq
        #'douban-user-mention-completion-at-point
        completion-at-point-functions)))))

(ert-deftest douban-test-user-mention-source-roundtrips-through-pandoc ()
  (skip-unless (executable-find "pandoc"))
  (dolist (name '("张三" "*星*" "`星`" "a &copy; b" "<b>" "张😀"))
    (let ((user
           (list
            :id "42"
            :name name
            :url "https://www.douban.com/people/example/")))
      (let* ((source (douban--user-mention-source user))
             (html
              (douban--pandoc-to-html "markdown+mark" source))
               (raw (douban--html-to-draft html))
               (entity (douban-test--first-draft-entity raw)))
          (should
           (equal
            (plist-get (aref (plist-get raw :blocks) 0) :text)
            (concat "@" name)))
          (should (equal (plist-get entity :type) "USER"))
          (should
           (equal
            (plist-get (plist-get entity :data) :name)
            name))
          (should
           (equal
            (plist-get (plist-get entity :data) :id)
            "42"))))))

(ert-deftest douban-test-review-publish-preserves-user-mention ()
  (let ((html
         (concat
          "<p>"
          (make-string 140 ?文)
          "<a href=\"https://www.douban.com/people/example/\" "
          "title=\"douban-user-mention:42\">@张三</a></p>"))
        submitted-raw)
    (cl-letf
        (((symbol-function 'douban--source-html)
          (lambda (_file) html))
         ((symbol-function 'douban--review-direct-session)
          (lambda (_meta)
            (douban--make-session :kind 'review)))
         ((symbol-function 'douban--submit-review)
          (lambda (_meta raw _session _title)
            (setq submitted-raw raw)
            '(:id "7"
              :url "https://book.douban.com/review/7/"))))
      (should
       (equal
        (douban--publish-review-file
         "/tmp/review.md"
         (douban-test--review-meta
          '(:subject-id "1"
            :id "7")
          "长评"))
        "7"))
      (should
       (douban--draft-has-entity-type-p submitted-raw "USER")))))

(ert-deftest douban-test-note-publish-preserves-user-mention ()
  (let ((html
         (concat
          "<p><a href=\"https://www.douban.com/people/example/\" "
          "title=\"douban-user-mention:42\">@张三</a></p>"))
        submitted-raw)
    (cl-letf
        (((symbol-function 'douban--source-html)
          (lambda (_file) html))
         ((symbol-function 'douban--note-session)
          (lambda (_meta)
            (douban--make-session
             :kind 'note
             :state
             '(:note-id "7"
               :action "page-update-action"))))
         ((symbol-function 'douban--note-privacy-value)
          (lambda (&rest _arguments) "public"))
         ((symbol-function 'douban--rewrite-draft-images)
          (lambda (raw &rest _arguments) raw))
         ((symbol-function 'douban--submit-note)
          (lambda (_meta raw _session _title _privacy)
            (setq submitted-raw raw)
            '(:id "7"
              :url "https://www.douban.com/note/7/"))))
      (should
       (equal
        (douban--publish-note-file
         "/tmp/note.md"
         (douban--meta-from-plist
          '(:note
            (:id "7"))
          "日记"))
        "7"))
      (should
       (douban--draft-has-entity-type-p submitted-raw "USER")))))

(ert-deftest douban-test-html-subject-url-starts-as-link-before-resolution ()
  (let* ((url "https://book.douban.com/subject/4908885/")
         (raw
          (douban--html-to-draft
           (format "<p><a href=\"%s\">局外人</a></p>" url)))
         (block (aref (plist-get raw :blocks) 0))
         (entity (douban-test--first-draft-entity raw)))
    (should (equal (plist-get block :type) "unstyled"))
    (should (equal (plist-get block :text) "局外人"))
    (should (equal (plist-get entity :type) "LINK"))
    (should (equal (plist-get entity :mutability) "MUTABLE"))
    (should
     (equal
      (plist-get (plist-get entity :data) :url)
      url))))

(ert-deftest douban-test-cc-statement-is-optional ()
  (let ((douban-cc-statement nil)
        (html "<p>正文 &amp; 引文</p>"))
    (should (eq (douban--append-cc-statement html) html))))

(ert-deftest douban-test-cc-statement-supports-cc0 ()
  (let ((douban-cc-statement 'cc0))
    (should
     (equal
      (douban--append-cc-statement "<p>正文</p>")
      (concat
       "<p>正文</p>\n"
       "<blockquote><p>除另有声明外，本文中的原创内容已通过 "
       "<a href=\"https://creativecommons.org/publicdomain/zero/"
       "1.0/deed.zh-hans\" rel=\"license\">CC0 1.0 通用</a>"
       "，在法律允许的范围内贡献至公共领域。</p></blockquote>")))))

(ert-deftest douban-test-cc-statement-supports-all-six-licenses ()
  (dolist
      (case
       '((by "by" "署名")
         (by-sa "by-sa" "署名—相同方式共享")
         (by-nd "by-nd" "署名—禁止演绎")
         (by-nc "by-nc" "署名—非商业性使用")
         (by-nc-sa "by-nc-sa" "署名—非商业性使用—相同方式共享")
         (by-nc-nd "by-nc-nd" "署名—非商业性使用—禁止演绎")))
    (let* ((douban-cc-statement (nth 0 case))
           (slug (nth 1 case))
           (name (nth 2 case)))
      (should
       (equal
        (douban--append-cc-statement
         "<p>正文 &amp; 引文</p>")
        (format
         (concat
          "<p>正文 &amp; 引文</p>\n"
          "<blockquote><p>除另有声明外，本文中的原创内容采用 "
          "<a href=\"https://creativecommons.org/licenses/%s/4.0/"
          "deed.zh-hans\" rel=\"license\">CC %s 4.0"
          "（%s 4.0 协议国际版）</a> 许可。</p></blockquote>")
         slug (upcase slug) name))))))

(ert-deftest douban-test-cc-statement-rejects-unknown-license ()
  (let ((douban-cc-statement 'unknown))
    (should-error
     (douban--append-cc-statement "<p>正文</p>")
     :type 'error)))

(ert-deftest douban-test-prepare-draft-applies-cc-to-long-form-only ()
  (let ((douban-cc-statement 'by)
        (source-html "<p>正文</p>")
        (validations 0))
    (cl-letf
        (((symbol-function 'douban--source-html)
          (lambda (_file) source-html)))
      (dolist (kind '(review note status))
        (pcase-let*
            ((`(,raw ,count ,base-directory)
              (douban--prepare-draft
               "/tmp/source.md"
               kind
               (lambda (source-raw)
                 (cl-incf validations)
                 (should
                  (= (length (plist-get source-raw :blocks)) 1))
                 (should
                  (equal
                   (plist-get
                    (aref (plist-get source-raw :blocks) 0)
                    :text)
                   "正文"))
                 2)))
             (blocks (plist-get raw :blocks)))
          (should (= count 2))
          (should (equal base-directory "/tmp/"))
          (if (memq kind '(review note))
              (progn
                (should (= (length blocks) 2))
                (should
                 (equal
                  (plist-get (aref blocks 1) :type)
                  "blockquote"))
                (let ((entity
                       (douban-test--first-draft-entity raw)))
                  (should (equal (plist-get entity :type) "LINK"))
                  (should
                   (equal
                    (plist-get entity :mutability)
                    "MUTABLE"))
                  (should
                   (equal
                    (plist-get (plist-get entity :data) :url)
                    (concat
                     "https://creativecommons.org/licenses/by/4.0/"
                     "deed.zh-hans")))))
            (progn
              (should (= (length blocks) 1))
              (should-not
               (douban-test--first-draft-entity raw)))))))
    (should (= validations 3))))

(ert-deftest douban-test-cc-statement-cannot-satisfy-source-validation ()
  (let ((douban-cc-statement 'by))
    (cl-letf
        (((symbol-function 'douban--source-html)
          (lambda (_file) "<p></p>")))
      (should-error
       (douban--prepare-draft
        "/tmp/note.md"
        'note
        (lambda (raw)
          (douban--validate-content-draft raw "日记")))
       :type 'user-error))
    (cl-letf
        (((symbol-function 'douban--source-html)
          (lambda (_file)
            (format
             "<p>%s</p>"
             (make-string
              (1- douban-minimum-review-length)
              ?字)))))
      (should-error
       (douban--prepare-draft
        "/tmp/review.md"
        'review
        #'douban--validate-draft)
       :type 'user-error))))



(ert-deftest douban-test-draft-entity-occurrences-ignore-orphans ()
  (let* ((referenced
          '(:type "USER" :mutability "IMMUTABLE"
            :data (:id "1")))
         (orphan-link
          '(:type "LINK" :mutability "IMMUTABLE"
            :data
            (:url "https://example.org/orphan"
             :display "atomic")))
         (orphan-image
          '(:type "IMAGE" :mutability "IMMUTABLE"
            :data
            (:src "https://example.org/orphan.png"
             :caption "")))
         (raw
          (douban-test--entity-raw
           `(("9" . ,orphan-image)
             ("0" . ,referenced)
             ("8" . ,orphan-link))
           '("0" "0")))
         (occurrences
          (douban--draft-entity-occurrences raw)))
    (should
     (equal
      (mapcar
       (lambda (occurrence)
         (plist-get occurrence :key))
       occurrences)
      '("0" "0")))
    (should
     (equal
      (douban--draft-referenced-entities raw)
      (list referenced)))
    (should
     (douban--draft-has-entity-type-p raw "USER"))
    (should-not
     (douban--draft-has-entity-type-p raw "LINK"))
    (should-not (douban--draft-has-image-p raw))
    (should (equal (douban--topic-image-ids raw) ""))
    (cl-letf
        (((symbol-function 'douban--resolve-card)
          (lambda (&rest _arguments)
            (ert-fail "孤立卡片不得发起解析请求")))
         ((symbol-function 'douban--upload-image-url)
          (lambda (&rest _arguments)
            (ert-fail "孤立图片不得发起上传请求"))))
      (should (eq (douban--rewrite-draft-cards raw) raw))
      (should
       (eq
        (douban--rewrite-draft-images
         raw (douban--make-session :kind 'review)
         default-directory)
        raw)))))

(ert-deftest douban-test-draft-entity-missing-key-fails-before-effects ()
  (let ((raw
         (douban-test--entity-raw
          '(("0" .
             (:type "IMAGE" :mutability "IMMUTABLE"
              :data
              (:src "https://example.org/zero.png"
               :caption ""))))
          '("404")))
        effects)
    (cl-letf
        (((symbol-function 'douban--resolve-card)
          (lambda (&rest _arguments)
            (push 'card effects)))
         ((symbol-function 'douban--upload-image-url)
          (lambda (&rest _arguments)
            (push 'image effects))))
      (dolist
          (form
           (list
            (lambda ()
              (douban--draft-entity-occurrences raw))
            (lambda ()
              (douban--draft-referenced-entities raw))
            (lambda ()
              (douban--draft-has-image-p raw))
            (lambda ()
              (douban--validate-content-draft raw "正文"))
            (lambda ()
              (douban--rewrite-draft-cards raw))
            (lambda ()
              (douban--rewrite-draft-images
               raw (douban--make-session :kind 'review)
               default-directory))
            (lambda ()
              (douban--topic-image-ids raw))))
        (should-error (funcall form) :type 'error)))
    (should-not effects)))






(ert-deftest douban-test-empty-html-still-has-one-block ()
  (let* ((raw (douban--html-to-draft ""))
         (blocks (plist-get raw :blocks)))
    (should (= (length blocks) 1))
    (should (equal (plist-get (aref blocks 0) :type) "unstyled"))))

(ert-deftest douban-test-html-layout-whitespace-is-not-content ()
  (let* ((raw
          (douban--html-to-draft
           "\n  <h2>标题</h2>\n\n<p>正文</p>\r\n"))
         (blocks (plist-get raw :blocks)))
    (should (= (length blocks) 2))
    (should
     (equal
      (mapcar
       (lambda (block) (plist-get block :text))
       (append blocks nil))
      '("标题" "正文")))
    (should (= (douban--draft-character-count raw) 4))))

(ert-deftest douban-test-draft-block-keys-remain-unique ()
  (let* ((html
          (apply #'concat (make-list 1000 "<p>x</p>")))
         (blocks
          (plist-get (douban--html-to-draft html) :blocks))
         (seen (make-hash-table :test 'equal)))
    (should (= (length blocks) 1000))
    (cl-loop
     for block across blocks
     for key = (plist-get block :key)
     do
     (should-not (gethash key seen))
     (puthash key t seen))))

(ert-deftest douban-test-draft-minimum-length ()
  (let ((douban-minimum-review-length 5))
    (should-error
     (douban--validate-draft
      (douban--html-to-draft "<p>一 二</p>"))
     :type 'user-error)
    (should
     (= (douban--validate-draft
         (douban--html-to-draft "<p>一二三四五</p>"))
        5))))

(ert-deftest douban-test-dom-input-helpers-share-name-filtering ()
  (let* ((document
          (douban--parse-html
           (concat
            "<html><body>"
            "<form id=\"target\">"
            "<input name=\"other\" value=\"decoy\">"
            "<input name=\"field\" value=\"\">"
            "<input name=\"field\" value=\"first\">"
            "<input type=\"radio\" name=\"choice\" value=\"\" checked>"
            "<input type=\"radio\" name=\"choice\" value=\"R\">"
            "<input type=\"radio\" name=\"choice\" value=\"G\" checked>"
            "<input type=\"hidden\" name=\"choice\" value=\"hidden\">"
            "<input type=\"radio\" name=\"fallback\" value=\"R\">"
            "<input type=\"hidden\" name=\"fallback\" value=\"\">"
            "<input type=\"hidden\" name=\"fallback\" value=\"hidden\">"
            "<input name=\"values\" value=\"a\">"
            "<input name=\"values\" value=\"\">"
            "<input name=\"values\" value=\"a\">"
            "<input name=\"values\" value=\"b\">"
            "</form>"
            "<form><input name=\"other\" value=\"ignored\"></form>"
            "</body></html>")))
         (form (douban--dom-form-with-input document "field"))
         (inputs (douban--dom-inputs form "values")))
    (should (equal (dom-attr form 'id) "target"))
    (should (= (length inputs) 4))
    (should
     (equal
      (mapcar
       (lambda (input)
         (dom-attr input 'value))
       inputs)
      '("a" "" "a" "b")))
    (should (equal (douban--dom-input-value form "field") "first"))
    (should
     (equal
      (douban--dom-input-choice-value form "choice")
      "G"))
    (should
     (equal
      (douban--dom-input-choice-value form "fallback")
      "hidden"))))

(ert-deftest douban-test-javascript-variable-enforces-type ()
  (let ((html
         (concat
          "<script>\n"
          "_APP_NAME = \"book\";\n"
          "_REVIEW_ID = 456;\n"
          "</script>")))
    (should
     (equal
      (douban--javascript-variable
       html "_APP_NAME" 'string)
      "book"))
    (should
     (equal
      (douban--javascript-variable
       html "_REVIEW_ID" 'id)
      "456"))
    (should-not
     (douban--javascript-variable
      html "_APP_NAME" 'id))
    (should-not
     (douban--javascript-variable
      html "_REVIEW_ID" 'string))
    (should-not
     (douban--javascript-variable
      html "_MISSING" 'string))
    (should-error
     (douban--javascript-variable
      html "_APP_NAME" 'boolean)
     :type 'error)))

(ert-deftest douban-test-review-editor-session-binds-create-page ()
  (let* ((meta
          (douban-test--review-meta
           (list :subject-id "123")
           "标题"))
         (url "https://www.douban.com/subject/123/new_review")
         (cookies '(("dbcl2" . "login-cookie"))))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (request-url)
            (should (equal request-url url))
            cookies))
         ((symbol-function 'douban--http)
          (lambda (method request-url &rest arguments)
            (should (equal method "GET"))
            (should (equal request-url url))
            (should
             (equal
              (douban--session-cookies
               (plist-get arguments :session))
              cookies))
            (list
             :status 200
             :body
             (douban-test--review-editor-html "123" "book")))))
      (let ((session (douban--review-editor-session meta)))
        (should (eq (douban--session-kind session) 'review))
        (should (equal (douban--session-ck session) "page-ck"))
        (should
         (equal
          (douban--session-state-get session :app-name)
          "book"))
        (should-not
         (douban--session-state-get session :review-id))
        (should (equal (douban--session-referer session) url))
        (should (equal (douban--session-host session) "www.douban.com"))
        (should
         (equal
          (douban--session-state-get
           session :upload-field)
          "upload_auth_token"))
        (should
         (equal
          (cdr
           (assoc "ck" (douban--session-cookies session)))
          "page-ck"))))))

(ert-deftest douban-test-review-editor-session-binds-edit-page ()
  (let* ((meta
          (douban-test--review-meta
           (list
            :subject-id "123"
            :id "456")
           "标题"))
         (url "https://book.douban.com/review/456/edit"))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (request-url)
            (should (equal request-url url))
            '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (method request-url &rest _arguments)
            (should (equal method "GET"))
            (should (equal request-url url))
            (list
             :status 200
             :body
             (douban-test--review-editor-html
              "123" "book" "456")))))
      (let ((session (douban--review-editor-session meta)))
        (should
         (equal
          (douban--session-state-get session :review-id)
          "456"))
        (should (equal (douban--session-referer session) url))
        (should
         (equal (douban--session-host session) "book.douban.com"))))))

(ert-deftest douban-test-review-editor-session-rejects-operation-id-mismatch ()
  (dolist
      (case
       (list
        (list
         (douban-test--review-meta
          (list :subject-id "123")
          "标题")
         (douban-test--review-editor-html
          "123" "book" "456"))
        (list
         (douban-test--review-meta
          (list
           :subject-id "123"
           :id "456")
          "标题")
         (douban-test--review-editor-html
          "123" "book" "789"))))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (_url) '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (list :status 200 :body (cadr case)))))
      (should-error
       (douban--review-editor-session (car case))
       :type 'error))))

(ert-deftest douban-test-review-editor-session-binds-subject-and-derived-app ()
  (let ((book-meta
         (douban-test--review-meta
          (list :subject-id "123")
          "标题")))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (_url) '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (list
             :status 200
             :body
             (douban-test--review-editor-html
              "999" "book")))))
      (should-error
       (douban--review-editor-session book-meta)
       :type 'user-error))
    (dolist
        (html
         (list
          (douban-test--review-editor-html "123" "movie")
          (replace-regexp-in-string
           "_APP_NAME = '[^']*';\n" ""
           (douban-test--review-editor-html "123" "book"))))
      (cl-letf
          (((symbol-function 'douban--read-browser-cookies)
            (lambda (_url) '(("dbcl2" . "login-cookie"))))
           ((symbol-function 'douban--http)
            (lambda (&rest _arguments)
              (list :status 200 :body html))))
        (should
         (equal
          (douban--session-state-get
           (douban--review-editor-session book-meta)
           :app-name)
          "book")))))
  (let ((tv-meta
         (douban-test--review-meta
          (list
           :subject-id "123"
           :subject-type "tv")
          "标题")))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (_url) '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (list
             :status 200
             :body
             (douban-test--review-editor-html
              "123" "book")))))
      (should
       (equal
        (douban--session-state-get
         (douban--review-editor-session tv-meta)
         :app-name)
        "movie")))))

(ert-deftest douban-test-review-editor-session-scopes-fields-to-review-form ()
  (let* ((meta
          (douban-test--review-meta
           (list :subject-id "123")
           "标题"))
         (decoy
          (concat
           "<form id=\"unrelated-form\">"
           "<input name=\"ck\" value=\"decoy-ck\">"
           "<input name=\"review[subject_id]\" value=\"999\">"
           "</form>"))
         (html
          (douban-test--review-editor-html
           "123" "book" nil nil decoy)))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (_url) '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (list :status 200 :body html))))
      (let ((session (douban--review-editor-session meta)))
        (should (equal (douban--session-ck session) "page-ck"))))))

(ert-deftest douban-test-review-editor-session-rejects-login-and-http-errors ()
  (let ((meta
         (douban-test--review-meta
          (list :subject-id "123")
          "标题")))
    (dolist (status '(401 403))
      (cl-letf
          (((symbol-function 'douban--read-browser-cookies)
            (lambda (_url) '(("dbcl2" . "login-cookie"))))
           ((symbol-function 'douban--http)
            (lambda (&rest _arguments)
              (list :status status :body "login required"))))
        (should-error
         (douban--review-editor-session meta)
         :type 'user-error)))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (_url) '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            '(:status 500 :body "server error"))))
      (should-error
       (douban--review-editor-session meta)
       :type 'error))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (_url) '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            '(:status 200
              :body "<html><title>login</title></html>"))))
      (should-error
       (douban--review-editor-session meta)
       :type 'user-error))))

(ert-deftest douban-test-review-editor-session-parses-game-rtype-default ()
  (dolist (expected '("R" "G"))
    (let ((meta
           (douban-test--review-meta
            (list
             :subject-id "123"
             :subject-type "game")
            "标题")))
      (cl-letf
          (((symbol-function 'douban--read-browser-cookies)
            (lambda (_url) '(("dbcl2" . "login-cookie"))))
           ((symbol-function 'douban--http)
            (lambda (&rest _arguments)
              (list
               :status 200
               :body
               (douban-test--review-editor-html
                "123" "game" nil expected)))))
        (should
         (equal
          (douban--session-state-get
           (douban--review-editor-session meta)
           :rtype)
          expected))))))

(ert-deftest douban-test-http-only-allows-https-douban ()
  (should
   (douban--https-douban-url-p
    "https://book.douban.com/review/1/edit"))
  (should-not
   (douban--https-douban-url-p
    "http://book.douban.com/review/1/edit"))
  (should-not
   (douban--https-douban-url-p
    "https://douban.com.example.org/review/1/edit"))
  (should-not
   (douban--https-douban-url-p
    "https://www.douban.com:8443/review/1/edit"))
  (should-not
   (douban--https-douban-url-p
    "https://user@www.douban.com/review/1/edit"))
  (let (called)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (&rest _arguments)
            (setq called t)
            (ert-fail "非 HTTPS 豆瓣 URL 不应进入传输层"))))
      (should-error
       (douban--http "GET" "http://www.douban.com/")
       :type 'error))
    (should-not called)))

(ert-deftest douban-test-plz-request-disables-redirects-and-keeps-response ()
  (let* ((response
          (make-plz-response
           :version 2
           :status douban--plz-filter-redirect-status
           :headers '(("location" . "/people/alice/"))
           :body "not followed"))
         (plz-curl-default-args
          '("--silent" "--location" "-L" "--location-trusted"
            "--compressed" "--disable"))
         method url options curl-arguments filtered-output)
    (cl-letf
        (((symbol-function 'plz)
          (lambda (request-method request-url &rest request-options)
            (setq
             method request-method
             url request-url
             options request-options
             curl-arguments (copy-sequence plz-curl-default-args))
            (let* ((buffer (generate-new-buffer " *douban-filter-test*"))
                   (process
                    (make-pipe-process
                     :name "douban-filter-test"
                     :buffer buffer
                     :noquery t))
                   (filter (plist-get request-options :filter)))
              (unwind-protect
                  (progn
                    ;; 同时覆盖代理响应以及横跨两个 chunk 的状态行。
                    (funcall
                     filter process
                     (concat
                      "HTTP/1.1 200 Connection established\r\n\r\n"
                      "HTTP/2 30"))
                    (funcall
                     filter process
                     (concat
                      "2 \r\nlocation: /people/alice/\r\n"
                      "content-length: 12\r\n\r\nnot followed"))
                    (setq
                     filtered-output
                     (with-current-buffer buffer (buffer-string))))
                (when (process-live-p process)
                  (delete-process process))
                (kill-buffer buffer)))
            (signal
             'plz-http-error
             (list
              "HTTP error"
              (make-plz-error :response response))))))
      (should
       (let ((actual
              (douban--plz-request
               "GET" douban--ck-bootstrap-url
               :headers '(("Accept" . "text/html")))))
         (and
          (= (plz-response-status actual) 302)
          (= (plz-response-version actual) 2)
          (equal (plz-response-headers actual)
                 '(("location" . "/people/alice/")))
          (equal (plz-response-body actual) "not followed")))))
    (should (eq method 'get))
    (should (equal url douban--ck-bootstrap-url))
    (should
     (equal curl-arguments
            '("--disable" "--silent" "--compressed")))
    (should
     (equal
      filtered-output
      (concat
       "HTTP/1.1 200 Connection established\r\n\r\n"
       "HTTP/2 599 \r\nlocation: /people/alice/\r\n"
       "content-length: 12\r\n\r\nnot followed")))
    (should
     (equal
      (plist-get options :headers)
      '(("Accept" . "text/html"))))
    (should (eq (plist-get options :body-type) 'binary))
    (should (eq (plist-get options :as) 'response))
    (should (eq (plist-get options :decode) t))
    (should (eq (plist-get options :then) 'sync))
    (should (functionp (plist-get options :filter)))
    (should
     (= (plist-get options :connect-timeout)
        douban--request-timeout))
    (should
     (= (plist-get options :timeout)
        douban--request-timeout))))

(ert-deftest douban-test-http-response-filter-preserves-nonredirects ()
  (should-not
   (douban--filtered-http-response-prefix
    "HTTP/2 20"))
  (let ((response
         (concat
          "HTTP/1.1 100 Continue\r\n\r\n"
          "HTTP/2 403 Forbidden\r\ncontent-type: text/plain\r\n\r\n"
          "denied")))
    (should
     (equal
      (douban--filtered-http-response-prefix response)
      (list response nil)))))

(ert-deftest douban-test-http-rejects-redirect-and-encodes-utf8-json ()
  (let ((text "{\"text\":\"换行\\n😀\"}")
        captured)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method url &rest options)
            (setq captured (list method url options))
            (make-plz-response
             :status 302
             :headers '(("Location" . "https://evil.example/"))
             :body ""))))
      (should-error
       (douban--http
        "POST" "https://www.douban.com/subject/1/new_review"
        :body text
        :content-type "application/json;charset=utf-8")
       :type 'error))
    (pcase-let ((`(,method ,url ,options) captured))
      (should (equal method "POST"))
      (should
       (equal
        url "https://www.douban.com/subject/1/new_review"))
      (let ((body (plist-get options :body))
            (headers (plist-get options :headers)))
        (should
         (equal body (encode-coding-string text 'utf-8 t)))
        (should-not (multibyte-string-p body))
        (should
         (equal
          (cdr (assoc-string "Content-Type" headers t))
          "application/json;charset=utf-8"))))))

(ert-deftest douban-test-review-direct-session-uses-cookie-without-get ()
  (let ((meta
         (douban-test--review-meta
          (list
           :subject-id "123"
           :subject-type "book")
          "标题")))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (should
             (equal url "https://book.douban.com/subject/123/"))
            '(("dbcl2" . "login")
              ("ck" . "abcd"))))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (ert-fail "text-only direct session must not issue GET"))))
      (let ((session (douban--review-direct-session meta)))
        (should (eq (douban--session-kind session) 'review))
        (should (equal (douban--session-ck session) "abcd"))
        (should
         (equal
          (douban--session-state-get session :app-name)
          "book"))
        (should
         (equal
          (douban--session-referer session)
          "https://book.douban.com/subject/123/"))))))

(ert-deftest douban-test-review-image-uses-page-session ()
  (let* ((meta
          (douban-test--review-meta
           (list
            :subject-id "123"
            :subject-type "book"
            :id "456")
           "标题"))
         (session
          (douban--make-session
           :ck "abcd"
           :host "book.douban.com"
           :state '(:app-name "book" :review-id "456")
           :referer
           "https://book.douban.com/review/456/edit"))
         (page-session-called nil))
    (cl-letf
        (((symbol-function 'douban--source-html)
          (lambda (_file)
            (concat
             "<p>" (douban-test--long-text) "</p>"
             "<p><img src=\"images/example.webp\"></p>")))
         ((symbol-function 'douban--review-direct-session)
          (lambda (_meta)
            (ert-fail "review containing IMAGE must not use direct session")))
         ((symbol-function 'douban--review-editor-session)
          (lambda (_meta)
            (setq page-session-called t)
            session))
         ((symbol-function 'douban--rewrite-draft-images)
          (lambda (&rest _arguments) nil))
         ((symbol-function 'douban--submit-review)
          (lambda (_meta _raw _session _title)
            '(:id "456"
                  :url "https://book.douban.com/review/456/")))
         ((symbol-function 'douban--checkpoint-published-content)
          (lambda (&rest _arguments)
            (ert-fail "评论更新不应写入创建检查点")))
         ((symbol-function 'douban--remove-created-review-broadcast)
          (lambda (&rest _arguments)
            (ert-fail "评论更新不应删除历史广播"))))
      (should
       (equal
        (douban--publish-review-file "/tmp/review-with-image.md" meta)
        "456"))
      (should page-session-called))))

(ert-deftest douban-test-review-form-matches-current-editor-contract ()
  (let* ((meta
          (douban-test--review-meta
           (list
            :subject-id "123"
            :rating "5"
            :spoiler "true"
            :donate "false"
            :explanation-types "ai-generated")
           "标题"))
         (raw (douban--html-to-draft "<p>正文</p>"))
         (session
          (douban--make-session
           :ck "abcd"
           :state '(:app-name "book")))
         (fields
          (douban--review-form-fields meta raw session "标题")))
    (should (equal (cdr (assoc "is_rich" fields)) "1"))
    (should
     (equal (cdr (assoc "review[subject_id]" fields)) "123"))
    (should
     (equal (cdr (assoc "review[spoiler]" fields)) "on"))
    (should
     (equal (cdr (assoc "review[original]" fields)) "on"))
    (should
     (equal (cdr (assoc "review[donate]" fields)) ""))
    (should
     (equal
      (cdr (assoc "review[explanation_types]" fields))
      "A"))
    (should
     (equal (cdr (assoc "ck" fields)) "abcd"))
    (should-not (assoc "review[rtype]" fields))
    (should
     (plist-get
      (json-parse-string
       (cdr (assoc "review[text]" fields))
       :object-type 'plist)
      :blocks))))

(ert-deftest douban-test-review-form-original-follows-default ()
  (let ((raw (douban--html-to-draft "<p>正文</p>"))
        (session
         (douban--make-session
          :ck "abcd"
          :state '(:app-name "book"))))
    (dolist (review-id '(nil "456"))
      (dolist (original '(t nil))
        (let* ((douban-default-original original)
               (meta
                (douban-test--review-meta
                 (append
                  '(:subject-id "123")
                  (and review-id (list :id review-id)))
                 "标题"))
               (fields
                (douban--review-form-fields
                 meta raw session "标题")))
          (should
           (equal
            (cdr (assoc "review[original]" fields))
            (if original "on" ""))))))))

(ert-deftest douban-test-game-review-form-uses-page-rtype-default ()
  (let ((raw (douban--html-to-draft "<p>正文</p>")))
    (dolist (page-rtype '("R" "G"))
      (let* ((meta
              (douban-test--review-meta
               (list
                :subject-id "123"
                :subject-type "game")
               "标题"))
             (session
              (douban--make-session
               :ck "abcd"
               :state
               (list
                :app-name "game"
                :rtype page-rtype)))
             (fields
              (douban--review-form-fields
               meta raw session "标题")))
        (should
         (equal
          (cdr (assoc "review[rtype]" fields))
          page-rtype))))))

(ert-deftest douban-test-game-review-form-metadata-overrides-page-rtype ()
  (let* ((meta
          (douban-test--review-meta
           (list
            :subject-id "123"
            :subject-type "game"
            :rtype "guide")
           "标题"))
         (session
          (douban--make-session
           :ck "abcd"
           :state '(:app-name "game" :rtype "R")))
         (fields
          (douban--review-form-fields
           meta
           (douban--html-to-draft "<p>正文</p>")
           session "标题")))
    (should
     (equal
      (cdr (assoc "review[rtype]" fields))
      "G"))))

(ert-deftest douban-test-game-review-direct-session-requires-metadata-rtype ()
  (let* ((meta
          (douban-test--review-meta
          (list
           :subject-id "123"
           :subject-type "game")
          "标题"))
         (raw (douban--html-to-draft "<p>纯文本正文</p>")))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (should
             (equal url "https://www.douban.com/game/123/"))
            '(("ck" . "abcd"))))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (ert-fail "direct game review must not issue GET"))))
      (let ((session (douban--review-direct-session meta)))
        (should-error
         (douban--review-form-fields meta raw session "标题")
         :type 'user-error)))))

(ert-deftest douban-test-non-game-review-metadata-rejects-game-fields ()
  (dolist (rtype '("review" "guide"))
    (should-error
     (douban-test--review-meta
      (list
       :subject-id "123"
       :rtype rtype)
      "标题")
     :type 'error))
  (should-error
   (douban-test--review-meta
    (list
     :subject-id "123"
     :platforms '("1"))
    "标题")
   :type 'error))

(ert-deftest douban-test-submit-review-create-endpoint-and-result ()
  (let* ((meta
          (douban-test--review-meta
           (list :subject-id "123")
           "标题"))
         (raw (douban--html-to-draft "<p>正文</p>"))
         (session
          (douban--make-session
           :kind 'review
           :ck "abcd"
           :host "www.douban.com"
           :state '(:app-name "book")
           :referer
           "https://www.douban.com/subject/123/new_review"))
         captured)
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (setq captured (list method url arguments))
            (list
             :status 200
             :body
             "{\"url\":\"https://book.douban.com/review/789/\"}"
             :json
             '(:url "https://book.douban.com/review/789/")))))
      (let ((result
             (douban--submit-review meta raw session "标题")))
        (should (equal (plist-get result :id) "789"))
        (should
         (equal
          (plist-get result :url)
          "https://book.douban.com/review/789/"))))
    (should (equal (car captured) "POST"))
    (should
     (equal
      (cadr captured)
      "https://www.douban.com/j/review/create"))
    (let ((arguments (nth 2 captured)))
      (should
       (plist-get arguments :allow-redirect-response))
      (let ((body (plist-get arguments :body)))
        (should (string-match-p "review%5Btitle%5D=" body))
        (should (string-match-p "ck=abcd" body))))))

(ert-deftest douban-test-submit-review-update-endpoint ()
  (let* ((meta
          (douban-test--review-meta
          (list
           :subject-id "123"
           :id "789")
           "标题"))
         (session
          (douban--make-session
           :ck "abcd"
           :host "book.douban.com"
           :state '(:app-name "book" :review-id "789")
           :referer
           "https://book.douban.com/review/789/edit"))
         captured)
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (setq captured (list method url arguments))
            (list
             :status 200
             :body "{\"url\":\"https://book.douban.com/review/789/\"}"
             :json '(:url "https://book.douban.com/review/789/")))))
      (douban--submit-review
       meta (douban--html-to-draft "<p>正文</p>")
       session "标题"))
    (should (equal (car captured) "POST"))
    (should
     (equal
      (cadr captured)
      "https://book.douban.com/j/review/789/update"))
    (should-not
     (plist-get
      (nth 2 captured)
      :allow-redirect-response))))

(ert-deftest douban-test-create-network-error-is-marked-ambiguous ()
  (let* ((meta
         (douban-test--review-meta
           (list :subject-id "123")
           "标题"))
         (session
          (douban--make-session
           :kind 'review
           :ck "abcd"
           :host "www.douban.com"
           :state '(:app-name "book")
           :referer
           "https://www.douban.com/subject/123/new_review")))
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (&rest _arguments)
            (signal 'plz-curl-error '("simulated timeout")))))
      (should-error
       (douban--submit-review
        meta (douban--html-to-draft "<p>正文</p>")
        session "标题")
       :type 'douban-create-result-unknown))))

(ert-deftest douban-test-create-request-does-not-wrap-program-errors ()
  (let ((condition
         (should-error
          (douban--create-request
           (lambda ()
             (error "programming bug"))
           "创建结果不确定。")
          :type 'error)))
    (should (eq (car condition) 'error))
    (should
     (string-match-p
      "programming bug"
      (error-message-string condition)))))

(ert-deftest douban-test-create-form-error-is-not-ambiguous ()
  (let* ((meta
         (douban-test--review-meta
           (list :subject-id "123")
           "标题"))
         (session
          (douban--make-session
           :ck "abcd"
           :host "www.douban.com"
           :state '(:app-name "book")
           :referer
           "https://www.douban.com/subject/123/new_review")))
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (&rest _arguments)
            (list
             :status 200
             :body "{\"errors\":{\"title\":\"请填写标题\"}}"
             :json '(:errors (:title "请填写标题"))))))
      (let ((condition
             (should-error
              (douban--submit-review
               meta (douban--html-to-draft "<p>正文</p>")
               session "标题")
              :type 'user-error)))
        (should-not
         (eq (car condition)
             'douban-create-result-unknown))))))

(ert-deftest douban-test-review-response-errors-are-reported ()
  (let ((session
         (douban--make-session
          :host "www.douban.com")))
    (should-error
     (douban--review-mutation-result
      (list
       :status 200
       :body "{\"errors\":{\"title\":\"请填写标题\"}}"
       :json '(:errors (:title "请填写标题")))
      session nil)
     :type 'user-error)))

(ert-deftest douban-test-review-create-http-408-is-ambiguous ()
  (let ((session
         (douban--make-session
          :host "book.douban.com"
          :state '(:app-name "book")
          :referer
          "https://book.douban.com/subject/1/new_review")))
    (should-error
     (douban--review-mutation-result
      '(:status 422
        :body "invalid review"
        :json nil)
      session nil)
     :type 'user-error)
    (dolist (status '(302 408 500))
      (should-error
       (douban--review-mutation-result
        (list
         :status status
         :body "result unknown"
         :json nil)
        session nil)
       :type 'douban-create-result-unknown))))

(ert-deftest douban-test-json-false-is-not-a-mutation-error ()
  (let ((review-session
         (douban--make-session
          :host "book.douban.com"
          :state '(:app-name "book"))))
    (should
     (equal
      (douban--review-mutation-result
       '(:status 200
         :body
         "{\"errors\":false,\"url\":\"https://book.douban.com/review/91/\"}"
         :json
         (:errors :json-false
          :url "https://book.douban.com/review/91/"))
       review-session nil)
      '(:id "91"
        :url "https://book.douban.com/review/91/")))))

(ert-deftest douban-test-review-response-requires-canonical-url ()
  (let ((session
         (douban--make-session
          :host "book.douban.com"
          :state '(:app-name "book")
          :referer
          "https://book.douban.com/subject/1/new_review")))
    (should-error
     (douban--review-mutation-result
      (list
       :status 200
       :body "{\"url\":\"https://evil.example/review/777/\"}"
       :json '(:url "https://evil.example/review/777/"))
      session nil)
     :type 'douban-create-result-unknown)
    (dolist (field '(:review_id :id :result))
      (should-error
       (douban--review-mutation-result
        (list
         :status 200
         :body "{}"
         :json (list field "777"))
        session nil)
       :type 'douban-create-result-unknown))
    (should-error
     (douban--review-mutation-result
      (list
       :status 200
       :body
       "{\"url\":\"https://book.douban.com/review/778/\"}"
       :json
       '(:url "https://book.douban.com/review/778/"))
      session "777")
     :type 'error)))

(ert-deftest douban-test-multipart-keeps-binary-bytes ()
  (let* ((bytes (unibyte-string 0 1 127 255))
         (multipart
          (douban--multipart-body
           '(("ck" . "abcd"))
           :file-field "picfile"
           :file-name "x.bin"
           :file-mime "application/octet-stream"
           :file-bytes bytes))
         (body (cdr multipart)))
    (should-not (multibyte-string-p body))
    (should (string-match-p "name=\"ck\"" body))
    (should (string-match-p "name=\"picfile\"" body))
    (should (string-match-p (regexp-quote bytes) body))))

(ert-deftest douban-test-review-remote-image-downloads-before-upload ()
  (let* ((session
          (douban--make-session
           :kind 'review
           :ck "abcd"
           :host "www.douban.com"
           :referer
           "https://www.douban.com/subject/1/new_review"
           :state
           '(:upload-field "upload_auth_token"
             :upload-token "dummy-token")))
         (source "https://example.com/image.jpg")
         (bytes (unibyte-string 255 216 255 224 0 16))
         downloaded
         uploaded)
    (cl-letf
        (((symbol-function 'douban--download-image-url)
          (lambda (url)
            (setq downloaded url)
            (cons "image/jpeg" bytes)))
         ((symbol-function 'douban--upload-image-bytes)
          (lambda (received-session received-bytes received-mime)
            (setq
             uploaded
             (list
              received-session received-bytes received-mime))
            '(:url
              "https://img1.doubanio.com/view/note/l/public/p9.webp"))))
      (let ((photo
             (douban--upload-image-url
              session source)))
        (should
         (equal
          (douban--photo-url photo 'review)
          "https://img1.doubanio.com/view/note/l/public/p9.webp"))))
    (should (equal downloaded source))
    (should (eq (nth 0 uploaded) session))
    (should (equal (nth 1 uploaded) bytes))
    (should (equal (nth 2 uploaded) "image/jpeg"))))

(ert-deftest douban-test-photo-url-accepts-current-response-fields-only ()
  (should
   (equal
    (douban--photo-url
     '(:url
       "https://img1.doubanio.com/view/photo/large/public/p9.webp")
     'review)
    "https://img1.doubanio.com/view/photo/large/public/p9.webp"))
  (should
   (equal
    (douban--photo-url
     '(:thumb
       "https://img1.doubanio.com/view/photo/small/public/p9.webp")
     'review)
    "https://img1.doubanio.com/view/photo/large/public/p9.webp"))
  (should-error
   (douban--photo-url
    '(:src
      "https://img1.doubanio.com/view/photo/large/public/p9.webp")
    'review)
   :type 'error)
  (should-error
   (douban--photo-url
    '(:thumb
      "https://img1.doubanio.com/view/note/small/public/p9.webp")
    'note)
   :type 'error))

(ert-deftest douban-test-rewrite-cdn-image-without-network ()
  (let* ((source-url
          "https://img1.doubanio.com/view/note/l/public/p123.webp")
         (url
          "https://img1.doubanio.com/view/note/l/public/p123.webp")
         (raw
          (douban--html-to-draft
           (format "<figure><img src=\"%s\"><figcaption>图</figcaption></figure>"
                   source-url)))
         (network-called nil))
    (cl-letf
        (((symbol-function 'douban--upload-image-url)
          (lambda (&rest _)
            (setq network-called t)
            (error "must not upload CDN image"))))
      (douban--rewrite-draft-images
       raw (douban--make-session)
       default-directory))
    (should-not network-called)
    (let* ((entity
            (gethash "0" (plist-get raw :entityMap)))
           (data (plist-get entity :data)))
      (should (equal (plist-get data :src) url))
      (should (equal (plist-get data :id) "123"))
      (should (equal (plist-get data :caption) "图")))))

(ert-deftest douban-test-rewrite-rejects-non-https-cdn-image ()
  (dolist
      (source
       '("http://img1.doubanio.com/view/note/l/public/p123.webp"
         "//img1.doubanio.com/view/note/l/public/p123.webp"))
    (let ((raw
           (douban--html-to-draft
            (format "<p><img src=\"%s\"></p>" source))))
      (should-error
       (douban--rewrite-draft-images
        raw (douban--make-session)
        default-directory)
       :type 'user-error))))

(ert-deftest douban-test-rewrite-repeated-image-uploads-each-occurrence ()
  (let* ((source "https://example.org/source.gif")
         (raw
          (douban--html-to-draft
           (concat
            (format "<p><img src=\"%s\"></p>" source)
            (format "<p><img src=\"%s\"></p>" source))))
         (uploaded-url
          "https://img1.doubanio.com/view/photo/l/public/p88.webp")
         (uploads 0))
    (cl-letf
        (((symbol-function 'douban--upload-image-url)
          (lambda (_session actual-source)
            (should (equal actual-source source))
            (cl-incf uploads)
            (list :id "88" :url uploaded-url))))
      (douban--rewrite-draft-images
       raw (douban--make-session)
       default-directory))
    (should (= uploads 2))))

(ert-deftest douban-test-rewrite-draft-images-follows-first-reference-order ()
  (cl-labels
      ((image
        (source)
        (list
         :type "IMAGE"
         :mutability "IMMUTABLE"
         :data (list :src source :caption ""))))
    (let* ((shared "https://example.org/shared.png")
           (zero "https://example.org/zero.png")
           (raw
            (douban-test--entity-raw
             `(("1" . ,(image shared))
               ("9" .
                ,(image "https://example.org/orphan.png"))
               ("0" . ,(image zero))
               ("2" . ,(image shared)))
             '("2" "0" "2")
             '("1")))
           requests
           (next-id 0))
      (cl-letf
          (((symbol-function 'douban--upload-image-url)
            (lambda (_session source)
              (push source requests)
              (cl-incf next-id)
              (list
               :id (number-to-string next-id)
               :url
               (format
                "https://img1.doubanio.com/view/photo/l/public/p%d.webp"
                next-id)))))
        (should
         (eq
          (douban--rewrite-draft-images
           raw (douban--make-session :kind 'review)
           default-directory)
          raw)))
      ;; Key 2 的重复 range 只上传一次；key 1 即使 source 相同仍单独上传。
      (should
       (equal
        (nreverse requests)
        (list shared zero shared))))))

(ert-deftest douban-test-image-data-keeps-required-fields ()
  (let ((data
         (douban--normalized-image-data
          "https://img1.doubanio.com/view/photo/l/public/p9.webp"
          "动画"
          '(:id 9
            :thumb
            "https://img1.doubanio.com/view/photo/s/public/p9.webp"
            :width 640))))
    (should (equal (plist-get data :id) "9"))
    (should
     (equal
      (plist-get data :thumb)
      "https://img1.doubanio.com/view/photo/s/public/p9.webp"))
    (should (equal (plist-get data :caption) "动画"))
    (should-not (plist-member data :width))))

(ert-deftest douban-test-image-data-url-decodes-recognized-formats ()
  (dolist
      (case
       `(("image/png" . ,(unibyte-string 137 80 78 71 13 10 26 10))
         ("image/jpeg" . ,(unibyte-string 255 216 255 224 0 16 74 70 73 70))
         ("image/gif" . ,(unibyte-string 71 73 70 56 57 97))
         ("image/bmp" . ,(unibyte-string 66 77 14 0 0 0 0 0 0 0 0 0 0 0))
         ("image/webp" .
          ,(concat "RIFF" (unibyte-string 12 0 0 0) "WEBPVP8 "))
         ("image/svg+xml" .
          "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>")))
    (let* ((mime (car case))
           (bytes (cdr case))
           (url
            (format
             "data:%s;base64,%s"
             mime (base64-encode-string bytes t))))
      (should
       (equal
        (douban--decode-image-data-url url)
        (cons mime bytes)))))
  (let* ((bytes (unibyte-string 137 80 78 71 13 10 26 10))
         (url
          (format
           "DATA:IMAGE/PNG;BASE64,%s"
           (base64-encode-string bytes t))))
    (should
     (equal
      (douban--decode-image-data-url url)
      (cons "image/png" bytes)))))

(ert-deftest douban-test-image-data-url-rejects-invalid-shape ()
  (dolist
      (bad
       '("data:not-a-valid-image"
         "data:text/plain;base64,aGVsbG8="
         "data:image/png,not-base64"
         "data:image/png;charset=utf-8;base64,aGVsbG8="
         "data:image/png;base64,"
         "data:image/png\nInjected;base64,AAAA"))
    (should-error
     (douban--decode-image-data-url bad)
     :type 'user-error)))

(ert-deftest douban-test-image-mime-prefers-signature-with-open-fallback ()
  (let ((webp
         (concat "RIFF" (unibyte-string 12 0 0 0) "WEBPVP8 ")))
    (should
     (equal
      (douban--image-mime "image/anything" webp "test")
      "image/webp"))
    (should
     (equal
      (douban--image-mime "IMAGE/AVIF" "unknown bytes" "test")
      "image/avif"))
    (should-error
     (douban--image-mime "text/plain" "unknown bytes" "test")
     :type 'user-error)))

(ert-deftest douban-test-remote-image-accepts-any-recognized-image-type ()
  (let ((webp
         (concat "RIFF" (unibyte-string 12 0 0 0) "WEBPVP8 "))
        captured)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method url &rest arguments)
            (setq captured (list method url arguments))
            (make-plz-response
             :status 200
             :headers '(("content-type" . "image/webp"))
             :body webp))))
      (should
       (equal
        (douban--download-image-url "https://example.org/image")
        (cons "image/webp" webp))))
    (should
     (equal
      (cdr
       (assoc
        "Accept"
        (plist-get (nth 2 captured) :headers)))
      "image/*"))))

(ert-deftest douban-test-rewrite-data-image-uploads-decoded-bytes ()
  (let* ((bytes (unibyte-string 137 80 78 71 13 10 26 10))
         (source
          (format
           "data:image/png;base64,%s"
           (base64-encode-string bytes t)))
         (raw
          (douban--html-to-draft
           (format "<img src=\"%s\" alt=\"内嵌图\">" source)))
         (uploaded-url
          "https://img1.doubanio.com/view/photo/l/public/p91.webp")
         (uploads 0))
    (cl-letf
        (((symbol-function 'douban--upload-image-bytes)
          (lambda (_session uploaded mime)
            (setq uploads (1+ uploads))
            (should (equal uploaded bytes))
            (should (equal mime "image/png"))
            (list :id 91 :url uploaded-url)))
         ((symbol-function 'douban--upload-image-url)
          (lambda (&rest _arguments)
            (ert-fail "data URL must not use the remote-URL uploader"))))
      (douban--rewrite-draft-images
       raw (douban--make-session) nil))
    (should (= uploads 1))
    (should
     (equal
      (plist-get
       (plist-get
       (gethash "0" (plist-get raw :entityMap))
        :data)
       :src)
      uploaded-url))))

(ert-deftest douban-test-upload-image-bytes-builds-supported-multipart ()
  (let* ((bytes (unibyte-string 137 80 78 71 13 10 26 10))
         (session
          (douban--make-session
           :kind 'review
           :ck "abcd"
           :host "www.douban.com"
           :state
           '(:upload-field "upload_auth_token"
             :upload-token "dummy-token")))
         captured)
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (setq captured (list method url arguments))
            (list
             :status 200
             :body
             "{\"r\":0,\"photo\":{\"url\":\"https://img1.doubanio.com/view/photo/l/public/p91.webp\"}}"
             :json
             '(:r 0
	       :photo
	       (:url
                "https://img1.doubanio.com/view/photo/l/public/p91.webp"))))))
      (should
       (equal
        (plist-get
         (douban--upload-image-bytes
          session bytes "IMAGE/PNG")
         :url)
        "https://img1.doubanio.com/view/photo/l/public/p91.webp")))
    (should
     (equal
      (cadr captured)
      "https://www.douban.com/j/review/upload_image"))
    (let ((body (plist-get (nth 2 captured) :body)))
      (should-not (multibyte-string-p body))
      (should (string-match-p "filename=\"image.png\"" body))
      (should (string-match-p "Content-Type: image/png" body))
      (should (string-match-p (regexp-quote bytes) body)))))

(ert-deftest douban-test-local-image-source-carries-declared-mime ()
  (douban-test--with-temp-file
      ".png" (unibyte-string 71 73 70 56 57 97)
    (let ((descriptor
           (douban--image-source
            file (file-name-directory file))))
      (should (equal (plist-get descriptor :mime) "image/png"))
      (should
       (equal
        (plist-get descriptor :bytes)
        (unibyte-string 71 73 70 56 57 97))))))

(ert-deftest douban-test-file-image-url-rejects-remote-host ()
  (should
   (equal
    (douban--local-image-path
     "file:///tmp/example.png" "/unused")
    "/tmp/example.png"))
  (should
   (equal
    (douban--local-image-path
     "file://localhost/tmp/example.png" "/unused")
    "/tmp/example.png"))
  (should-error
   (douban--local-image-path
    "file://evil.example/tmp/example.png" "/unused")
   :type 'user-error))

(ert-deftest douban-test-local-image-path-normalizes-reference ()
  (let* ((base (file-name-as-directory temporary-file-directory))
         (default-directory base)
         (expected (expand-file-name "images/a b.png" base)))
    (should
     (equal
      (douban--local-image-path
       "images/a%20b.png?width=640#figure" base)
      expected))
    (should
     (equal
      (douban--local-image-path
       (concat "file://" expected "?width=640#figure") base)
      expected))
    (should
     (equal
      (douban--local-image-path
       "images/a%20b.png"
       "/ssh:example.invalid:/srv/notes/")
      "/ssh:example.invalid:/srv/notes/images/a b.png"))
    (should-not
     (douban--local-image-path
      "https://example.org/a.png?width=640#figure" base))
    (should-not
     (douban--local-image-path
      "https://example.org/a.png?width=640#figure" nil))
    (should
     (equal
      (douban--local-image-path "images/a.png" nil)
      (expand-file-name "images/a.png" default-directory)))))

(ert-deftest douban-test-image-url-validation-rejects-ambiguous-input ()
  (should (douban--https-url-p "https://example.org/image.png?q=1"))
  (should-not
   (douban--https-url-p "https://user@example.org/image.png"))
  (should-not
   (douban--https-url-p "https://example.org/a\\b.png"))
  (should-not
   (douban--https-url-p "https://example.org/image.png\nInjected"))
  (dolist
      (bad
       '("http://example.org/image.png"
         "https://"
         "https:///image.png"
         "https://user@example.org/image.png"
         "https://example.org/a\\b.png"))
    (let ((session (douban--make-session :kind 'review)))
      (cl-letf
          (((symbol-function 'douban--http-json)
            (lambda (&rest _arguments)
              (ert-fail "无效远程 URL 不应进入上传请求"))))
        (should-error
         (douban--upload-image-url session bad)
         :type 'user-error)))))

(ert-deftest douban-test-markdown-source-conversion-via-pandoc ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\ntitle: \"转换测试\"\ndouban:\n"
    "  review:\n"
    "    subject-id: \"123\"\n"
    "    subject-type: book\n"
    "---\n\n"
    "# 标题\n\n"
    (douban-test--long-text)
    "\n")
   (let* ((html (douban--source-html file))
          (raw (douban--html-to-draft html)))
     (should
      (equal
       (plist-get (aref (plist-get raw :blocks) 0) :type)
       "header-two"))
     (should
      (>=
       (douban--draft-character-count raw)
       douban-minimum-review-length)))))

(ert-deftest douban-test-markdown-toc-generates-linked-outline ()
  (skip-unless (executable-find "pandoc"))
  (douban-test--with-temp-file
      ".md"
      (concat
       "---\n"
       "title: '目录测试'\n"
       "toc: true\n"
       "douban:\n"
       "  note: {}\n"
       "---\n\n"
       "# 第一章\n\n正文。\n\n"
       "## 第二节\n\n内容。\n")
    (let* ((raw
            (douban--html-to-draft
             (douban--source-html file)))
           (blocks (append (plist-get raw :blocks) nil))
           (title (nth 0 blocks))
           (first (nth 1 blocks))
           (second (nth 2 blocks)))
      (should
       (equal
        (mapcar
         (lambda (block) (plist-get block :text))
         blocks)
        '("目录" "第一章" "第二节"
          "第一章" "正文。" "第二节" "内容。")))
      (should
       (equal
        (mapcar
         (lambda (block) (plist-get block :type))
         blocks)
        '("unstyled"
          "unordered-list-item" "unordered-list-item"
          "header-two" "unstyled" "header-two" "unstyled")))
      (should
       (equal
        (mapcar
         (lambda (block) (plist-get block :depth))
         blocks)
        '(0 0 1 0 0 0 0)))
      (should
       (equal
        (append (plist-get title :inlineStyleRanges) nil)
        '((:offset 0 :length 2 :style "BOLD"))))
      (should
       (equal
        (plist-get
         (plist-get
          (douban-test--block-first-entity raw first)
          :data)
         :url)
        "#第一章"))
      (should
       (equal
        (plist-get
         (plist-get
          (douban-test--block-first-entity raw second)
          :data)
         :url)
        "#第二节")))))

(ert-deftest douban-test-toc-state-is-isolated-between-documents ()
  (should-not (boundp 'douban--generated-toc-headings))
  (let* ((first
          (douban--html-to-draft
           (concat
            (douban-test--toc-marker 2)
            "<h2 id=\"first\">第一篇标题</h2>")))
         (second
          (douban--html-to-draft
           (concat
            (douban-test--toc-marker 2)
            "<h2 id=\"second\">第二篇标题</h2>")))
         (first-blocks (append (plist-get first :blocks) nil))
         (second-blocks (append (plist-get second :blocks) nil))
         (first-entry (nth 1 first-blocks))
         (second-entry (nth 1 second-blocks)))
    (should
     (equal
      (mapcar
       (lambda (block) (plist-get block :text))
       first-blocks)
      '("目录" "第一篇标题" "第一篇标题")))
    (should
     (equal
      (mapcar
       (lambda (block) (plist-get block :text))
       second-blocks)
      '("目录" "第二篇标题" "第二篇标题")))
    (should
     (equal
      (plist-get
       (plist-get
        (douban-test--block-first-entity first first-entry)
        :data)
       :url)
      "#第一篇标题"))
    (should
     (equal
      (plist-get
       (plist-get
        (douban-test--block-first-entity second second-entry)
        :data)
       :url)
      "#第二篇标题"))))

(ert-deftest douban-test-toc-heading-records-only-keep-consumed-fields ()
  (let* ((document
          (douban--parse-html
           (concat
            "<html><body>"
            (douban-test--toc-marker 3)
            "<h3 id=\"source-id\">精简记录</h3>"
            "</body></html>")))
         (body (car (dom-by-tag document 'body))))
    (should
     (equal
      (douban--prepare-section-navigation body)
      '((:level 3 :text "精简记录"))))))

(ert-deftest douban-test-markdown-toc-false-and-invalid-values ()
  (skip-unless (executable-find "pandoc"))
  (douban-test--with-temp-file
      ".md"
      (concat
       "---\ntoc: false\ndouban:\n  note: {}\n---\n"
       "# 标题\n")
    (let* ((raw
            (douban--html-to-draft
             (douban--source-html file)))
           (blocks (append (plist-get raw :blocks) nil)))
      (should (= (length blocks) 1))
      (should (equal (plist-get (car blocks) :text) "标题"))))
  (dolist
      (frontmatter
       '("toc: yes\n"
         "toc: []\n"
         "toc: {}\n"
         "toc: true\ntoc-depth: 7\n"
         "toc: true\ntoc-depth: nope\n"))
    (should-error
     (douban--md-toc-depth frontmatter)
     :type 'error)))



(ert-deftest douban-test-fragment-links-rewrite-to-visible-heading-text ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (case
       '((".md"
          . "## 展示标题 {#source-target}\n\n\
[跳转](#source-target)和[外链](https://example.org/page#source-target)。\n")))
    (douban-test--with-temp-file
        (car case) (cdr case)
      (let* ((raw
              (douban--html-to-draft
               (douban--source-html file)))
             (blocks (append (plist-get raw :blocks) nil))
             (link-block (nth 1 blocks))
             (ranges
              (append (plist-get link-block :entityRanges) nil))
             (first
              (gethash
               (number-to-string (plist-get (nth 0 ranges) :key))
               (plist-get raw :entityMap)))
             (second
              (gethash
               (number-to-string (plist-get (nth 1 ranges) :key))
               (plist-get raw :entityMap))))
        (should (equal (plist-get (car blocks) :text) "展示标题"))
        (should
         (equal
          (plist-get (plist-get first :data) :url)
          "#展示标题"))
        (should
         (equal
          (plist-get (plist-get second :data) :url)
          "https://example.org/page#source-target"))))))

(ert-deftest douban-test-fragment-percent-decoding-keeps-visible-text ()
  (let* ((raw
          (douban--html-to-draft
           (concat
            "<h2 id=\"目标\">完成度 100%</h2>"
            "<p><a href=\"#%E7%9B%AE%E6%A0%87\">跳转</a></p>"
            "<h2 id=\"rate%25\">原样百分号</h2>"
            "<p><a href=\"#rate%25\">再跳转</a></p>")))
         (blocks (append (plist-get raw :blocks) nil)))
    (should
     (equal
      (mapcar
       (lambda (block)
         (when-let* ((entity
                     (douban-test--block-first-entity raw block)))
           (plist-get (plist-get entity :data) :url)))
       (list (nth 1 blocks) (nth 3 blocks)))
      '("#完成度 100%" "#原样百分号")))))

(ert-deftest douban-test-navigation-ignores-document-reference-links ()
  (skip-unless (executable-find "pandoc"))
  (douban-test--with-temp-file
      ".md"
      (concat
       "---\ntoc: true\ndouban:\n  note: {}\n---\n"
       "# 正文标题\n\n正文脚注[^n]。\n\n"
       "[^n]:\n"
       "    ## 脚注标题 {#inside-note}\n\n"
       "    脚注内容。\n")
    (let* ((raw
            (douban--html-to-draft
             (douban--source-html file)))
           (blocks (append (plist-get raw :blocks) nil))
           (toc-items
            (cl-remove-if-not
             (lambda (block)
               (equal
                (plist-get block :type)
                "unordered-list-item"))
             blocks)))
      (should (= (length toc-items) 1))
      (should (equal (plist-get (car toc-items) :text) "正文标题"))
      (should
       (douban--draft-has-entity-type-p raw "LINK")))))

(ert-deftest douban-test-navigation-excludes-headings-in-nested-blocks ()
  (let* ((raw
          (douban--html-to-draft
           (concat
            (douban-test--toc-marker 3)
            "<blockquote><h2 id=\"quoted\">引用标题</h2></blockquote>"
            "<ul><li><h2 id=\"listed\">列表标题</h2></li></ul>"
            "<h2 id=\"body\">正文标题</h2>")))
         (toc-items
          (cl-remove-if-not
           (lambda (block)
             (and
              (equal
               (plist-get block :type)
               "unordered-list-item")
              (douban-test--block-first-entity raw block)))
           (append (plist-get raw :blocks) nil))))
    (should (= (length toc-items) 1))
    (should (equal (plist-get (car toc-items) :text) "正文标题")))
  (should-error
   (douban--html-to-draft
    (concat
     "<blockquote><h2 id=\"quoted\">引用标题</h2></blockquote>"
     "<p><a href=\"#quoted\">跳转</a></p>"))
   :type 'user-error))

(ert-deftest douban-test-navigation-rejects-ambiguous-or-formatted-targets ()
  (let ((duplicate
         (concat
          "<h1 id=\"first\">重复</h1>"
          "<h2 id=\"second\">重复</h2>"))
        (formatted
         "<h2 id=\"styled\">普通 <strong>强调</strong> 标题</h2>"))
    (should-error
     (douban--html-to-draft
      (concat
       (douban-test--toc-marker 3)
       duplicate))
     :type 'user-error)
    (should-error
     (douban--html-to-draft
      (concat
       duplicate
       "<p><a href=\"#first\">跳转</a></p>"))
     :type 'user-error)
    (should-error
     (douban--html-to-draft
      (concat
       (douban-test--toc-marker 3)
       formatted))
     :type 'user-error)
    (should-error
     (douban--html-to-draft
      (concat
       formatted
       "<p><a href=\"#styled\">跳转</a></p>"))
     :type 'user-error)
    ;; 未启用目录且没有引用时，不限制普通正文标题。
    (should
     (= (length
         (plist-get
          (douban--html-to-draft
           (concat duplicate formatted))
          :blocks))
        3))))



(ert-deftest douban-test-pandoc-keeps-link-ordinary-before-resolution ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (source
       '("[局外人](https://book.douban.com/subject/4908885/)"
         "[局外人](https://book.douban.com/subject/4908885/ \"cardinal\")"))
    (let* ((html (douban--pandoc-to-html "gfm" source))
           (raw (douban--html-to-draft html))
           (entity (douban-test--first-draft-entity raw)))
      (should-not
       (string-match-p "data-draft-type=\"link-card\"" html))
      (should (equal (plist-get entity :type) "LINK")))))

(ert-deftest douban-test-pandoc-ordinary-subject-links-remain-links ()
  (skip-unless (executable-find "pandoc"))
  (let ((url "https://book.douban.com/subject/4908885/"))
    (dolist
        (case
         '(("gfm"
            . "正文 [普通文字](https://book.douban.com/subject/4908885/) 后文")))
      (let* ((html (douban--pandoc-to-html (car case) (cdr case)))
             (raw (douban--html-to-draft html))
             (before (copy-tree raw)))
        (cl-letf
            (((symbol-function 'douban--plz-request)
              (lambda (&rest _arguments)
                (ert-fail "普通链接不得调用卡片解析接口"))))
          (should (eq (douban--rewrite-draft-cards raw) raw)))
        (should (equal raw before))
        (let* ((block (aref (plist-get raw :blocks) 0))
               (entity (douban-test--first-draft-entity raw))
               (data (plist-get entity :data)))
          (should (equal (plist-get block :text)
                         "正文 普通文字 后文"))
          (should (equal (plist-get entity :type) "LINK"))
          (should (equal (plist-get entity :mutability) "MUTABLE"))
          (should (equal (plist-get data :url) url))
          (should-not (plist-member data :display)))))))


(ert-deftest douban-test-pandoc-highlight-filter-always-deletes-temp-file ()
  (dolist (fail '(nil t))
    (let (filters)
      (cl-letf
          (((symbol-function 'douban--shell-convert)
            (lambda (_program arguments _input)
              (should
               (= (cl-count
                   "--lua-filter" arguments :test #'equal)
                  1))
              (setq
               filters
               (cl-loop
                for tail on arguments
                when (equal (car tail) "--lua-filter")
                collect (cadr tail)))
              (should (= (length filters) 1))
              (dolist (filter filters)
                (should (file-exists-p filter)))
              (if fail
                  (error "模拟 Pandoc 失败")
                "<p>正文</p>"))))
        (if fail
            (should-error
             (douban--pandoc-to-html "markdown+mark" "正文")
             :type 'error)
          (should
           (equal
            (douban--pandoc-to-html "markdown+mark" "正文")
            "<p>正文</p>"))))
      (should (= (length filters) 1))
      (dolist (filter filters)
        (should-not (file-exists-p filter))))))

(ert-deftest douban-test-pandoc-filters-follow-markdown-reader ()
  (dolist
      (case
       '(("gfm" . nil)
         ("markdown+mark"
          . ("douban-highlight-block-"))))
    (let (filter-names)
      (cl-letf
          (((symbol-function 'douban--shell-convert)
            (lambda (_program arguments _input)
              (setq
               filter-names
               (mapcar
                #'file-name-nondirectory
                (cl-loop
                 for tail on arguments
                 when (equal (car tail) "--lua-filter")
                 collect (cadr tail))))
              "<p>正文</p>")))
        (should
         (equal
          (douban--pandoc-to-html (car case) "正文")
          "<p>正文</p>")))
      (should (= (length filter-names) (length (cdr case))))
      (cl-mapc
       (lambda (name prefix)
         (should (string-prefix-p prefix name)))
       filter-names (cdr case)))))

(ert-deftest douban-test-pandoc-filter-creation-failure-skips-shell ()
  (let (shell-called)
    (cl-letf
        (((symbol-function 'make-temp-file)
          (lambda (&rest _arguments)
            (error "模拟 filter 创建失败")))
         ((symbol-function 'douban--shell-convert)
          (lambda (&rest _arguments)
            (setq shell-called t)
            (ert-fail
             "filter 尚未全部创建时不得调用 Pandoc"))))
      (should-error
       (douban--pandoc-to-html "markdown+mark" "正文")
       :type 'error))
    (should-not shell-called)))

(ert-deftest douban-test-publish-file-checkpoints-created-id ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\ntitle: \"端到端测试\"\ndouban:\n"
    "  review:\n"
    "    subject-id: \"123\"\n"
    "    subject-type: book\n"
    "---\n\n"
    (douban-test--long-text)
    "\n")
   (let* ((meta (douban--read-meta file))
          (session
           (douban--make-session
            :ck "abcd"
            :host "www.douban.com"
            :state '(:app-name "book")
            :referer
            "https://www.douban.com/subject/123/new_review"))
          events)
     (cl-letf
         (((symbol-function 'yes-or-no-p)
           (lambda (&rest _arguments)
             (ert-fail "publish must not ask for confirmation")))
          ((symbol-function 'douban--review-direct-session)
           (lambda (_meta) session))
          ((symbol-function 'douban--review-editor-session)
           (lambda (_meta)
             (ert-fail "text-only review must not load a page session")))
          ((symbol-function 'douban--submit-review)
           (lambda (_meta _raw _session _title)
             (push 'submit events)
             '(:id "999"
                   :url "https://book.douban.com/review/999/")))
          ((symbol-function 'douban--remove-created-review-broadcast)
           (lambda (actual-session review-id)
             (should (eq actual-session session))
             (should (equal review-id "999"))
             (should
              (equal
               (plist-get (douban--read-meta file) :review-id)
               "999"))
             (push 'cleanup events)
             "9001")))
       (should (equal (douban--publish-file file meta) "999")))
     (should (equal (nreverse events) '(submit cleanup)))
     (let ((saved (douban--read-meta file)))
       (should (equal (plist-get saved :review-id) "999"))))))

(ert-deftest douban-test-review-broadcast-option-keeps-created-broadcast ()
  (let ((douban-review-send-broadcast t)
        (meta
         (douban-test--review-meta
          '(:subject-id "123") "长评")))
    (cl-letf
        (((symbol-function 'douban--source-html)
          (lambda (_file)
            (concat "<p>" (douban-test--long-text) "</p>")))
         ((symbol-function 'douban--review-direct-session)
          (lambda (_meta)
            (douban--make-session :kind 'review)))
         ((symbol-function 'douban--submit-review)
          (lambda (&rest _arguments)
            '(:id "999"
              :url "https://book.douban.com/review/999/")))
         ((symbol-function 'douban--checkpoint-published-content)
          (lambda (&rest _arguments) "999"))
         ((symbol-function 'douban--remove-created-review-broadcast)
          (lambda (&rest _arguments)
            (ert-fail "启用选项时不应删除评论广播"))))
      (should
       (equal
        (douban--publish-review-file "/tmp/review.md" meta)
        "999")))))

(ert-deftest douban-test-review-cleanup-failure-keeps-checkpointed-id ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\ntitle: 长评\ndouban:\n"
    "  review:\n"
    "    subject-id: '123'\n"
    "    subject-type: book\n"
    "---\n\n"
    (douban-test--long-text)
    "\n")
   (let ((meta (douban--read-meta file))
         (submit-count 0))
     (cl-letf
         (((symbol-function 'douban--source-html)
           (lambda (_file)
             (concat "<p>" (douban-test--long-text) "</p>")))
          ((symbol-function 'douban--review-direct-session)
           (lambda (_meta)
             (douban--make-session :kind 'review)))
          ((symbol-function 'douban--submit-review)
           (lambda (&rest _arguments)
             (cl-incf submit-count)
             '(:id "999"
               :url "https://book.douban.com/review/999/")))
          ((symbol-function 'douban--remove-created-review-broadcast)
           (lambda (_session review-id)
             (should (equal review-id "999"))
             (error "首页暂时不可用"))))
       (let ((condition
              (should-error
               (douban--publish-review-file file meta)
               :type 'douban-review-broadcast-cleanup-failed)))
         (should
          (string-match-p "999" (error-message-string condition)))
         (should
          (string-match-p
           "不要重新发布" (error-message-string condition)))))
     (should (= submit-count 1))
     (should
      (equal
       (plist-get (douban--read-meta file) :review-id)
       "999")))))

(ert-deftest douban-test-publish-reports-checkpoint-failure-with-id ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\ntitle: \"写回失败测试\"\ndouban:\n"
    "  review:\n"
    "    subject-id: \"123\"\n"
    "    subject-type: book\n"
    "---\n\n"
    (douban-test--long-text)
    "\n")
   (let* ((meta (douban--read-meta file))
          (session
           (douban--make-session
            :ck "abcd"
            :host "www.douban.com"
            :state '(:app-name "book")
            :referer
            "https://www.douban.com/subject/123/new_review"))
          (submit-count 0)
          (checkpoint-count 0))
     (cl-letf
         (((symbol-function 'douban--review-direct-session)
           (lambda (_meta) session))
          ((symbol-function 'douban--review-editor-session)
           (lambda (_meta)
             (ert-fail "text-only review must not load a page session")))
          ((symbol-function 'douban--submit-review)
           (lambda (_meta _raw _session _title)
             (cl-incf submit-count)
             (should (= checkpoint-count 0))
             '(:id "999"
                   :url "https://book.douban.com/review/999/")))
          ((symbol-function 'douban--checkpoint-meta)
           (lambda (&rest _arguments)
             (cl-incf checkpoint-count)
             (error "disk full")))
          ((symbol-function 'douban--remove-created-review-broadcast)
           (lambda (&rest _arguments)
             (ert-fail "metadata 写回失败后不得尝试删除广播"))))
       (let ((condition
              (should-error
               (douban--publish-file file meta)
               :type 'douban-published-but-not-checkpointed)))
         (should
          (string-match-p
           "999"
           (error-message-string condition)))
         (should
          (string-match-p
           (regexp-quote "https://book.douban.com/review/999/")
           (error-message-string condition)))))
     (should (= submit-count 1))
     (should (= checkpoint-count 1))
     (let ((saved (douban--read-meta file)))
       (should-not (plist-get saved :review-id))))))

(ert-deftest douban-cookie-browser-is-a-custom-choice ()
  (should (custom-variable-p 'douban-cookie-browser))
  (should (eq (default-value 'douban-cookie-browser) 'firefox))
  (should
   (equal
    (mapcar (lambda (choice) (car (last choice)))
            (cdr (get 'douban-cookie-browser 'custom-type)))
    '(firefox chromium chrome))))

(ert-deftest douban-review-send-broadcast-is-a-disabled-boolean-custom ()
  (should (custom-variable-p 'douban-review-send-broadcast))
  (should-not (default-value 'douban-review-send-broadcast))
  (should
   (eq (get 'douban-review-send-broadcast 'custom-type) 'boolean)))

(ert-deftest douban-default-reply-limit-is-an-all-following-custom ()
  (should (custom-variable-p 'douban-default-reply-limit))
  (should (eq (default-value 'douban-default-reply-limit) 'all))
  (should
   (equal
    (mapcar
     (lambda (choice) (car (last choice)))
     (cdr (get 'douban-default-reply-limit 'custom-type)))
    '(all following))))

(ert-deftest douban-default-original-is-an-enabled-boolean-custom ()
  (should (custom-variable-p 'douban-default-original))
  (should (eq (default-value 'douban-default-original) t))
  (should
   (eq (get 'douban-default-original 'custom-type) 'boolean)))

(ert-deftest douban-cookie-profile-directory-selects-one-fixed-store ()
  (should (custom-variable-p 'douban-cookie-profile-directory))
  (should-not (default-value 'douban-cookie-profile-directory))
  (let ((profile (make-temp-file "douban-test-cookie-profile-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "Network" profile))
          (dolist
              (relative
               '("cookies.sqlite"
                 "Network/Cookies"))
            (with-temp-file (expand-file-name relative profile)))
          (let ((douban-cookie-profile-directory profile))
            (dolist
                (case
                 '((firefox . "cookies.sqlite")
                   (chromium . "Network/Cookies")
                   (chrome . "Network/Cookies")))
              (should
               (equal
                (douban--cookie-store-file (car case))
                (expand-file-name (cdr case) profile))))))
      (delete-directory profile t))))

(ert-deftest douban-cookie-profile-directory-must-be-explicit-and-valid ()
  (let ((douban-cookie-profile-directory nil))
    (should-error (douban--cookie-store-file 'firefox)))
  (let ((douban-cookie-profile-directory
         "/definitely/missing/douban-test-profile"))
    (should-error (douban--cookie-store-file 'firefox)))
  (let ((profile (make-temp-file "douban-test-empty-profile-" t)))
    (unwind-protect
        (let ((douban-cookie-profile-directory profile))
          (should-error (douban--cookie-store-file 'firefox))
          (should-error (douban--cookie-store-file 'chromium)))
      (delete-directory profile t)))
  (dolist (browser '(edge safari))
    (should-error (douban--cookie-store-file browser)))
  (should-error (douban--cookie-store-file 'unknown)))

(ert-deftest douban-cookie-domain-and-path-matching-follow-request-url ()
  (should
   (douban--cookie-domain-matches-p
    ".douban.com" "book.douban.com"))
  (should
   (douban--cookie-domain-matches-p ".douban.com" "douban.com"))
  (should-not
   (douban--cookie-domain-matches-p ".douban.com" "evildouban.com"))
  (should
   (douban--cookie-domain-matches-p
    "www.douban.com" "www.douban.com"))
  (should-not
   (douban--cookie-domain-matches-p
    "www.douban.com" "book.douban.com"))
  (should (douban--cookie-path-matches-p "/subject" "/subject"))
  (should
   (douban--cookie-path-matches-p
    "/subject" "/subject/123/new_review"))
  (should-not
   (douban--cookie-path-matches-p
    "/subject" "/subjects/123/new_review"))
  (should
   (equal
    (douban--cookie-url-parts
     "https://WWW.DOUBAN.COM/subject/123/new_review?q=/ignored#fragment")
    '("www.douban.com" "/subject/123/new_review" t))))

(ert-deftest douban-cookie-records-apply-secure-expiry-and-rfc-order ()
  (let* ((now (float-time))
         (records
          (list
           (douban--make-cookie-record
            :name "same" :value "root"
            :domain ".douban.com" :path "/"
            :expires (+ now 60) :secure nil :creation 1)
           (douban--make-cookie-record
            :name "same" :value "subject"
            :domain ".douban.com" :path "/subject"
            :expires nil :secure t :creation 2)
           (douban--make-cookie-record
            :name "expired" :value "no"
            :domain ".douban.com" :path "/"
            :expires (- now 1) :secure nil :creation 3)
           (douban--make-cookie-record
            :name "other-host" :value "no"
            :domain "book.douban.com" :path "/"
            :expires nil :secure nil :creation 4))))
    (should
     (equal
      (douban--cookie-records-for-url
       records
       "https://www.douban.com/subject/123/new_review?q=ignored")
      '(("same" . "subject")
        ("same" . "root"))))
    (should
     (equal
      (douban--cookie-records-for-url
       records "http://www.douban.com/subject/123/new_review")
      '(("same" . "root"))))))

(ert-deftest douban-browser-cookie-dispatch-keeps-the-complete-url ()
  (let ((url
         "https://www.douban.com/subject/123/new_review?q=full")
        (store "/profile/cookie-store"))
    (cl-letf
        (((symbol-function 'douban--cookie-store-file)
          (lambda (browser)
            (if (memq browser '(firefox chromium chrome))
                store
              (error "unsupported")))))
      (let ((douban-cookie-browser 'firefox))
        (cl-letf
            (((symbol-function 'douban--read-firefox-cookies)
              (lambda (path request-url)
                (should (equal path store))
                (should (equal request-url url))
                '(("source" . "firefox")))))
          (should
           (equal
            (douban--read-browser-cookies url)
            '(("source" . "firefox"))))))
      (let ((douban-cookie-browser 'chromium))
        (cl-letf
            (((symbol-function 'douban--read-chromium-cookies)
              (lambda (path request-url browser)
                (should (equal path store))
                (should (equal request-url url))
                (should (eq browser 'chromium))
                '(("source" . "chromium")))))
          (should
           (equal
            (douban--read-browser-cookies url)
            '(("source" . "chromium"))))))
      (let ((douban-cookie-browser 'chrome))
        (cl-letf
            (((symbol-function 'douban--read-chromium-cookies)
              (lambda (path request-url browser)
                (should (equal path store))
                (should (equal request-url url))
                (should (eq browser 'chrome))
                '(("source" . "chrome")))))
          (should
           (equal
            (douban--read-browser-cookies url)
            '(("source" . "chrome"))))))
      (dolist (browser '(edge safari unknown))
        (let ((douban-cookie-browser browser))
          (should-error
           (douban--read-browser-cookies url)
           :type 'error))))))

(ert-deftest douban-browser-cookie-reading-requires-gnu-linux ()
  (let ((system-type 'darwin)
        (douban-cookie-browser 'firefox))
    (cl-letf
        (((symbol-function 'douban--cookie-store-file)
          (lambda (&rest _arguments)
            (ert-fail "非 GNU/Linux 系统不应解析 profile"))))
      (should-error
       (douban--read-browser-cookies "https://www.douban.com/")
       :type 'error))))

(ert-deftest douban-firefox-readonly-uri-escapes-reserved-path-characters ()
  (let* ((directory
          (make-temp-file "douban-test-firefox-uri-#?-" t))
         (database (expand-file-name "cookies.sqlite" directory))
         db)
    (unwind-protect
        (progn
          (setq db (sqlite-open database))
          (sqlite-execute
           db
           (concat
            "CREATE TABLE moz_cookies ("
            "name TEXT, value TEXT, host TEXT, path TEXT, "
            "expiry INTEGER, isSecure INTEGER, creationTime INTEGER, "
            "originAttributes TEXT)"))
          (sqlite-execute
           db
           "INSERT INTO moz_cookies VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
           '("dbcl2" "login" ".douban.com" "/" 0 1 1 ""))
          (sqlite-close db)
          (setq db nil)
          (should
           (equal
            (douban--read-firefox-cookies
             database "https://www.douban.com/")
            '(("dbcl2" . "login")))))
      (when db
        (ignore-errors (sqlite-close db)))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest douban-cookie-database-readonly-uses-escaped-uri-and-transaction ()
  (let* ((directory
          (make-temp-file "douban-test-cookie-uri-#?-" t))
         (path (expand-file-name "Cookies" directory))
         statements
         closed)
    (unwind-protect
        (cl-letf
            (((symbol-function 'sqlite-open)
              (lambda (&rest args)
                (should-not args)
                'readonly-db))
             ((symbol-function 'sqlite-execute)
              (lambda (db statement &optional values)
                (should (eq db 'readonly-db))
                (push (list statement values) statements)))
             ((symbol-function 'sqlite-close)
              (lambda (db)
                (should (eq db 'readonly-db))
                (setq closed t)))
             ((symbol-function 'make-temp-file)
              (lambda (&rest _args)
                (ert-fail "Successful readonly query must not make a copy"))))
          (should
           (equal
            (douban--query-cookie-database
             path "test cookies"
             (lambda (db schema)
               (should (eq db 'readonly-db))
               (should (equal schema "cookies"))
               'readonly-result))
            'readonly-result))
          (setq statements (nreverse statements))
          (should
           (equal (mapcar #'car statements)
                  '("ATTACH DATABASE ? AS cookies"
                    "BEGIN"
                    "COMMIT")))
          (let ((uri (car (cadr (car statements)))))
            (should (string-match-p "%23%3F" uri))
            (should (string-suffix-p "?mode=ro&cache=private" uri)))
          (should closed))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest douban-cookie-database-fallback-is-only-for-sqlite-errors ()
  (let ((path (make-temp-file "douban-test-cookie-database-"))
        snapshot-created)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'sqlite-open)
                (lambda (&rest _args)
                  (error "key access denied")))
               ((symbol-function 'make-temp-file)
                (lambda (&rest _args)
                  (setq snapshot-created t)
                  (ert-fail "Ordinary errors must not trigger a copy"))))
            (should-error
             (douban--query-cookie-database
              path "test cookies" (lambda (&rest _args) nil)))
            (should-not snapshot-created))
          (let ((opens 0)
                schemas
                snapshot-directory
                (real-make-temp-file
                 (symbol-function 'make-temp-file)))
            (cl-letf
                (((symbol-function 'make-temp-file)
                  (lambda (&rest args)
                    (setq snapshot-directory
                          (apply real-make-temp-file args))))
                 ((symbol-function 'sqlite-open)
                  (lambda (&rest args)
                    (cl-incf opens)
                    (if args 'copy-db 'readonly-db)))
                 ((symbol-function 'sqlite-execute)
                  (lambda (db statement &optional _values)
                    (when (and (eq db 'readonly-db)
                               (string-prefix-p "ATTACH" statement))
                      (signal 'sqlite-error '("database is locked")))))
                 ((symbol-function 'sqlite-close) #'ignore))
              (should
               (equal
                (douban--query-cookie-database
                 path "test cookies"
                 (lambda (_db schema)
                   (push schema schemas)
                   'copied-result))
                'copied-result)))
            (should (= opens 2))
            (should (equal schemas '("main")))
            (should snapshot-directory)
            (should-not (file-exists-p snapshot-directory))))
      (ignore-errors (delete-file path)))))

(ert-deftest douban-cookie-database-reports-both-errors-and-cleans-up ()
  (let* ((source-directory
          (make-temp-file "douban-test-cookie-source-" t))
         (path (expand-file-name "Cookies" source-directory))
         snapshot-directory
         opened-snapshot
         closed
         rollbacks
         (real-make-temp-file (symbol-function 'make-temp-file)))
    (unwind-protect
        (progn
          (with-temp-file path (insert "database"))
          (with-temp-file (concat path "-wal") (insert "wal"))
          (with-temp-file (concat path "-shm") (insert "shm"))
          (cl-letf
              (((symbol-function 'make-temp-file)
                (lambda (&rest args)
                  (setq snapshot-directory
                        (apply real-make-temp-file args))))
               ((symbol-function 'sqlite-open)
                (lambda (&rest args)
                  (if args
                      (progn
                        (setq opened-snapshot (car args))
                        (should (file-exists-p opened-snapshot))
                        (should
                         (file-exists-p (concat opened-snapshot "-wal")))
                        (should-not
                         (file-exists-p (concat opened-snapshot "-shm")))
                        'copy-db)
                    'readonly-db)))
               ((symbol-function 'sqlite-execute)
                (lambda (db statement &optional _values)
                  (when (equal statement "ROLLBACK")
                    (push db rollbacks))))
               ((symbol-function 'sqlite-close)
                (lambda (db)
                  (push db closed))))
            (let ((message
                   (condition-case err
                       (progn
                         (douban--query-cookie-database
                          path "test cookies"
                          (lambda (_db schema)
                            (if (equal schema "cookies")
                                (signal
                                 'sqlite-error
                                 '("database is locked"))
                              (error "snapshot is invalid"))))
                         nil)
                     (error (error-message-string err)))))
              (should message)
              (should
               (string-match-p
                (regexp-quote "database is locked") message))
              (should
               (string-match-p
                (regexp-quote "snapshot is invalid") message))))
          (should opened-snapshot)
          (should (equal closed '(copy-db readonly-db)))
          (should (equal rollbacks '(copy-db readonly-db)))
          (should snapshot-directory)
          (should-not (file-exists-p snapshot-directory)))
      (ignore-errors (delete-directory source-directory t)))))

(ert-deftest douban-firefox-origin-attributes-selects-one-container ()
  (let* ((directory
          (make-temp-file "douban-test-firefox-container-" t))
         (database (expand-file-name "cookies.sqlite" directory))
         db)
    (unwind-protect
        (progn
          (setq db (sqlite-open database))
          (sqlite-execute
           db
           (concat
            "CREATE TABLE moz_cookies ("
            "name TEXT, value TEXT, host TEXT, path TEXT, "
            "expiry INTEGER, isSecure INTEGER, creationTime INTEGER, "
            "originAttributes TEXT)"))
          (dolist
              (row
               '(("dbcl2" "default" ".douban.com" "/" 0 1 1 "")
                 ("dbcl2" "work" ".douban.com" "/" 0 1 1
                  "^userContextId=2")))
            (sqlite-execute
             db
             "INSERT INTO moz_cookies VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
             row))
          (sqlite-close db)
          (setq db nil)
          (let ((douban-firefox-origin-attributes "^userContextId=2"))
            (should
             (equal
              (douban--read-firefox-cookies
               database "https://www.douban.com/")
              '(("dbcl2" . "work"))))))
      (when db
        (ignore-errors (sqlite-close db)))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest douban-firefox-and-chromium-share-cookie-database-lifecycle ()
  (douban-test--with-temp-file ".sqlite" ""
    (let ((url "https://www.douban.com/")
          (douban-firefox-origin-attributes "^userContextId=2")
          calls)
      (cl-letf
          (((symbol-function 'douban--query-cookie-database)
            (lambda (actual-path label query)
              (should (equal actual-path file))
              (push label calls)
              (funcall query 'test-db "test-schema")))
           ((symbol-function 'douban--select-firefox-cookies)
            (lambda (db table actual-url)
              (should (eq db 'test-db))
              (should (equal table "test-schema.moz_cookies"))
              (should (equal actual-url url))
              (should
               (equal douban-firefox-origin-attributes
                      "^userContextId=2"))
              'firefox-result))
           ((symbol-function 'douban--select-chromium-cookies)
            (lambda (db schema actual-url spec)
              (should (eq db 'test-db))
              (should (equal schema "test-schema"))
              (should (equal actual-url url))
              (should
               (equal
                spec
                (douban--chromium-browser-spec 'chromium)))
              'chromium-result)))
        (should
         (eq (douban--read-firefox-cookies file url)
             'firefox-result))
        (should
         (eq
          (douban--read-chromium-cookies file url 'chromium)
          'chromium-result)))
      (should (= (length calls) 2))
      (should (member "Firefox cookies" calls))
      (should (member "chromium Cookie" calls)))))

(ert-deftest douban-test-content-branch-normalization-and-helpers ()
  (let ((review
         (douban--meta-from-plist
          '(:review
            (:subject-id "12"
             :subject-type "book"))
          nil))
        (note
         (douban--meta-from-plist
          '(:note
            (:id "34"
             :privacy "friends"
             :cannot-reply "true"
             :author-tags ("随笔" "生活")))
          "日记标题")))
    (should (eq (plist-get review :kind) 'review))
    (should (equal (plist-get review :subject-id) "12"))
    (should (eq (plist-get note :kind) 'note))
    (should (equal (plist-get note :note-id) "34"))
    (should (equal (plist-get note :note-privacy) "friends"))
    (should (plist-get note :cannot-reply))
    (should
     (equal (plist-get note :author-tags) '("随笔" "生活"))))
  (should-error
   (douban--meta-from-plist
    '(:review (:subject-type "book")) nil)
   :type 'error)
  (should-error
   (douban--meta-from-plist
    '(:review (:subject-id "" :subject-type "book")) nil)
   :type 'error)
  (let ((status
         (douban--meta-from-plist
          '(:status nil)
          "会被忽略的广播标题")))
    (should (eq (plist-get status :kind) 'status))
    (should-not (plist-member status :status-id))
    (should-not (plist-get status :status-id))
    (should-not (plist-member status :title))
    (should-not (plist-get status :title))))

(ert-deftest douban-test-content-branch-rejects-zero-multiple-and-flat-fields ()
  (dolist
      (value
       '(nil
         (:subject-type "book")
         (:note-privacy "public")
         (:explanation-types "none")
         (:kind review :subject-id "1" :subject-type "book")
         (:review (:subject-id "1" :subject-type "book")
          :note nil)
         (:review "not-a-mapping")
         (:note ["not-a-mapping"])
         (:review (:subject-id "1" :subject-type "book")
          :review (:subject-id "2" :subject-type "book"))))
    (should-error
     (douban--meta-from-plist value nil)
     :type 'error)))

(ert-deftest douban-test-content-branch-rejects-unrelated-fields ()
  (dolist
      (value
       '((:review
          (:subject-id "1" :subject-type "book"
           :privacy "friends"))
         (:note (:rating "4"))
         (:status (:privacy "friends"))
         (:note (:unknown-field "x"))))
    (should-error
     (douban--meta-from-plist value nil)
     :type 'error))
  (should-error
   (douban--meta-from-plist
   (list :anthology-id "1") nil)
   :type 'error))

(ert-deftest douban-test-original-is-not-source-metadata ()
  (dolist
      (value
       '((:review
          (:subject-id "1" :subject-type "book"
           :original "true"))
         (:annotation
          (:subject-id "1" :original "true"))
         (:status (:original "true"))))
    (should-error
     (douban--meta-from-plist value "标题")
     :type 'error)))

(ert-deftest douban-test-reply-limit-is-not-source-metadata ()
  (dolist
      (value
       '((:annotation
          (:subject-id "1" :reply-limit "all"))
         (:status (:reply-limit "following"))))
    (should-error
     (douban--meta-from-plist value "标题")
     :type 'error)))

(ert-deftest douban-test-content-id-may-be-omitted-but-not-empty ()
  (dolist
      (value
       '((:review (:subject-id "1" :subject-type "book"))
         (:note nil)
         (:status nil)))
    (should (douban--meta-from-plist value nil)))
  (dolist
      (case
       '((:review :review-id
          (:subject-id "1" :subject-type "book" :id "11"))
         (:note :note-id (:id "22"))
         (:status :status-id
          (:id "303"))))
    (let ((meta
           (douban--meta-from-plist
            (list (car case) (nth 2 case))
            nil)))
      (should (equal (plist-get meta (nth 1 case))
                     (plist-get (nth 2 case) :id)))))
  (dolist (kind '(review note status))
    (dolist (empty '(nil "" " \t" "null"))
      (let ((fields
             (if (eq kind 'review)
                 (list :subject-id "1" :subject-type "book"
                       :id empty)
               (list :id empty))))
        (should-error
         (douban--meta-from-plist
          (list (douban--metadata-source-kind-key kind)
                fields)
          nil)
         :type 'error)))))

(ert-deftest douban-test-markdown-empty-id-and-sequence-branches-are-invalid ()
  (dolist
      (case
       '(("review" "    subject-id: '1'\n    subject-type: book\n")
         ("note" "")
         ("status" "")))
    (dolist (id-value '("" " ''" " '   '" " null"))
      (douban-test--with-temp-file
          ".md"
          (format
           "---\ndouban:\n  %s:\n%s    id:%s\n---\n"
           (car case) (cadr case) id-value)
        (should-error
         (douban--read-meta file)
         :type 'error))))
  (dolist (kind '("note" "status"))
    (douban-test--with-temp-file
        ".md"
        (format "---\ndouban:\n  %s: []\n---\n" kind)
      (should-error
       (douban--read-meta file)
       :type 'error)))
  (dolist (kind '("note" "status"))
    (douban-test--with-temp-file
        ".md"
        (format "---\ndouban:\n  %s: {}\n---\n" kind)
      (should
       (eq
        (plist-get (douban--read-meta file) :kind)
        (intern kind))))))


(ert-deftest douban-test-anthology-metadata-only-belongs-to-status ()
  (let ((status
         (douban--meta-from-plist
          '(:status (:anthology-id "42"))
          nil)))
    (should (equal (plist-get status :anthology-id) "42")))
  (should-error
   (douban--meta-from-plist
    '(:review
      (:subject-id "1"
       :subject-type "book"
       :anthology-id "42"))
    "长评标题")
   :type 'error)
  (should-error
   (douban--meta-from-plist
   '(:note (:anthology-id "42"))
    "日记标题")
   :type 'error))

(ert-deftest douban-test-current-user-id-uses-bootstrap-cookie-scope ()
  (let (cookie-url request)
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (setq cookie-url url)
            '(("dbcl2" . "www-login")
              ("ck" . "www-ck"))))
         ((symbol-function 'douban--http)
          (lambda (method url &rest arguments)
            (setq request (list method url arguments))
            '(:status 302
              :headers
              (("location" . "/people/alice/"))
              :body ""))))
      (should (equal (douban--current-user-id) "alice")))
    (should (equal cookie-url douban--ck-bootstrap-url))
    (pcase-let ((`(,method ,url ,arguments) request))
      (should (equal method "GET"))
      (should (equal url douban--ck-bootstrap-url))
      (should (plist-get arguments :allow-redirect-response))
      (let ((session (plist-get arguments :session)))
        (should (eq (douban--session-kind session) 'anthology))
        (should
         (equal
          (douban--session-referer session)
          douban--ck-bootstrap-url))
        (should (equal (douban--session-host session) "www.douban.com"))
        (should
         (equal
          (douban--session-cookies session)
          '(("dbcl2" . "www-login")
            ("ck" . "www-ck"))))))))

(ert-deftest douban-test-anthology-list-paginates-with-www-referer ()
  (let ((cookies
         '(("dbcl2" . "\"266418270:login-token\"")
           ("ck" . "page-ck")))
        requests)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method url &rest arguments)
            (push (list method url arguments) requests)
            (make-plz-response
             :status 200
             :headers nil
             :body
             (if (string-match-p "start=0&count=50&ck=page-ck\\'" url)
                 (concat
                  "{\"total\":3,\"start\":0,\"count\":2,\"doulists\":["
                  "{\"id\":11,\"title\":\"文集一\",\"items_count\":2},"
                  "{\"id\":\"12\",\"title\":\"文集二\",\"items_count\":4}"
                  "]}")
               (concat
                "{\"total\":3,\"start\":2,\"count\":1,\"doulists\":["
                "{\"id\":13,\"title\":\"文集三\",\"items_count\":1}"
                "]}"))))))
      (should
       (equal
        (douban--anthologies "user/name" cookies)
        '((:id "11" :title "文集一" :items-count 2)
          (:id "12" :title "文集二" :items-count 4)
          (:id "13" :title "文集三" :items-count 1)))))
    (setq requests (nreverse requests))
    (should (= (length requests) 2))
    (should
     (equal
      (mapcar #'cadr requests)
      '("https://m.douban.com/rexxar/api/v2/user/user%2Fname/anthologies?start=0&count=50&ck=page-ck"
        "https://m.douban.com/rexxar/api/v2/user/user%2Fname/anthologies?start=2&count=50&ck=page-ck")))
    (dolist (request requests)
      (should (equal (car request) "GET"))
      (let ((headers (plist-get (nth 2 request) :headers)))
        (should
         (equal
          (cdr (assoc-string "Cookie" headers))
          "dbcl2=266418270:login-token; ck=page-ck"))
        (should
         (equal
          (cdr (assoc-string "Referer" headers))
          "https://www.douban.com/"))))))

(ert-deftest douban-test-new-anthology-submits-current-multipart-contract ()
  (douban-test--with-temp-file
      ".jpg"
      (concat (unibyte-string #xff #xd8 #xff) "jpeg-bytes")
    (with-temp-buffer
      (setq
       douban--anthology-completion-cache
       '((:id "old" :title "旧缓存")))
      (let (request session-request)
        (cl-letf
            (((symbol-function 'douban--cookie-session)
              (lambda (kind url)
                (setq session-request (list kind url))
                (douban--make-session
                 :kind kind
                 :cookies '(("dbcl2" . "login"))
                 :ck "page-ck"
                 :referer url
                 :host "m.douban.com")))
             ((symbol-function 'douban--http-json)
              (lambda (method url &rest arguments)
                (setq request
                      (list method url arguments))
                (list
                 :status 201
                 :body
                 "{\"id\":\"42\",\"title\":\"我的文集\"}"
                 :json
                 '(:id "42"
                   :title "我的文集"
                   :category "common"
                   :doulist_type "anthology"
                   :items_count 0
                   :cover_url
                   "https://img.example/cover.jpg")))))
          (should
           (equal
            (douban-new-anthology "  我的文集  " file)
            "42")))
        (should
         (equal
          session-request
          (list
           'anthology
           douban--anthology-create-endpoint)))
        (pcase-let
            ((`(,method ,url ,arguments) request))
          (should (equal method "POST"))
          (should
           (equal
            url douban--anthology-create-endpoint))
          (should
           (eq (plist-get arguments :raw-body) t))
          (should
           (eq
            (plist-get arguments :allow-redirect-response)
            t))
          (should
           (string-prefix-p
            "multipart/form-data; boundary="
            (plist-get arguments :content-type)))
          (let ((headers
                 (plist-get
                  arguments :extra-headers))
                (body
                 (decode-coding-string
                  (plist-get arguments :body)
                  'utf-8)))
            (should
             (equal
              (cdr
               (assoc-string
                "X-CSRF-TOKEN" headers))
              "page-ck"))
            (should
             (equal
              (cdr (assoc-string "Origin" headers))
              "https://www.douban.com"))
            (should
             (equal
              (cdr (assoc-string "Referer" headers))
              "https://www.douban.com/"))
            (dolist
                (fragment
                 '("name=\"title\"\n\n我的文集\n"
                   "name=\"desc\"\n\n\n"
                   "name=\"is_private\"\n\nfalse\n"
                   "name=\"type\"\n\nanthology\n"
                   "name=\"header_bg_image\"; filename=\""))
              (should
               (string-search
                fragment
                body)))
            (should
             (string-search
              "Content-Type: image/jpeg"
              body))
            (should
             (string-search "jpeg-bytes" body))))
        (should
         (eq
          douban--anthology-completion-cache
          douban--anthology-completion-cache-unloaded))))))

(ert-deftest douban-test-new-anthology-validates-title-and-jpeg-cover ()
  (dolist (title '("" " \t " "123456789012345678901"))
    (should-error
     (douban--anthology-title title)
     :type 'user-error))
  (should
   (equal
    (douban--anthology-title
     "12345678901234567890")
    "12345678901234567890"))
  (should
   (equal
    (douban--anthology-title
     "😀😀😀😀😀😀😀😀😀😀")
    "😀😀😀😀😀😀😀😀😀😀"))
  (should-error
   (douban--anthology-title
    "😀😀😀😀😀😀😀😀😀😀😀")
   :type 'user-error)
  (douban-test--with-temp-file ".png" "png-bytes"
    (should-error
     (douban--anthology-cover file)
     :type 'user-error))
  (douban-test--with-temp-file ".jpg" "not-really-jpeg"
    (should-error
     (douban--anthology-cover file)
     :type 'user-error)))

(ert-deftest douban-test-new-anthology-classifies-create-results ()
  (should
   (equal
    (douban--anthology-create-result
     '(:status 200
       :body "{\"id\":42,\"title\":\"文集\"}"
       :json (:id 42 :title "文集"))
     "文集")
    '(:id "42"
      :title "文集"
      :url "https://www.douban.com/doulist/42/"
      :cover-url nil)))
  (should-error
   (douban--anthology-create-result
    '(:status 422
      :body "invalid"
      :json (:localized_message "名称不可用"))
    "文集")
   :type 'user-error)
  (should
   (equal
    (douban--anthology-create-result
     '(:status 201
       :body
       "{\"id\":\"42\",\"title\":\"远端标题\",\"doulist_type\":\"common\"}"
       :json
       (:id "42"
        :title "远端标题"
        :doulist_type "common"))
     "提交标题")
    '(:id "42"
      :title "提交标题"
      :url "https://www.douban.com/doulist/42/"
      :cover-url nil)))
  (dolist
      (response
       '((:status 302 :body "redirect" :json nil)
         (:status 503 :body "server error" :json nil)
         (:status 200
          :body "{\"title\":\"文集\"}"
          :json (:title "文集"))))
    (should-error
     (douban--anthology-create-result
     response "文集")
     :type 'douban-create-result-unknown)))

(ert-deftest douban-test-anthology-completion-labels-never-collide ()
  (let* ((anthologies
          '((:id "99" :title "Foo [42]" :items-count 9)
            (:id "100" :title "Foo [42] [42]" :items-count 10)
            (:id "42" :title "Foo" :items-count 4)
            (:id "43" :title "Foo" :items-count 3)))
         (entries
          (douban--anthology-completion-candidates anthologies))
         (labels (mapcar #'car entries)))
    (should (= (length labels) (length (delete-dups (copy-sequence labels)))))
    (should (member "Foo [42]" labels))
    (should (member "Foo [42] [42]" labels))
    (should
     (equal
      (plist-get
       (cdr (assoc-string "Foo [42] [42] [42]" entries))
       :id)
      "42"))
    (should
     (equal
      (plist-get (cdr (assoc-string "Foo [43]" entries)) :id)
      "43"))))

(ert-deftest douban-test-markdown-anthology-completion-uses-names-and-id ()
  (let ((anthologies
         '((:id "42" :title "旅行随笔" :items-count 3)
           (:id "43" :title "同名文集" :items-count 1)
           (:id "44" :title "同名文集" :items-count 8)))
        (source
         (generate-new-buffer
          " *douban-anthology-capf-source*"))
        (reads 0))
    (unwind-protect
        (with-current-buffer source
          (setq buffer-file-name "/tmp/status.md")
          (insert
           (concat
            "---\n"
            "douban:\n"
            "  status:\n"
            "    anthology-id: \n"
            "---\n\n"
            "广播正文\n"))
          (goto-char (point-min))
          (search-forward "anthology-id:")
          (end-of-line)
          (cl-letf
              (((symbol-function 'douban--current-user-id)
                (lambda () "current-user"))
               ((symbol-function
                 'douban--read-browser-cookies)
                (lambda (&rest _arguments) 'cookies))
               ((symbol-function 'douban--anthologies)
                (lambda (user-id cookies)
                  (cl-incf reads)
                  (should (equal user-id "current-user"))
                  (should (equal cookies 'cookies))
                  anthologies)))
            (let* ((capf
                    (douban-metadata-completion-at-point))
                   (start (nth 0 capf))
                   (end (nth 1 capf))
                   (table (nth 2 capf))
                   (properties (nthcdr 3 capf))
                   (annotation
                    (plist-get
                     properties :annotation-function))
                   (exit-function
                    (plist-get
                     properties :exit-function))
                   candidates)
              ;; 构造 CAPF 时不联网；真正列举候选才读取一次。
              (should capf)
              (should (= reads 0))
              (should
               (equal
                (buffer-substring start end)
                ""))
              (setq candidates
                    (all-completions "" table))
              (should
               (equal
                candidates
                '("旅行随笔"
                  "同名文集 [43]"
                  "同名文集 [44]")))
              (should (= reads 1))
              (should
               (equal
                (all-completions "" table)
                candidates))
              (should (= reads 1))
              (should
               (equal
                (funcall annotation "旅行随笔")
                "  （3 篇）"))
              (should
               (eq
                (plist-get properties :exclusive)
                t))
              ;; 补全 UI 插入名称；选定后回调在源 buffer 写入纯 ID。
              (delete-region start end)
              (goto-char start)
              (insert "旅行随笔")
              (with-temp-buffer
                (funcall
                 exit-function "旅行随笔" 'finished))
              (should
               (string-match-p
                "^    anthology-id: '42'$"
                (buffer-string)))
              (should
               (equal
                (plist-get
                 (douban--current-buffer-meta)
                 :anthology-id)
                "42")))))
      (when (buffer-live-p source)
        (kill-buffer source)))))


(ert-deftest douban-test-anthology-completion-caches-empty-list ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/status.md")
    (insert
     (concat
      "---\n"
      "douban:\n"
      "  status:\n"
      "    anthology-id:\n"
      "---\n\n"
      "广播正文\n"))
    (goto-char (point-min))
    (search-forward "anthology-id:")
    (let ((reads 0))
      (cl-letf
          (((symbol-function 'douban--current-user-id)
            (lambda () "current-user"))
           ((symbol-function
             'douban--read-browser-cookies)
            (lambda (&rest _arguments) nil))
           ((symbol-function 'douban--anthologies)
            (lambda (&rest _arguments)
              (cl-incf reads)
              nil)))
        (let* ((capf
                (douban-metadata-completion-at-point))
               (table (nth 2 capf)))
          (should capf)
          (should (= reads 0))
          (should-not (all-completions "" table))
          (should-not (all-completions "" table))
          (should (= reads 1)))))))

(ert-deftest douban-test-anthology-completion-keeps-unfinished-session ()
  (dolist (case '(("专题 # 一" sole) ("'引号标题" exact)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/status.md")
      (insert
       "---\ndouban:\n  status:\n    anthology-id:\n---\n")
      (goto-char (point-min))
      (search-forward "anthology-id:")
      (cl-letf
          (((symbol-function 'douban--current-user-id)
            (lambda () "current-user"))
           ((symbol-function
             'douban--read-browser-cookies)
            (lambda (&rest _arguments) nil))
           ((symbol-function 'douban--anthologies)
            (lambda (&rest _arguments)
              `((:id "42" :title ,(car case)
                :items-count 2)))))
        (let* ((capf
                (douban-metadata-completion-at-point))
               (start (nth 0 capf))
               (end (nth 1 capf))
               (table (nth 2 capf))
               (exit-function
                (plist-get
                 (nthcdr 3 capf)
                 :exit-function)))
          (should (member (car case) (all-completions "" table)))
          (delete-region start end)
          (goto-char start)
          (insert (car case))
          (funcall
           exit-function (car case) (cadr case))
          (let ((info (douban--metadata-context)))
            (should (eq (plist-get info :slot) 'value))
            (should
             (eq
              (plist-get info :field)
              :anthology-id))
            (should
             (equal
              (buffer-substring-no-properties
               (plist-get info :completion-start)
               (plist-get info :completion-end))
              (car case))))
          ;; 同一个 completion session 随后真正结束；未完成状态不能提前
          ;; 释放用于最终写回的 markers。
          (funcall exit-function (car case) 'finished)
          (should
           (string-match-p
            "^    anthology-id: '42'$"
            (buffer-string))))))))

(ert-deftest douban-test-anthology-completion-widens-narrowed-source ()
  (dolist
      (case
       '(("/tmp/status.md"
          "---\ndouban:\n  status:\n    anthology-id:\n---\n"
          "anthology-id:")))
    (with-temp-buffer
      (setq buffer-file-name (nth 0 case))
      (insert (nth 1 case))
      (goto-char (point-min))
      (search-forward (nth 2 case))
      (let ((point (point))
            (line-start (line-beginning-position))
            (line-end (line-end-position)))
        (narrow-to-region line-start line-end)
        (goto-char point)
        (should (douban-metadata-completion-at-point))))))

(ert-deftest douban-test-anthology-completion-contexts ()
  (dolist
      (case
       '(("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-id: '1'\n    subject-type: book\n    anthology-id:\n---\n")
         ("/tmp/status.md"
          "---\ndouban:\n  status: {}\nother:\n  anthology-id:\n---\n")
         ("/tmp/status.md"
          "---\ndouban:\n  status:\n    nested:\n      anthology-id:\n---\n")))
    (with-temp-buffer
      (setq buffer-file-name (car case))
      (insert (cadr case))
      (goto-char (point-min))
      (search-forward "anthology-id:")
      (should-not
       (douban-metadata-completion-at-point)))))

(ert-deftest douban-test-published-status-can-complete-anthology ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/status.md")
    (insert
     (concat
      "---\n"
      "douban:\n"
      "  status:\n"
      "    id: '703'\n"
      "    anthology-id:\n"
      "---\n\n"
      "广播正文\n"))
    (goto-char (point-min))
    (search-forward "anthology-id:")
    (should
     (douban-metadata-completion-at-point))))

(ert-deftest douban-test-douban-mode-has-no-global-auto-detection ()
  (should-not (fboundp 'douban--maybe-enable-mode))
  (should-not
   (and
    (boundp 'markdown-mode-hook)
    (memq #'douban-mode (symbol-value 'markdown-mode-hook)))))

(ert-deftest douban-test-douban-publish-does-not-require-douban-mode ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/status.md")
    (setq major-mode 'markdown-mode)
    (let ((meta '(:kind status :status-id nil))
          published)
      (cl-letf
          (((symbol-function 'douban--current-source)
            (lambda ()
              (should-not douban-mode)
              (list buffer-file-name meta)))
           ((symbol-function 'douban--publish-file)
            (lambda (file value)
              (setq published (list file value))
              'published)))
        (should (eq (douban-publish) 'published)))
      (should
       (equal
       published
       (list buffer-file-name meta))))))

(ert-deftest douban-test-annotation-entry-is-metadata-completion-only ()
  (should-not (fboundp 'douban-new-annotation))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/annotation.md")
    (insert "---\ndouban:\n  ann\n---\n")
    (goto-char (point-min))
    (search-forward "  ann")
    (let* ((capf (douban-metadata-completion-at-point))
           (candidates (all-completions "" (nth 2 capf))))
      (should capf)
      (should (member "annotation:" candidates))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/annotation.md")
    (insert "---\ndouban:\n  annotation:\n    s\n---\n")
    (goto-char (point-min))
    (search-forward "    s")
    (let* ((capf (douban-metadata-completion-at-point))
           (candidates (all-completions "" (nth 2 capf))))
      (should capf)
      (dolist
          (field
           '("id:" "subject-id:" "privacy:" "explanation-types:"))
        (should (member field candidates)))
      (should-not (member "original:" candidates))
      (should-not (member "reply-limit:" candidates))
      (should-not (member "subject-type:" candidates)))))

(ert-deftest douban-test-markdown-metadata-field-completion-is-kind-aware ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/status.md")
    (insert
     (concat
      "---\n"
      "douban:\n"
      "  status:\n"
      "    id: '703'\n"
      "    exp\n"
      "---\n"))
    (goto-char (point-min))
    (search-forward "    exp")
    (let* ((capf (douban-metadata-completion-at-point))
           (table (nth 2 capf))
           (properties (nthcdr 3 capf))
           (annotation
            (plist-get properties :annotation-function))
           (candidates (all-completions "" table)))
      (should capf)
      (should-not
       (plist-member properties :company-prefix-length))
      (should
       (equal
        (buffer-substring (nth 0 capf) (nth 1 capf))
        "exp"))
      (should (member "explanation-types:" candidates))
      (should-not (member "original:" candidates))
      (should-not (member "reply-limit:" candidates))
      (should-not (member "subject-type:" candidates))
      (should-not (member "id:" candidates))
      (should-not (member "url:" candidates))
      (should
       (equal
        (funcall annotation "explanation-types:")
        "  单项内容说明"))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/note.md")
    (insert "---\ndouban:\n  note:\n    p\n---\n")
    (goto-char (point-min))
    (search-forward "    p")
    (let ((candidates
           (all-completions
            ""
            (nth 2
                 (douban-metadata-completion-at-point)))))
      (should (member "privacy:" candidates))
      (should-not (member "url:" candidates))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/undecided.md")
    (insert "---\ndouban:\n  st\n---\n")
    (goto-char (point-min))
    (search-forward "  st")
    (let* ((capf (douban-metadata-completion-at-point))
           (candidates (all-completions "" (nth 2 capf))))
      (should capf)
      (should
       (equal
        candidates
        '("review:" "note:" "annotation:" "status:"))))))

(ert-deftest douban-test-global-settings-are-not-metadata-completion ()
  (dolist
      (case
       '(("/tmp/review.md"
          "---\ndouban:\n  review:\n    e\n---\n"
          "    e" "original:")
         ("/tmp/annotation.md"
          "---\ndouban:\n  annotation:\n    e\n---\n"
          "    e" "original:")
         ("/tmp/status.md"
          "---\ndouban:\n  status:\n    e\n---\n"
          "    e" "original:")
         ("/tmp/annotation.md"
          "---\ndouban:\n  annotation:\n    e\n---\n"
          "    e" "reply-limit:")
         ("/tmp/status.md"
          "---\ndouban:\n  status:\n    e\n---\n"
          "    e" "reply-limit:")))
    (with-temp-buffer
      (setq buffer-file-name (nth 0 case))
      (insert (nth 1 case))
      (goto-char (point-min))
      (search-forward (nth 2 case))
      (let* ((capf (douban-metadata-completion-at-point))
             (candidates
              (and capf (all-completions "" (nth 2 capf)))))
        (should capf)
        (should-not (member (nth 3 case) candidates))))))

(ert-deftest douban-test-review-field-completion-honors-subject-type ()
  (dolist
      (case
       '(("book" nil)
         ("game" t)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/review.md")
      (insert
       (format
        (concat
         "---\n"
         "douban:\n"
         "  review:\n"
         "    subject-id: '42'\n"
         "    subject-type: %s\n"
         "    r\n"
         "---\n")
        (car case)))
      (goto-char (point-min))
      (search-forward "    r")
      (let* ((capf
              (douban-metadata-completion-at-point))
             (annotation
              (plist-get
               (nthcdr 3 capf)
               :annotation-function))
             (candidates
              (all-completions "" (nth 2 capf))))
        (should (member "rating:" candidates))
        (should (member "explanation-types:" candidates))
        (should-not
         (funcall annotation "rating:"))
        (should
         (equal
          (funcall annotation "spoiler:")
          "  是否包含剧透"))
        (should-not (member "original:" candidates))
        (should-not (member "url:" candidates))
        (should
         (eq
          (and (member "rtype:" candidates) t)
          (cadr case)))
        (should
         (eq
          (and (member "platforms:" candidates) t)
          (cadr case)))))))

(ert-deftest douban-test-markdown-metadata-capf-builds-one-source-index ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/review.md")
    (insert
     (concat
      "---\n"
      "douban:\n"
      "  review:\n"
      "    subject-id: '42'\n"
      "    subject-type: game\n"
      "    r\n"
      "---\n"))
    (goto-char (point-min))
    (search-forward "    r")
    (let ((builder
           (symbol-function
            'douban--markdown-metadata-source-index))
          (scans 0))
      (cl-letf
          (((symbol-function
             'douban--markdown-metadata-source-index)
            (lambda (containers)
              (cl-incf scans)
              (funcall builder containers))))
        (let ((capf
               (douban-metadata-completion-at-point)))
          (should capf)
          (should
           (member
            "rtype:"
            (all-completions "" (nth 2 capf))))
          (should (= scans 1)))))))


(ert-deftest douban-test-metadata-capf-rebuilds-index-after-edit ()
  (dolist
      (case
       '(("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-id: '42'\n    subject-type: book\n    r\n---\n"
          "    r"
          "subject-type: book"
          "subject-type: game"
          "rtype:")))
    (with-temp-buffer
      (setq buffer-file-name (nth 0 case))
      (insert (nth 1 case))
      (goto-char (point-min))
      (search-forward (nth 2 case))
      (should-not
       (member
        (nth 5 case)
        (all-completions
         ""
         (nth
          2
          (douban-metadata-completion-at-point)))))
      (save-excursion
        (goto-char (point-min))
        (search-forward (nth 3 case))
        (replace-match (nth 4 case) t t))
      (should
       (member
        (nth 5 case)
        (all-completions
         ""
         (nth
          2
          (douban-metadata-completion-at-point))))))))


(ert-deftest douban-test-explanation-types-is-strict-single-source-metadata ()
  (dolist
      (value
       '((:review
          (:subject-id "1"
           :subject-type "book"
           :explanation-types "ai-generated"))
         (:status (:explanation-types "repost"))))
    (should
     (stringp
      (plist-get
       (douban--meta-from-plist value nil)
       :explanation-types))))
  (dolist (kind '(review status))
    (dolist
        (invalid
         (list
          '("ai-generated" "repost")
          ["ai-generated" "repost"]
          "ai-generated,repost"
          "A" "X" "K" "M" "P" "O" "R" "N"))
      (should-error
       (douban--meta-from-plist
        (list
         (douban--metadata-source-kind-key kind)
         (if (eq kind 'review)
             (list
              :subject-id "1"
              :subject-type "book"
              :explanation-types invalid)
           (list :explanation-types invalid)))
        nil)
       :type 'error)))
  (dolist
      (case
       '((".md"
          "---\ndouban:\n  review:\n    subject-id: '1'\n    subject-type: book\n    explanation-types: ai-generated\n---\n"
          "ai-generated")
         (".md"
          "---\ndouban:\n  status:\n    explanation-types: repost\n---\n"
          "repost")))
    (douban-test--with-temp-file
        (nth 0 case) (nth 1 case)
      (let ((meta (douban--read-meta file)))
        (should
         (equal
          (plist-get meta :explanation-types)
          (nth 2 case)))
        (douban--write-meta file meta))
      (should
       (equal
        (plist-get
         (douban--read-meta file)
         :explanation-types)
        (nth 2 case)))))
  (dolist
      (case
       '((".md"
          "---\ndouban:\n  review:\n    subject-id: '1'\n    subject-type: book\n    explanation-types: [ai-generated, repost]\n---\n")
         (".md"
          "---\ndouban:\n  status:\n    explanation-types: [ai-generated, repost]\n---\n")))
    (douban-test--with-temp-file
        (car case) (cadr case)
      (should-error
       (douban--read-meta file)
       :type 'error))))

(ert-deftest douban-test-markdown-explanation-value-completion-is-readable ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/status.md")
    (insert
     (concat
      "---\n"
      "douban:\n"
      "  status:\n"
      "    explanation-types: ai\n"
      "---\n"))
    (goto-char (point-min))
    (search-forward "explanation-types: ai")
    (let* ((capf (douban-metadata-completion-at-point))
           (start (nth 0 capf))
           (end (nth 1 capf))
           (properties (nthcdr 3 capf))
           (annotation
            (plist-get properties :annotation-function))
           (exit-function
            (plist-get properties :exit-function))
           (candidates
            (all-completions "" (nth 2 capf))))
      (should capf)
      (dolist
          (candidate
           '("ai-generated" "fictional" "marketing" "minor-safety"
             "public-affairs" "personal-opinion" "repost" "none"))
        (should (member candidate candidates)))
      (dolist
          (case
           '(("ai-generated" . "  含 AI 生成内容")
             ("fictional" . "  含虚构内容")
             ("marketing" . "  含营销信息")
             ("minor-safety" .
              "  含或影响未成年人身心健康信息")
             ("public-affairs" .
              "  涉及时事、公共政策、社会事件")
             ("personal-opinion" .
              "  个人观点仅供参考")
             ("repost" . "  内容为转载，来源见正文")
             ("none" . "  无需标注")))
        (should
         (equal
          (funcall annotation (car case))
          (cdr case))))
      (delete-region start end)
      (goto-char start)
      (insert "ai-generated")
      (funcall exit-function "ai-generated" 'finished)
      (should
       (string-match-p
        "^    explanation-types: ai-generated$"
        (buffer-string)))
      (should
       (equal
        (plist-get
         (douban--current-buffer-meta)
         :explanation-types)
        "ai-generated")))))

(ert-deftest douban-test-markdown-rating-value-has-no-redundant-annotation ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/review.md")
    (insert
     (concat
      "---\n"
      "douban:\n"
      "  review:\n"
      "    subject-id: '42'\n"
      "    subject-type: book\n"
      "    rating: 1\n"
      "---\n"))
    (goto-char (point-min))
    (search-forward "rating: 1")
    (let* ((capf
            (douban-metadata-completion-at-point))
           (annotation
            (plist-get
             (nthcdr 3 capf)
             :annotation-function)))
      (should capf)
      (should
       (member "1" (all-completions "" (nth 2 capf))))
      (should-not
       (funcall annotation "1")))))

(ert-deftest douban-test-empty-static-metadata-value-overrides-auto-prefix ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/review.md")
    (insert
     (concat
      "---\n"
      "douban:\n"
      "  review:\n"
      "    subject-type:\n"
      "---\n"))
    (goto-char (point-min))
    (search-forward "subject-type:")
    (let* ((capf (douban-metadata-completion-at-point))
           (properties (nthcdr 3 capf)))
      (should capf)
      (should (= (nth 0 capf) (nth 1 capf)))
      (should
       (eq
        (plist-get properties :company-prefix-length)
        t))
      (should
       (equal
        (all-completions "" (nth 2 capf))
        '("book" "movie" "tv" "music" "game"))))))


(ert-deftest douban-test-markdown-metadata-value-completion-normalizes-quotes ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/annotation.md")
    (insert
     (concat
      "---\n"
      "douban:\n"
      "  annotation:\n"
      "    subject-id: '1'\n"
      "    privacy: 'pri'\n"
      "---\n"))
    (goto-char (point-min))
    (search-forward "privacy: 'pri")
    (let* ((capf (douban-metadata-completion-at-point))
           (start (nth 0 capf))
           (end (nth 1 capf))
           (exit-function
            (plist-get (nthcdr 3 capf) :exit-function)))
      (should
       (equal
        (buffer-substring start end)
        "pri"))
      (delete-region start end)
      (goto-char start)
      (insert "private")
      (funcall exit-function "private" 'finished)
      (should
       (string-match-p
        "^    privacy: private$"
        (buffer-string))))))

(ert-deftest douban-test-static-metadata-completion-rejects-wrong-kind ()
  (dolist
      (case
       '(("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-id: '1'\n    subject-type: book\n    privacy: f\n---\n"
          "privacy: f")
         ("/tmp/status.md"
          "---\ndouban:\n  status:\n    nested:\n      explanation-types: f\n---\n"
          "explanation-types: f")
         ("/tmp/status.md"
          "---\ndouban:\n  status: {}\nother:\n  explanation-types: f\n---\n"
          "explanation-types: f")))
    (with-temp-buffer
      (setq buffer-file-name (nth 0 case))
      (insert (nth 1 case))
      (goto-char (point-min))
      (search-forward (nth 2 case))
      (should-not
       (douban-metadata-completion-at-point)))))



(ert-deftest douban-test-protocol-letters-are-not-source-metadata-values ()
  (dolist
      (value
       '((:review
          (:subject-id "1" :subject-type "game" :rtype "R"))
         (:note (:privacy "P"))))
    (should-error
     (douban--meta-from-plist value nil)
     :type 'error)))

(ert-deftest douban-test-published-review-and-note-use-id-alone ()
  (let ((review
         (douban--meta-from-plist
          '(:review
            (:subject-id "9" :subject-type "book" :id "1"))
          nil))
        (note
         (douban--meta-from-plist
          '(:note (:id "2"))
          nil)))
    (should (equal (plist-get review :review-id) "1"))
    (should (equal (plist-get note :note-id) "2"))))

(ert-deftest douban-test-published-status-uses-topic-id-as-id ()
  (let ((published '(:status (:id "303"))))
    (should
     (equal
      (plist-get
       (douban--meta-from-plist published nil)
       :status-id)
      "303")))
  (dolist
      (obsolete
       '((:status (:topic-id "303"))
         (:status (:id "303" :topic-id "303"))))
    (should-error
     (douban--meta-from-plist obsolete nil)
     :type 'error)))

(ert-deftest douban-test-markdown-all-kinds-roundtrip ()
  (dolist
      (case
       `((review
          ,(concat
            "---\n"
            "title: '评论'\n"
            "douban:\n"
            "  review:\n"
            "    subject-id: '1'\n"
            "    subject-type: book\n"
            "    id: '11'\n"
            "---\n\n正文\n")
          :review-id "11")
         (note
          ,(concat
            "---\n"
            "title: '日记'\n"
            "douban:\n"
            "  note:\n"
            "    id: '22'\n"
            "    privacy: 'friends'\n"
            "    cannot-reply: true\n"
            "    author-tags: ['随笔', '生活']\n"
            "---\n\n正文\n")
          :note-id "22")
         (status
         ,(concat
            "---\n"
            "douban:\n"
            "  status:\n"
            "    id: '303'\n"
            "---\n\n广播正文\n")
          :status-id "303")))
    (douban-test--with-temp-file
        ".md" (nth 1 case)
      (let ((meta (douban--read-meta file)))
        (should (eq (plist-get meta :kind) (car case)))
        (should
         (equal
          (plist-get meta (nth 2 case))
          (nth 3 case)))
        (when (eq (car case) 'note)
          (should (equal (plist-get meta :note-privacy) "friends"))
          (should (plist-get meta :cannot-reply))
          (should
           (equal
            (plist-get meta :author-tags)
            '("随笔" "生活"))))
        (douban--write-meta file meta)
        (let ((roundtrip (douban--read-meta file))
              (text
               (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))
          (should (eq (plist-get roundtrip :kind) (car case)))
          (should
           (equal
            (plist-get roundtrip (nth 2 case))
            (nth 3 case)))
          (should
           (string-match-p
            (format "^  %s:$" (car case))
            text))
          (should-not
           (string-match-p
            "^  \\(?:review-id\\|note-id\\|status-id\\|kind\\):"
            text)))))))


(ert-deftest douban-test-omitted-id-branches-roundtrip ()
  (dolist
      (case
       '((".md"
          review
          :review-id
          "---\ntitle: ''\ndouban:\n  review:\n    subject-id: '1'\n    subject-type: book\n---\n\n"
          "^  review:$"
          "^    id:")
         (".md"
          note
          :note-id
          "---\ntitle: ''\ndouban:\n  note: {}\n---\n\n"
          "^  note: {}$"
          "^    id:")
         (".md"
          status
          :status-id
          "---\ndouban:\n  status: {}\n---\n\n"
          "^  status: {}$"
          "^    id:")))
    (douban-test--with-temp-file
        (nth 0 case) (nth 3 case)
      (let ((meta (douban--read-meta file)))
        (should (eq (plist-get meta :kind) (nth 1 case)))
        (should-not (plist-member meta (nth 2 case)))
        (should-not (plist-get meta (nth 2 case)))
        (douban--write-meta file meta))
      (let ((roundtrip (douban--read-meta file))
            (text
             (with-temp-buffer
               (insert-file-contents file)
               (buffer-string))))
        (should (eq (plist-get roundtrip :kind) (nth 1 case)))
        (should-not
         (plist-member roundtrip (nth 2 case)))
        (should-not (plist-get roundtrip (nth 2 case)))
        (should (string-match-p (nth 4 case) text))
        (should-not (string-match-p (nth 5 case) text))
        (should-not (string-match-p "\\_<kind\\_>\\|DOUBAN_KIND" text))))))

(ert-deftest douban-test-source-metadata-rejects-legacy-flat-layouts ()
  (dolist
      (case
       '((".md"
          "---\ntitle: '评论'\ndouban:\n  kind: review\n  subject-id: '1'\n  subject-type: book\n---\n")
         (".md"
          "---\ndouban:\n  status-id: '1'\n---\n")))
    (douban-test--with-temp-file
        (nth 0 case) (nth 1 case)
      (should-error (douban--read-meta file) :type 'error))))

(ert-deftest douban-test-status-explanation-metadata-normalizes-explicit-field ()
  (let ((explicit
         (douban--meta-from-plist
          '(:status (:explanation-types "none"))
          nil))
        (omitted
         (douban--meta-from-plist '(:status nil) nil)))
    (should (plist-member explicit :explanation-types))
    (should
     (equal
      (plist-get explicit :explanation-types)
      "none"))
    (should-not (plist-member omitted :explanation-types)))
  (dolist
      (value
       '("ai-generated" "fictional" "marketing" "minor-safety"
         "public-affairs" "personal-opinion" "repost" "none"))
    (should
     (equal
      (plist-get
       (douban--meta-from-plist
        (list
         :status
         (list :explanation-types value))
        nil)
       :explanation-types)
      value))))

(ert-deftest douban-test-status-private-metadata-is-unknown ()
  (should-error
   (douban--meta-from-plist
    '(:status (:private "true"))
    nil)
   :type 'error))

(ert-deftest douban-test-status-explanation-metadata-roundtrips ()
  (dolist
      (case
       (list
        (list
         ".md"
         (concat
          "---\n"
          "douban:\n"
          "  status:\n"
          "    explanation-types: none\n"
          "---\n\n广播正文\n")
         (concat
          "---\n"
          "douban:\n"
          "  status: {}\n"
          "---\n\n广播正文\n")
         "^    explanation-types: none$"
         "^    explanation-types:")))
    (douban-test--with-temp-file
        (nth 0 case) (nth 1 case)
      (let ((meta (douban--read-meta file)))
        (should
         (equal (plist-get meta :explanation-types) "none"))
        (douban--write-meta file meta))
      (let ((text
             (with-temp-buffer
               (insert-file-contents file)
               (buffer-string)))
            (roundtrip (douban--read-meta file)))
        (should (string-match-p (nth 3 case) text))
        (should (plist-member roundtrip :explanation-types))
        (should
         (equal
          (plist-get roundtrip :explanation-types)
          "none"))))
    (douban-test--with-temp-file
        (nth 0 case) (nth 2 case)
      (let ((meta (douban--read-meta file)))
        (should-not (plist-member meta :explanation-types))
        (douban--write-meta file meta))
      (let ((text
             (with-temp-buffer
               (insert-file-contents file)
               (buffer-string)))
            (roundtrip (douban--read-meta file)))
        (should-not (string-match-p (nth 4 case) text))
        (should-not
         (plist-member roundtrip :explanation-types))))))

(ert-deftest douban-test-new-review-creates-parents-and-refuses-existing-files ()
  (let* ((suffix ".md")
         (directory
            (make-temp-file "douban-new-review-" t))
           (file
            (expand-file-name
             (concat "尚不存在的父目录/不应成为标题" suffix)
             directory)))
      (unwind-protect
          (cl-letf
              (((symbol-function 'douban--review-editor-session)
                (lambda (&rest _args)
                  (error "new command must stay offline")))
               ((symbol-function 'douban--read-browser-cookies)
                (lambda (&rest _args)
                  (error "new command must stay offline")))
               ((symbol-function 'tab-new) #'ignore))
            (douban-new-review
             "book"
             "https://book.douban.com/subject/123/"
             file)
            (should (equal (buffer-file-name) file))
            (should douban-mode)
            (let* ((text
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
                   (meta (douban--read-meta file)))
              (should (eq (plist-get meta :kind) 'review))
              (should-not
               (string-match-p "^[ \t]+kind:" text))
              (should-not
               (string-match-p
                (regexp-quote "不应成为标题")
                text))
              (should (string-match-p "^title: ''$" text))
              (should
               (equal (plist-get meta :subject-id) "123"))
              (should
               (equal (plist-get meta :subject-type) "book"))
              (should
               (string-match-p "^    subject-id: '123'$" text))
              (should-not (plist-member meta :review-id))
              (should-not (plist-get meta :review-id))
              (should-not (string-match-p "^    id:" text))
              (should-error
               (douban--create-source-file file meta)
               :type 'file-already-exists)))
        (when-let* ((buffer (find-buffer-visiting file)))
          (kill-buffer buffer))
        (ignore-errors (delete-directory directory t)))))

(ert-deftest douban-test-new-review-lisp-entry-requires-url ()
  (should-error
   (douban-new-review
    "book"
    '(:subject-id "123" :subject-type "book")
    "/tmp/not-created.md")
   :type 'user-error)
  (should-error
   (douban-new-review
    "podcast"
    "https://book.douban.com/subject/123/"
    "/tmp/not-created.md")
   :type 'user-error)
  (should-error
   (douban-new-review
    "music"
    "https://book.douban.com/subject/123/"
   "/tmp/not-created.md")
   :type 'user-error))

(ert-deftest douban-test-cookie-header-removes-syntactic-outer-quotes ()
  (should
   (equal
    (douban--cookie-header
     '(("dbcl2" . "\"123456:login-token\"")
       ("ck" . "plain-ck")
       ("bid" . "\"quoted-bid\"")
       ("leading" . "\"not-closed")
       ("trailing" . "not-opened\"")))
    (concat
     "dbcl2=123456:login-token; ck=plain-ck; bid=quoted-bid; "
     "leading=\"not-closed; trailing=not-opened\"")))
  (should-not (douban--cookie-header nil)))

(ert-deftest douban-test-http-mine-redirect-refreshes-ck ()
  (let (captured)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method url &rest options)
            (setq captured (list method url options))
            (make-plz-response
             :status 302
             :headers
             '((Location . "/people/alice/")
               (Set-Cookie . "ck=fresh-ck; Path=/")
               (Set-Cookie . "theme=green; Path=/"))
             :body "not followed"))))
      (let* ((session
              (douban--make-session
               :cookies
               '(("dbcl2" . "\"123456:login-token\"")
                 ("bid" . "\"quoted-bid\""))))
             (response
              (douban--http
               "GET" douban--ck-bootstrap-url
               :session session
               :extra-headers
               '(("Accept" . "text/html")
                 ("Cache-Control" . "no-cache"))
               :allow-redirect-response t)))
        (should (= (plist-get response :status) 302))
        (should
         (equal
          (plist-get response :headers)
          '(("location" . "/people/alice/")
            ("set-cookie" . "ck=fresh-ck; Path=/")
            ("set-cookie" . "theme=green; Path=/"))))
        (should (equal (plist-get response :body) "not followed"))
        (should (equal (douban--session-ck session) "fresh-ck"))
        (should
         (equal
          (cdr (assoc-string
                "theme" (douban--session-cookies session)))
          "green"))))
    (pcase-let ((`(,method ,url ,options) captured))
      (should (equal method "GET"))
      (should (equal url douban--ck-bootstrap-url))
      (should-not (plist-get options :body))
      (let ((headers (plist-get options :headers)))
        (should
         (equal
          (cdr (assoc-string "Cookie" headers t))
          "dbcl2=123456:login-token; bid=quoted-bid"))
        (should
         (equal
          (cdr (assoc-string "Accept" headers t))
          "text/html"))
        (should
         (equal
          (cdr (assoc-string "Cache-Control" headers t))
          "no-cache"))))))

(ert-deftest douban-test-http-keeps-non-2xx-response-and-updates-cookie ()
  (let (captured)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method url &rest options)
            (setq captured (list method url options))
            (make-plz-response
             :status 403
             :headers
             '(("Content-Type" . "text/plain")
               ("Set-Cookie" . "ck=new-token; Path=/"))
             :body "forbidden"))))
      (let* ((session
              (douban--make-session
               :cookies '(("ck" . "old-token"))
               :ck "old-token"))
             (response
              (douban--http
               "GET" "https://www.douban.com/" :session session)))
        (should (= (plist-get response :status) 403))
        (should
         (equal
          (plist-get response :headers)
          '(("content-type" . "text/plain")
            ("set-cookie" . "ck=new-token; Path=/"))))
        (should (equal (plist-get response :body) "forbidden"))
        (should (equal (douban--session-ck session) "new-token"))))
    (should
     (equal
      (cl-subseq captured 0 2)
      '("GET" "https://www.douban.com/")))))

(ert-deftest douban-test-plz-request-preserves-transport-error ()
  (let ((data
         (make-plz-error
          :curl-error '(7 . "Could not connect to server"))))
    (cl-letf
        (((symbol-function 'plz)
          (lambda (&rest _arguments)
            (signal
             'plz-curl-error
             (list "Curl error" data)))))
      (let ((caught
             (should-error
              (douban--plz-request
               "GET" "https://www.douban.com/")
              :type 'plz-curl-error)))
        (should (eq (car caught) 'plz-curl-error))
        (should (equal (cadr caught) "Curl error"))
        (should (eq (caddr caught) data))))))

(ert-deftest douban-test-http-preserves-plz-transport-error ()
  (cl-letf
      (((symbol-function 'douban--plz-request)
        (lambda (&rest _arguments)
          (signal 'plz-curl-error '("connection refused")))))
    (let ((caught
           (condition-case err
               (progn
                 (douban--http "GET" "https://www.douban.com/")
                 nil)
             (plz-curl-error err))))
      (should (eq (car caught) 'plz-curl-error))
      (should (equal (cadr caught) "connection refused")))))

(ert-deftest douban-test-http-json-and-binary-request-bodies ()
  (let ((json-text "{\"text\":\"中文😀\"}")
        (binary (unibyte-string 0 137 255 10))
        requests)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method url &rest options)
            (push (list method url options) requests)
            (make-plz-response
             :status 200
             :headers
             '(("Content-Type" .
                "application/json; charset=utf-8"))
             :body "{\"ok\":true,\"message\":\"收到😀\"}"))))
      (let ((response
             (douban--http-json
              "POST" "https://m.douban.com/rexxar/api/test"
              :body json-text
              :content-type "application/json;charset=utf-8")))
        (should (eq (plist-get (plist-get response :json) :ok) t))
        (should
         (equal
          (plist-get (plist-get response :json) :message)
          "收到😀")))
      (douban--http
       "POST" "https://www.douban.com/upload"
       :body binary
       :content-type "application/octet-stream"
       :raw-body t))
    (pcase-let*
        ((`((,_ ,_ ,binary-options)
            (,_ ,_ ,json-options))
          requests)
         (json-body (plist-get json-options :body))
         (binary-body (plist-get binary-options :body)))
      (should
       (equal
        json-body
        (encode-coding-string json-text 'utf-8 t)))
      (should-not (multibyte-string-p json-body))
      (should (equal binary-body binary))
      (should-not (multibyte-string-p binary-body)))))

(ert-deftest douban-test-read-json-endpoint-uses-unified-http-contract ()
  (let (requests)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method url &rest options)
            (push (list method url options) requests)
            (make-plz-response
             :status
             (if (string-suffix-p "/invalid" url) 503 200)
             :headers '(("Content-Type" . "application/json"))
             :body
             (if (string-suffix-p "/invalid" url)
                 "not-json"
               "{\"ok\":true,\"message\":\"统一解码\"}")))))
      (let ((response
             (douban--read-json-endpoint
              "https://m.douban.com/rexxar/api/v2/test"
              "https://www.douban.com/source"
              :cookies
              '(("dbcl2" . "\"123:login\"")
                ("ck" . "page-ck")))))
        (should (= (plist-get response :status) 200))
        (should (eq (plist-get (plist-get response :json) :ok) t))
        (should
         (equal
          (plist-get (plist-get response :json) :message)
          "统一解码")))
      (let ((response
             (douban--read-json-endpoint
              "https://m.douban.com/rexxar/api/v2/invalid"
              "https://www.douban.com/source")))
        (should (= (plist-get response :status) 503))
        (should (equal (plist-get response :body) "not-json"))
        (should-not (plist-get response :json))))
    (setq requests (nreverse requests))
    (should (= (length requests) 2))
    (pcase-let ((`(,method ,url ,options) (car requests)))
      (should (equal method "GET"))
      (should
       (equal url "https://m.douban.com/rexxar/api/v2/test"))
      (should-not (plist-get options :body))
      (let ((headers (plist-get options :headers)))
        (should
         (equal
          (cdr (assoc-string "Accept" headers t))
          "application/json"))
        (should
         (equal
          (cdr (assoc-string "Referer" headers t))
          "https://www.douban.com/source"))
        (should
         (equal
          (cdr (assoc-string "Cookie" headers t))
          "dbcl2=123:login; ck=page-ck"))
        (should
         (equal
          (cdr (assoc-string "User-Agent" headers t))
          douban-user-agent))
        (should
         (equal
          (cdr (assoc-string "Accept-Language" headers t))
          "zh-CN,zh;q=0.9,en;q=0.8"))))
    (should-not
     (assoc-string
      "Cookie"
      (plist-get (nth 2 (cadr requests)) :headers)
      t))))


(ert-deftest douban-test-json-endpoint-callers-keep-non-2xx-errors ()
  (cl-letf
      (((symbol-function 'douban--read-json-endpoint)
        (lambda (&rest _arguments)
          '(:status 503 :body "unavailable" :json nil))))
    (dolist
        (case
         `((,(lambda ()
               (douban--resolve-card
                "https://example.org/article"))
            . "douban: 无法解析卡片 https://example.org/article（HTTP 503）")
           (,(lambda ()
               (douban--anthologies "alice"))
            . "douban: 文集列表读取失败（HTTP 503）")
           (,(lambda ()
               (douban--search-subjects "局外人" "book"))
            . "douban: 条目搜索失败（HTTP 503）")
           (,(lambda ()
               (douban--game-platforms "123"))
            . "douban: 读取游戏平台失败（HTTP 503）")))
      (let ((err
             (should-error
              (funcall (car case))
              :type 'error)))
        (should
         (equal
          (error-message-string err)
          (cdr case)))))))

(ert-deftest douban-test-browser-session-binds-one-cookie-scope ()
  (let ((api-url douban--topic-post-endpoint)
        (page-url douban--status-home-url)
        calls)
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (push url calls)
            (cond
             ((equal url api-url)
              '(("api-cookie" . "m-only")
                ("ck" . "api-ck")))
             ((equal url page-url)
              '(("www-cookie" . "www-only")
                ("ck" . "stale-ck")))
             (t
              (ert-fail
               (format "unexpected cookie URL: %s" url)))))))
      (let ((api-session
             (douban--browser-session 'status api-url))
            (page-session
             (douban--browser-session
              'status page-url "shared-ck")))
        (should
         (equal
          (nreverse calls)
          (list api-url page-url)))
        (should (eq (douban--session-kind api-session) 'status))
        (should (equal (douban--session-referer api-session) api-url))
        (should (equal (douban--session-host api-session) "m.douban.com"))
        (should (equal (douban--session-ck api-session) "api-ck"))
        (should
         (equal
          (douban--session-cookies api-session)
          '(("api-cookie" . "m-only")
            ("ck" . "api-ck"))))
        (should-not (eq api-session page-session))
        (should-not
         (eq
          (douban--session-cookies api-session)
          (douban--session-cookies page-session)))
        (should (equal (douban--session-referer page-session) page-url))
        (should (equal (douban--session-host page-session) "www.douban.com"))
        (should (equal (douban--session-ck page-session) "shared-ck"))
        (should
         (equal
          (douban--session-cookies page-session)
          '(("ck" . "shared-ck")
            ("www-cookie" . "www-only"))))))))

(ert-deftest douban-test-cookie-session-keeps-persisted-ck-without-get ()
  (let (cookie-url)
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (setq cookie-url url)
            '(("ck" . "cookie-ck")
              ("dbcl2" . "login"))))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (ert-fail "已有 ck 时不应读取 bootstrap 页面"))))
      (let ((session
             (douban--cookie-session
              'status douban--topic-post-endpoint)))
        (should (equal cookie-url douban--topic-post-endpoint))
        (should (eq (douban--session-kind session) 'status))
        (should (equal (douban--session-ck session) "cookie-ck"))
        (should
         (equal
          (douban--session-referer session)
          douban--topic-post-endpoint))
        (should (equal (douban--session-host session) "m.douban.com"))
        (should
         (equal
          (douban--session-cookies session)
          '(("ck" . "cookie-ck")
            ("dbcl2" . "login"))))))))

(ert-deftest douban-test-ensure-ck-reads-bootstrap-once ()
  (let (cookie-urls http-calls)
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (push url cookie-urls)
            (if (equal url douban--topic-post-endpoint)
                '(("ck" . "deleted")
                  ("api-cookie" . "m-only"))
              '(("dbcl2" . "login")
                ("www-cookie" . "www-only")))))
         ((symbol-function 'douban--http)
          (lambda (method url &rest arguments)
            (push (list method url arguments) http-calls)
            (let ((bootstrap (plist-get arguments :session)))
              (douban--response-set-cookie
               bootstrap
               '(("set-cookie" . "ck=fresh-ck; Path=/"))))
            '(:status 302
              :headers
              (("location" . "/people/alice/")
               ("set-cookie" . "ck=fresh-ck; Path=/"))
              :body ""))))
      (let ((session
             (douban--cookie-session
              'status douban--topic-post-endpoint)))
        (should (equal (douban--session-ck session) "fresh-ck"))
        (should
         (equal
          (douban--session-cookies session)
          '(("ck" . "fresh-ck")
            ("api-cookie" . "m-only"))))
        (should (equal (douban--session-host session) "m.douban.com"))))
    (should
     (equal
      (nreverse cookie-urls)
      (list douban--topic-post-endpoint douban--ck-bootstrap-url)))
    (should (= (length http-calls) 1))
    (pcase-let
        ((`(,method ,url ,arguments) (car http-calls)))
      (should (equal method "GET"))
      (should (equal url douban--ck-bootstrap-url))
      (should (plist-get arguments :allow-redirect-response))
      (let ((bootstrap (plist-get arguments :session)))
        (should
         (eq (douban--session-kind bootstrap) 'bootstrap))
        (should
         (equal
          (douban--session-referer bootstrap)
          douban--ck-bootstrap-url))
        (should (equal (douban--session-host bootstrap)
                       "www.douban.com"))
        (should
         (equal
          (cdr
           (assoc-string
            "www-cookie" (douban--session-cookies bootstrap)))
          "www-only"))))))

(ert-deftest douban-test-ensure-ck-rejects-missing-or-deleted-cookie ()
  (dolist
      (set-cookie
       '(nil
         ("set-cookie" . "ck=; Path=/")
         ("set-cookie" . "ck=deleted; Path=/")))
    (let ((http-count 0))
      (cl-letf
          (((symbol-function 'douban--read-browser-cookies)
            (lambda (_url) '(("dbcl2" . "expired"))))
           ((symbol-function 'douban--http)
            (lambda (_method _url &rest arguments)
              (cl-incf http-count)
              (when set-cookie
                (douban--response-set-cookie
                 (plist-get arguments :session)
                 (list set-cookie)))
              (list
               :status 302
               :headers (and set-cookie (list set-cookie))
               :body ""))))
        (should-error
         (douban--cookie-session
          'status douban--topic-post-endpoint)
         :type 'user-error))
      (should (= http-count 1)))))

(defun douban-test--annotation-edit-html
    (annotation-id subject-id &optional subtype)
  "返回 ANNOTATION-ID、SUBJECT-ID 对应的读书笔记编辑页 HTML。"
  (let ((topic
         (list
          :id annotation-id
          :subtype (or subtype "annotation")
          :subject_id subject-id
          :reply_limit "F"
          :accessible "public"
          :interest_tags [(:name "文学") (:name "随笔")]
          :explanation_types ["O"]
          :is_original t
          :video_info :json-null
          :image_layout "horizontal"
          :anthology_id "82"))
        (photos [(:id "42" :seq_id "9")
                 (:id 99 :seq_id 3)]))
    (format
     (concat
      "<script>\n__INIT_STATE__.topic = %s;\n"
      "__INIT_STATE__.topic.photos = %s;\n</script>")
     (json-serialize
      topic :null-object :json-null :false-object :json-false)
     (json-serialize
      photos :null-object :json-null :false-object :json-false))))

(ert-deftest douban-test-annotation-subject-preflight-requires-matching-book ()
  (let* ((session
          (douban--make-session
           :kind 'annotation
           :referer (douban--annotation-create-url "123")))
         request
         (response '(:status 200 :json (:id "123" :type "book"))))
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (setq request (list method url arguments))
            response)))
      (should
       (equal
        (douban--annotation-require-subject session "123")
        '(:id "123" :type "book")))
      (pcase-let ((`(,method ,url ,arguments) request))
        (should (equal method "GET"))
        (should
         (equal
          url
          "https://m.douban.com/rexxar/api/v2/book/123"))
        (should (eq (plist-get arguments :session) session)))
      (dolist
          (invalid
           '((:status 200 :json (:id "124" :type "book"))
             (:status 200 :json (:id "123" :type "movie"))
             (:status 404 :json (:id "123" :type "book"))))
        (setq response invalid)
        (should-error
         (douban--annotation-require-subject session "123")
         :type 'user-error)))))

(ert-deftest douban-test-annotation-edit-state-validates-topic-identity ()
  (let* ((meta
          '(:kind annotation
            :annotation-id "456"
            :subject-id "123"))
         (state
          (douban--annotation-edit-state
           (douban-test--annotation-edit-html "456" "123")
           meta)))
    (should (equal (plist-get state :reply-limit) "F"))
    (should (equal (plist-get state :accessible) "public"))
    (should (equal (plist-get state :interest-tags) "文学#随笔"))
    (should (equal (plist-get state :explanation-types) "O"))
    (should (eq (plist-get state :original) t))
    (should (eq (plist-get state :video-info) :json-null))
    (should (equal (plist-get state :image-layout) "horizontal"))
    (should (equal (plist-get state :anthology-id) "82"))
    (should (= (length (plist-get state :photos)) 2)))
  (dolist
      (html
       (list
        (douban-test--annotation-edit-html "457" "123")
        (douban-test--annotation-edit-html "456" "124")
        (douban-test--annotation-edit-html "456" "123" "personal")))
    (should-error
     (douban--annotation-edit-state
      html
      '(:kind annotation
        :annotation-id "456"
        :subject-id "123"))
     :type 'user-error)))

(ert-deftest douban-test-annotation-hobbit-tag-is-best-effort ()
  (let ((session
         (douban--make-session
          :kind 'annotation
          :referer (douban--annotation-create-url "123")))
        response)
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (&rest _arguments)
            (if (eq response 'error)
                (error "offline")
              response))))
      (setq response '(:status 200 :json (:hobbit_name "共读活动")))
      (should
       (equal
        (douban--annotation-hobbit-tag session "123")
        "共读活动"))
      (dolist
          (invalid
           '(error
             (:status 500 :json (:hobbit_name "共读活动"))
             (:status 200 :json (:hobbit_name ""))
             (:status 200 :json nil)))
        (setq response invalid)
        (should-not
         (douban--annotation-hobbit-tag session "123"))))))

(ert-deftest douban-test-annotation-create-body-matches-current-topic-editor ()
  (let* ((raw (douban-test--status-raw "第一段" "第二段"))
         (meta
          '(:kind annotation
            :subject-id "123"
            :title "读书笔记"))
         (payload
          (json-parse-string
           (douban--annotation-request-body raw meta)
           :object-type 'plist
           :array-type 'array
           :null-object :json-null
           :false-object :json-false))
         (content
          (json-parse-string
           (plist-get payload :content)
           :object-type 'plist
           :array-type 'array
           :null-object :json-null
           :false-object :json-false)))
    (should (equal (plist-get payload :title) "读书笔记"))
    (should (equal (plist-get payload :subject_id) "123"))
    (should (equal (plist-get payload :subtype) "annotation"))
    (should (equal (plist-get payload :accessible) "public"))
    (should (equal (plist-get payload :reply_limit) "A"))
    (should (equal (plist-get payload :explanation_types) ""))
    (should (equal (plist-get payload :image_ids) ""))
    (should (equal (plist-get payload :topic_tag_ids) ""))
    (should (equal (plist-get payload :interest_tags) ""))
    (should (eq (plist-get payload :send_status) :json-false))
    (should (eq (plist-get payload :original) t))
    (should (eq (plist-get payload :is_event) :json-false))
    (should (eq (plist-get payload :is_activity_rule) :json-false))
    (should (eq (plist-get payload :enable_item_tag) :json-false))
    (dolist (field '(:id :group_id :video_info
                     :image_layout :anthology_id :hobbit_tag))
      (should-not (plist-member payload field)))
    (should
     (equal
      (mapcar
       (lambda (block) (plist-get block :text))
       (append (plist-get content :blocks) nil))
      '("第一段" "第二段"))))
  (let* ((douban-review-send-broadcast t)
         (douban-default-original nil)
         (raw (douban-test--status-raw "正文"))
         (payload
          (json-parse-string
           (douban--annotation-request-body
            raw
            '(:kind annotation
              :subject-id "123"
              :title "标题"
              :explanation-types "none")
            '(:hobbit-tag "共读活动"))
           :object-type 'plist
           :false-object :json-false)))
    (should (eq (plist-get payload :original) :json-false))
    (should (eq (plist-get payload :send_status) t))
    (should (equal (plist-get payload :explanation_types) "N"))
    (should (equal (plist-get payload :hobbit_tag) "共读活动"))))

(ert-deftest douban-test-annotation-create-reply-limit-follows-global-setting ()
  (let ((raw (douban-test--status-raw "正文")))
    (dolist
        (case
         '((all "A")
           (following "F")))
      (let* ((douban-default-reply-limit (nth 0 case))
             (payload
              (json-parse-string
               (douban--annotation-request-body
                raw
                '(:kind annotation
                  :subject-id "123"
                  :title "标题"
                  :annotation-privacy "public"))
               :object-type 'plist
               :false-object :json-false)))
        (should (equal (plist-get payload :accessible) "public"))
        (should (equal (plist-get payload :reply_limit) (nth 1 case)))))
    (let* ((douban-default-reply-limit 'following)
           (private
            (json-parse-string
             (douban--annotation-request-body
              raw
              '(:kind annotation
                :subject-id "123"
                :title "标题"
                :annotation-privacy "private"))
             :object-type 'plist
             :false-object :json-false)))
      (should (equal (plist-get private :accessible) "private"))
      (should (equal (plist-get private :reply_limit) "N")))))

(ert-deftest douban-test-annotation-update-preserves-editor-state-and-images ()
  (let* ((douban-default-reply-limit 'all)
         (douban-default-original nil)
         (raw (douban-test--topic-image-raw "42" "7" "99"))
         (state
          '(:photos [(:id "42" :seq_id "9")
                     (:id 99 :seq_id 3)]
            :image-layout "horizontal"
            :reply-limit "F"
            :accessible "public"
            :interest-tags "文学#随笔"
            :explanation-types "O"
            :original t
            :video-info :json-null
            :anthology-id "82"))
         (meta
          '(:kind annotation
            :annotation-id "456"
            :subject-id "123"
            :title "更新后的标题"))
         (payload
          (json-parse-string
           (douban--annotation-request-body raw meta state)
           :object-type 'plist
           :null-object :json-null
           :false-object :json-false)))
    (should-not (plist-member payload :id))
    (should-not (plist-member payload :group_id))
    (should (equal (plist-get payload :subtype) "annotation"))
    (should (equal (plist-get payload :subject_id) "123"))
    (should (equal (plist-get payload :accessible) "public"))
    (should (equal (plist-get payload :reply_limit) "F"))
    (should (equal (plist-get payload :interest_tags) "文学#随笔"))
    (should (equal (plist-get payload :explanation_types) "O"))
    (should (eq (plist-get payload :send_status) :json-false))
    (should (eq (plist-get payload :original) t))
    (should (eq (plist-get payload :video_info) :json-null))
    (should (equal (plist-get payload :anthology_id) "82"))
    (should (equal (plist-get payload :image_ids) "9_42,10_7,3_99"))
    (should (equal (plist-get payload :image_layout) "horizontal")))
  (let ((raw (douban-test--status-raw "正文")))
    (dolist
        (case
         '((all "F")
           (following "A")))
      (let* ((douban-default-reply-limit (nth 0 case))
             (preserved
              (json-parse-string
               (douban--annotation-request-body
                raw
                '(:kind annotation
                  :annotation-id "456"
                  :subject-id "123"
                  :title "标题")
                (list
                 :accessible "public"
                 :reply-limit (nth 1 case)
                 :original :json-false
                 :video-info :json-null))
               :object-type 'plist
               :null-object :json-null
               :false-object :json-false)))
        (should (equal (plist-get preserved :accessible) "public"))
        (should
         (equal (plist-get preserved :reply_limit) (nth 1 case)))))
    (dolist
        (case
         '((all "A")
           (following "F")))
      (let* ((douban-default-reply-limit (nth 0 case))
             (public
              (json-parse-string
               (douban--annotation-request-body
                raw
                '(:kind annotation
                  :annotation-id "456"
                  :subject-id "123"
                  :title "标题"
                  :annotation-privacy "public")
                '(:accessible "private"
                  :reply-limit "N"
                  :original :json-false
                  :video-info :json-null))
               :object-type 'plist
               :null-object :json-null
               :false-object :json-false)))
        (should (equal (plist-get public :accessible) "public"))
        (should (equal (plist-get public :reply_limit) (nth 1 case)))
        (should
         (eq (plist-get public :original) :json-false))))
    (let* ((douban-default-reply-limit 'following)
           (private
            (json-parse-string
             (douban--annotation-request-body
              raw
              '(:kind annotation
                :annotation-id "456"
                :subject-id "123"
                :title "标题"
                :annotation-privacy "private")
              '(:accessible "public"
                :reply-limit "F"
                :original :json-false
                :video-info :json-null))
             :object-type 'plist
             :null-object :json-null
             :false-object :json-false)))
      (should (equal (plist-get private :accessible) "private"))
      (should (equal (plist-get private :reply_limit) "N"))
      (should
       (eq (plist-get private :original) :json-false)))))

(ert-deftest douban-test-annotation-create-result-accepts-id-or-topic-url ()
  (dolist
      (case
       '(((:status 200 :json (:id "456") :body "{\"id\":\"456\"}")
          "456")
         ((:status 201 :json (:url "/topic/457/?from=editor")
           :body "{\"url\":\"/topic/457/?from=editor\"}")
          "457")
         ((:status 200
           :json (:id 458
                  :url "https://www.douban.com/topic/458/#comments")
           :body "ok")
          "458")))
    (let* ((result (douban--annotation-create-result (nth 0 case)))
           (id (nth 1 case)))
      (should (equal (plist-get result :id) id))
      (should
       (equal
        (plist-get result :url)
        (format "https://www.douban.com/topic/%s/" id)))))
  (dolist
      (response
       '((:status 200 :json nil :body "{}")
         (:status 200 :json (:id "0") :body "{}")
         (:status 200
          :json (:id "456" :url "/topic/457/") :body "{}")
         (:status 200
          :json (:id "456" :url "https://example.com/topic/456/")
          :body "{}")))
    (should-error
     (douban--annotation-create-result response)
     :type 'douban-published-but-not-checkpointed))
  (dolist (status '(400 401 403 422))
    (should-error
     (douban--annotation-create-result
      (list :status status :json '(:msg "rejected") :body "rejected"))
     :type 'user-error))
  (dolist (status '(302 408 500))
    (should-error
     (douban--annotation-create-result
      (list :status status :body "unknown"))
     :type 'douban-create-result-unknown)))

(ert-deftest douban-test-annotation-update-result-rejects-false-or-other-topic ()
  (let ((meta
         '(:kind annotation
           :annotation-id "456"
           :subject-id "123"
           :title "标题")))
    (should
     (equal
      (douban--annotation-update-result
       '(:status 200 :json (:id "456" :url "/topic/456/")
         :body "{\"id\":\"456\"}")
       meta)
      '(:id "456" :url "https://www.douban.com/topic/456/")))
    (dolist
        (response
         '((:status 200 :json (:id "457") :body "{\"id\":\"457\"}")
           (:status 200 :json (:url "/topic/457/") :body "{}")
           (:status 200 :body "")
           (:status 200 :body "false")
           (:status 200 :body "null")))
      (should-error
       (douban--annotation-update-result response meta)
       :type 'error))))

(ert-deftest douban-test-submit-annotation-uses-current-topic-endpoints-once ()
  (let* ((raw (douban-test--status-raw "正文"))
         (referer (douban--annotation-create-url "123"))
         (session
          (douban--make-session
           :kind 'annotation
           :ck "fresh-ck"
           :referer referer
           :host "m.douban.com"))
         calls)
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (push (list method url arguments) calls)
            '(:status 200 :json (:id "456")
              :body "{\"id\":\"456\"}"))))
      (should
       (equal
        (douban--submit-annotation
         '(:kind annotation
           :subject-id "123"
           :title "标题"
           :explanation-types "none")
         session raw)
        '(:id "456" :url "https://www.douban.com/topic/456/"))))
    (should (= (length calls) 1))
    (pcase-let ((`(,method ,url ,arguments) (car calls)))
      (should (equal method "POST"))
      (should (equal url douban--topic-post-endpoint))
      (should (eq (plist-get arguments :session) session))
      (should (plist-get arguments :allow-redirect-response))
      (should
       (equal
        (plist-get arguments :content-type)
        "application/json;charset=utf-8"))
      (let ((headers (plist-get arguments :extra-headers)))
        (should (equal (cdr (assoc-string "X-CSRF-TOKEN" headers))
                       "fresh-ck"))
        (should (equal (cdr (assoc-string "Referer" headers)) referer))
        (should (equal (cdr (assoc-string "Origin" headers))
                       "https://www.douban.com")))
      (let ((payload
             (json-parse-string
              (plist-get arguments :body)
              :object-type 'plist
              :false-object :json-false)))
        (should (equal (plist-get payload :subtype) "annotation"))
        (should (equal (plist-get payload :subject_id) "123"))
        (should (equal (plist-get payload :explanation_types) "N"))
        (should-not (plist-member payload :id))
        (should-not (plist-member payload :group_id))))
    (setq calls nil)
    (setf
     (douban--session-referer session)
     "https://www.douban.com/topic/456/edit"
     (douban--session-state session)
     '(:reply-limit "F"
       :accessible "public"
       :original :json-false
       :video-info :json-null))
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (push (list method url arguments) calls)
            '(:status 200 :json (:id "456")
              :body "{\"id\":\"456\"}"))))
      (should
       (equal
        (douban--submit-annotation
         '(:kind annotation
           :annotation-id "456"
           :subject-id "123"
           :title "更新标题")
         session raw)
        '(:id "456" :url "https://www.douban.com/topic/456/"))))
    (should (= (length calls) 1))
    (pcase-let ((`(,_method ,url ,arguments) (car calls)))
      (should
       (equal
        url
        "https://m.douban.com/rexxar/api/v2/group/topic/456/post"))
      (should-not (plist-get arguments :allow-redirect-response))
      (let ((payload
             (json-parse-string
              (plist-get arguments :body)
              :object-type 'plist
              :null-object :json-null
              :false-object :json-false)))
        (should-not (plist-member payload :id))
        (should (eq (plist-get payload :video_info) :json-null))))))

(ert-deftest douban-test-submit-annotation-create-never-retries-unknown-result ()
  (let ((calls 0)
        (session
         (douban--make-session
          :kind 'annotation
          :ck "fresh-ck"
          :referer (douban--annotation-create-url "123")
          :host "m.douban.com")))
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (&rest _arguments)
            (cl-incf calls)
            '(:status 500 :body "unknown"))))
      (should-error
       (douban--submit-annotation
        '(:kind annotation :subject-id "123" :title "标题")
        session (douban-test--status-raw "正文"))
       :type 'douban-create-result-unknown))
    (should (= calls 1))))

(ert-deftest douban-test-annotation-sessions-bind-create-and-edit-referers ()
  (let (api-arguments subject-arguments page-arguments edit-arguments
        hobbit-arguments)
    (cl-letf
        (((symbol-function 'douban--topic-api-session)
          (lambda (kind referer)
            (setq api-arguments (list kind referer))
            (douban--make-session
             :kind kind :referer referer :ck "fresh-ck"
             :host "m.douban.com")))
         ((symbol-function 'douban--annotation-require-subject)
          (lambda (session subject-id)
            (setq subject-arguments (list session subject-id))
            '(:id "123" :type "book")))
         ((symbol-function 'douban--topic-page-context)
          (lambda (&rest arguments)
            (setq page-arguments arguments)
            (cons
             (douban--make-session
              :kind 'annotation :host "www.douban.com")
             "edit-html")))
         ((symbol-function 'douban--annotation-edit-state)
          (lambda (html meta)
            (setq edit-arguments (list html meta))
            '(:accessible "public"
              :reply-limit "F"
              :original :json-false
              :video-info :json-null)))
         ((symbol-function 'douban--annotation-hobbit-tag)
          (lambda (session subject-id)
            (setq hobbit-arguments (list session subject-id))
            "共读活动")))
      (pcase-let*
          ((meta
            '(:kind annotation :subject-id "123" :title "标题"))
           (`(,api-session . ,upload-session)
            (douban--annotation-sessions meta nil)))
        (should-not upload-session)
        (should
         (equal
          api-arguments
          (list 'annotation (douban--annotation-create-url "123"))))
        (should-not page-arguments)
        (should-not edit-arguments)
        (should (equal (cadr subject-arguments) "123"))
        (should (eq (car subject-arguments) api-session))
        (should (equal (cadr hobbit-arguments) "123"))
        (should (eq (car hobbit-arguments) api-session))
        (should
         (equal
          (plist-get (douban--session-state api-session) :hobbit-tag)
          "共读活动")))
      (setq page-arguments nil edit-arguments nil)
      (pcase-let*
          ((meta
            '(:kind annotation
              :annotation-id "456"
              :subject-id "123"
              :title "更新标题"))
           (`(,api-session . ,upload-session)
            (douban--annotation-sessions meta t)))
        (should upload-session)
        (should
         (equal
          api-arguments
          '(annotation "https://www.douban.com/topic/456/edit")))
        (should
         (equal
          page-arguments
          '(annotation
            "https://www.douban.com/topic/456/edit"
            "fresh-ck" t "读书笔记编辑页")))
        (should (equal edit-arguments (list "edit-html" meta)))
        (should
         (equal
          (plist-get (douban--session-state api-session) :hobbit-tag)
          "共读活动"))))))

(ert-deftest douban-test-publish-annotation-checkpoints-created-topic-id ()
  (douban-test--with-temp-file
      ".md"
      (concat
       "---\ntitle: 新笔记\ndouban:\n"
       "  annotation:\n"
       "    subject-id: '123'\n"
       "---\n\n正文\n")
    (let* ((meta (douban--read-meta file))
           (raw (douban-test--status-raw "正文"))
           (session
            (douban--make-session
             :kind 'annotation
             :ck "fresh-ck"
             :referer (douban--annotation-create-url "123")
             :host "m.douban.com"))
           (submit-count 0))
      (cl-letf
          (((symbol-function 'douban--prepare-draft)
            (lambda (_file kind _validator)
              (should (eq kind 'annotation))
              (list raw 2 temporary-file-directory)))
           ((symbol-function 'douban--annotation-sessions)
            (lambda (value images-p)
              (should (equal value meta))
              (should-not images-p)
              (cons session nil)))
           ((symbol-function 'douban--submit-annotation)
            (lambda (value actual-session actual-raw)
              (cl-incf submit-count)
              (should (equal value meta))
              (should (eq actual-session session))
              (should (eq actual-raw raw))
              '(:id "999"
                :url "https://www.douban.com/topic/999/"))))
        (should (equal (douban--publish-file file meta) "999")))
      (should (= submit-count 1))
      (let ((saved (douban--read-meta file)))
        (should (eq (plist-get saved :kind) 'annotation))
        (should (equal (plist-get saved :subject-id) "123"))
        (should (equal (plist-get saved :annotation-id) "999"))))))

(ert-deftest douban-test-publish-annotation-update-does-not-recheckpoint ()
  (let* ((meta
          '(:kind annotation
            :annotation-id "456"
            :subject-id "123"
            :title "更新标题"))
         (raw (douban-test--status-raw "更新正文"))
         (session (douban--make-session :kind 'annotation)))
    (cl-letf
        (((symbol-function 'douban--prepare-draft)
          (lambda (&rest _arguments)
            (list raw 4 temporary-file-directory)))
         ((symbol-function 'douban--annotation-sessions)
          (lambda (&rest _arguments) (cons session nil)))
         ((symbol-function 'douban--submit-annotation)
          (lambda (&rest _arguments)
            '(:id "456"
              :url "https://www.douban.com/topic/456/")))
         ((symbol-function 'douban--checkpoint-published-content)
          (lambda (&rest _arguments)
            (ert-fail "更新已有读书笔记不应重新写回 ID"))))
      (should
       (equal
        (douban--publish-annotation-file "/tmp/annotation.md" meta)
        "456")))))

(ert-deftest douban-test-publish-annotation-title-limit-is-70-utf16-units ()
  (let ((called nil))
    (cl-letf
        (((symbol-function 'douban--prepare-draft)
          (lambda (&rest _arguments)
            (setq called t)
            (ert-fail "标题校验必须先于正文编译"))))
      (should-error
       (douban--publish-annotation-file
        "/tmp/annotation.md"
        (list
         :kind 'annotation
         :subject-id "123"
         :title (make-string 36 #x1f642)))
       :type 'user-error))
    (should-not called)))

(ert-deftest douban-test-status-text-has-no-fixed-140-limit ()
  (let ((long-text (make-string 500 ?豆))
        (emoji-text (make-string 200 #x1f642)))
    (should
     (= (douban--validate-content-draft
         (douban-test--status-raw long-text)
         "普通广播")
        500))
    (should
     (= (douban--validate-content-draft
         (douban-test--status-raw emoji-text)
         "普通广播")
        400))
    (dolist
        (raw
         (list
          (douban-test--status-raw)
          (douban-test--status-raw "")
          (douban-test--status-raw " \t ")))
      (should-error
       (douban--validate-content-draft raw "普通广播")
       :type 'user-error))))

(ert-deftest douban-test-status-request-body-uses-personal-topic-draft ()
  (let* ((input
          (douban-test--status-raw
           "第一行" "" "第三行"))
         (body
          (douban--status-request-body input nil nil))
         (outer
          (json-parse-string
           body
           :object-type 'plist
           :array-type 'array
           :false-object :json-false))
         (raw
          (json-parse-string
           (plist-get outer :content)
           :object-type 'plist
           :array-type 'array
           :false-object :json-false))
         (blocks (plist-get raw :blocks)))
    (should (equal (plist-get outer :group_id) "0"))
    (should (equal (plist-get outer :subtype) "personal"))
    (should (equal (plist-get outer :accessible) "public"))
    (should (equal (plist-get outer :reply_limit) "A"))
    (should (eq (plist-get outer :send_status) t))
    (should (eq (plist-get outer :original) t))
    (should-not (plist-member outer :ck))
    (should-not (plist-member outer :comment))
    (should-not (plist-member outer :anthology_id))
    (should-not (plist-member outer :image_layout))
    (should (equal (plist-get outer :image_ids) ""))
    (should (= (length blocks) 3))
    (should
     (equal
      (mapcar
       (lambda (block) (plist-get block :text))
       (append blocks nil))
      '("第一行" "" "第三行")))
    (dolist (block (append blocks nil))
      (should (equal (plist-get block :type) "unstyled")))
    (should (equal (plist-get raw :entityMap) nil))))

(ert-deftest douban-test-status-create-settings-map-to-topic-payload ()
  (let ((raw (douban-test--status-raw "广播正文"))
        (douban-default-original t))
    (dolist
        (case
         '((all "ai-generated" "A" "A")
           (following "none" "F" "")))
      (let* ((douban-default-reply-limit (nth 0 case))
             (payload
              (json-parse-string
               (douban--status-request-body
                raw nil nil nil
                (list
                 :kind 'status
                 :explanation-types (nth 1 case)))
               :object-type 'plist
               :false-object :json-false)))
        (should
         (equal (plist-get payload :accessible) "public"))
        (should
         (equal (plist-get payload :reply_limit) (nth 2 case)))
        (should (eq (plist-get payload :original) t))
        (should
         (equal
          (plist-get payload :explanation_types)
          (nth 3 case)))))))

(ert-deftest douban-test-status-explanation-values-map-to-protocol ()
  (let ((raw (douban-test--status-raw "广播正文")))
    (dolist
        (mapping
         '(("ai-generated" . "A")
           ("fictional" . "X")
           ("marketing" . "K")
           ("minor-safety" . "M")
           ("public-affairs" . "P")
           ("personal-opinion" . "O")
           ("repost" . "R")))
      (dolist (topic-id '(nil "495304730"))
        (let ((payload
               (json-parse-string
                (douban--status-request-body
                 raw nil topic-id
                 (and topic-id '(:video-info :json-null))
                 (list
                  :kind 'status
                  :explanation-types (car mapping)))
                :object-type 'plist
                :false-object :json-false
                :null-object :json-null)))
          (should
           (equal
            (plist-get payload :explanation_types)
            (cdr mapping))))))))

(ert-deftest douban-test-status-update-preserves-reply-and-access-state ()
  (let ((raw (douban-test--status-raw "更新正文"))
        (douban-default-original t))
    (dolist
        (case
         '((all "F")
           (following "A")))
      (let* ((douban-default-reply-limit (nth 0 case))
             (state
              (list
               :reply-limit (nth 1 case)
               :accessible "public"
               :explanation-types "X"
               :original :json-false
               :video-info :json-null))
             (payload
              (json-parse-string
               (douban--status-request-body
                raw nil "495304730" state
                '(:kind status
                  :explanation-types "public-affairs"))
               :object-type 'plist
               :false-object :json-false
               :null-object :json-null)))
        (should (equal (plist-get payload :reply_limit) (nth 1 case)))
        (should (equal (plist-get payload :accessible) "public"))
        (should (equal (plist-get payload :explanation_types) "P"))
        (should (eq (plist-get payload :original) :json-false))))
    (let* ((douban-default-reply-limit 'following)
           (private-state
            '(:reply-limit "N"
              :accessible "private"
              :explanation-types "X"
              :original :json-false
              :video-info :json-null))
           (payload
            (json-parse-string
             (douban--status-request-body
              raw nil "495304730" private-state
              '(:kind status :explanation-types "none"))
             :object-type 'plist
             :false-object :json-false
             :null-object :json-null)))
      (should (equal (plist-get payload :reply_limit) "N"))
      (should (equal (plist-get payload :accessible) "private"))
      (should (equal (plist-get payload :explanation_types) "N"))
      (should (eq (plist-get payload :original) :json-false)))))

(ert-deftest douban-test-status-create-original-follows-global-setting ()
  (let ((raw (douban-test--status-raw "广播正文")))
    (dolist
        (case
         '((t t)
           (nil :json-false)))
      (let* ((douban-default-original (nth 0 case))
             (payload
              (json-parse-string
               (douban--status-request-body
                raw nil nil nil '(:kind status))
               :object-type 'plist
               :false-object :json-false)))
        (should (equal (plist-get payload :reply_limit) "A"))
        (should (equal (plist-get payload :accessible) "public"))
        (should (eq (plist-get payload :original) (nth 1 case)))))))

(ert-deftest douban-test-status-update-body-uses-personal-subtype ()
  (let* ((douban-default-original nil)
         (body
          (douban--status-request-body
           (douban-test--status-raw "更新正文")
           nil "495304730"
           '(:reply-limit "F"
             :accessible "friends"
             :interest-tags "电影#随笔"
             :explanation-types "spoiler"
             :original t
             :video-info :json-null
             :anthology-id "82")))
         (payload
          (json-parse-string
           body
           :object-type 'plist
           :false-object :json-false
           :null-object :json-null)))
    (should-not (plist-member payload :id))
    (should-not (plist-member payload :title))
    (should (equal (plist-get payload :subtype) "personal"))
    (should (equal (plist-get payload :reply_limit) "F"))
    (should (equal (plist-get payload :accessible) "friends"))
    (should (equal (plist-get payload :topic_tag_ids) ""))
    (should (equal (plist-get payload :interest_tags) "电影#随笔"))
    (should (equal (plist-get payload :explanation_types) "spoiler"))
    (should (eq (plist-get payload :original) t))
    (should (equal (plist-get payload :anthology_id) "82"))
    (should (eq (plist-get payload :is_event) :json-false))
    (should (eq (plist-get payload :is_activity_rule) :json-false))
    (should (eq (plist-get payload :enable_item_tag) :json-false))
    (should-not (plist-member payload :group_id))
    (should (eq (plist-get payload :video_info) :json-null))
    (should-not (plist-member payload :image_layout))
    (should (equal (plist-get payload :image_ids) ""))))

(ert-deftest douban-test-status-request-body-submits-anthology-only-when-set ()
  (let* ((raw (douban-test--status-raw "正文"))
         (with-anthology
          (json-parse-string
           (douban--status-request-body raw "42" nil)
           :object-type 'plist
           :false-object :json-false))
         (without-anthology
          (json-parse-string
           (douban--status-request-body raw nil nil)
           :object-type 'plist
           :false-object :json-false)))
    (should (equal (plist-get with-anthology :anthology_id) "42"))
    (should-not (plist-member without-anthology :anthology_id))))

(ert-deftest douban-test-topic-image-ids-and-vertical-layout ()
  (let* ((raw (douban-test--topic-image-raw "42" "7"))
         (blocks (plist-get raw :blocks)))
    (setf
     (plist-get raw :blocks)
     (vconcat blocks (vector (copy-tree (aref blocks 0)))))
    (should (equal (douban--topic-image-ids raw) "1_42,2_7,3_42"))
    (let ((payload
           (json-parse-string
            (douban--status-request-body raw nil nil)
            :object-type 'plist
            :false-object :json-false)))
      (should
       (equal
        (plist-get payload :image_ids)
        "1_42,2_7,3_42"))
      (should (equal (plist-get payload :image_layout) "vertical")))))

(ert-deftest douban-test-status-update-preserves-existing-image-sequences ()
  (let* ((raw (douban-test--topic-image-raw "42" "7" "99"))
         (photos
          [(:id "42" :seq_id "9")
           (:id 99 :seq_id 3)])
         (body
          (douban--status-request-body
           raw "81" "495304730"
           (list
            :photos photos
            :image-layout "horizontal"
            :video-info :json-null)))
         (payload
          (json-parse-string
           body
           :object-type 'plist
           :false-object :json-false)))
    (should
     (equal
      (plist-get payload :image_ids)
      "9_42,10_7,3_99"))
    (should (equal (plist-get payload :image_layout) "horizontal"))
    (should (equal (plist-get payload :anthology_id) "81"))))

(defun douban-test--review-broadcast-item-html
    (sid review-id &optional action object-kind)
  "返回 SID、REVIEW-ID、ACTION、OBJECT-KIND 对应的长评广播 HTML。"
  (format
   (concat
    "<div class=\"status-item\" data-sid=\"%s\" "
    "data-action=\"%s\" data-object-kind=\"%s\" "
    "data-object-id=\"%s\"></div>")
   (or sid "") (or action "7") (or object-kind "1012")
   (or review-id "")))

(ert-deftest douban-test-review-broadcast-sid-matches-one-review-activity ()
  (let ((html
         (concat
          (douban-test--review-broadcast-item-html
           "9000" "122")
          (douban-test--review-broadcast-item-html
           "9001" "123")
          (douban-test--review-broadcast-item-html
           "9002" "123" "8")
          (douban-test--review-broadcast-item-html
           "9003" "123" "7" "1001"))))
    (should
     (equal (douban--review-broadcast-sid html "123") "9001"))))

(ert-deftest douban-test-review-broadcast-sid-fails-closed ()
  (dolist
      (html
       (list
        "<html><body></body></html>"
        (douban-test--review-broadcast-item-html "bad" "123")
        (douban-test--review-broadcast-item-html "9001" "1234")
        (douban-test--review-broadcast-item-html "9001" "123" "8")
        (douban-test--review-broadcast-item-html
         "9001" "123" "7" "1001")
        (concat
         (douban-test--review-broadcast-item-html "9001" "123")
         (douban-test--review-broadcast-item-html "9002" "123"))))
    (should-not (douban--review-broadcast-sid html "123")))
  (dolist (review-id '(nil ""))
    (should-error
     (douban--review-broadcast-sid
      (concat
       "<div class=\"status-item\" data-sid=\"9001\" "
       "data-action=\"7\" data-object-kind=\"1012\"></div>")
      review-id)
     :type 'error)))

(ert-deftest douban-test-remove-created-review-broadcast-uses-www-session ()
  (let* ((review-session
          (douban--make-session
           :kind 'review
           :cookies '(("book-cookie" . "book-only"))
           :ck "fresh-ck"
           :host "book.douban.com"
           :referer "https://book.douban.com/subject/123/"))
         (html
          (douban-test--review-broadcast-item-html "9001" "123"))
         cookie-url
         requests)
    (cl-letf
        (((symbol-function 'sleep-for)
          (lambda (&rest _arguments) nil))
         ((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (setq cookie-url url)
            '(("www-cookie" . "www-only"))))
         ((symbol-function 'douban--http)
          (lambda (method url &rest arguments)
            (push (list method url arguments) requests)
            (if (equal method "GET")
                (list :status 200 :headers nil :body html)
              '(:status 200 :headers nil :body "{\"r\":0}")))))
      (should
       (equal
        (douban--remove-created-review-broadcast
         review-session "123")
        "9001")))
    (should (equal cookie-url douban--status-home-url))
    (setq requests (nreverse requests))
    (should (= (length requests) 2))
    (pcase-let
        ((`(("GET" ,get-url ,get-arguments)
           ("POST" ,post-url ,post-arguments))
          requests))
      (should (equal get-url douban--status-home-url))
      (should (equal post-url douban--status-delete-endpoint))
      (let ((home-session (plist-get get-arguments :session)))
        (should-not (eq home-session review-session))
        (should (eq home-session (plist-get post-arguments :session)))
        (should (equal (douban--session-host home-session)
                       "www.douban.com"))
        (should
         (equal
          (douban--session-cookies home-session)
          '(("ck" . "fresh-ck")
            ("www-cookie" . "www-only"))))
        (should
         (equal
          (plist-get get-arguments :extra-headers)
          '(("Accept" . "text/html,application/xhtml+xml")
            ("Cache-Control" . "no-cache"))))
        (should
         (equal
          (plist-get post-arguments :body)
          "sid=9001&ck=fresh-ck"))
        (should
         (equal
          (plist-get post-arguments :content-type)
          "application/x-www-form-urlencoded; charset=UTF-8"))
        (should
         (equal
          (plist-get post-arguments :extra-headers)
          '(("X-Requested-With" . "XMLHttpRequest")
            ("Referer" . "https://www.douban.com/")
            ("Origin" . "https://www.douban.com"))))))))

(ert-deftest douban-test-remove-created-review-broadcast-requires-r-zero ()
  (let ((review-session (douban--make-session :kind 'review :ck "ck"))
        (home-session
         (douban--make-session
          :kind 'status :ck "ck" :host "www.douban.com"
          :referer douban--status-home-url))
        (html
         (douban-test--review-broadcast-item-html "9001" "123"))
        response)
    (cl-letf
        (((symbol-function 'sleep-for)
          (lambda (&rest _arguments) nil))
         ((symbol-function 'douban--browser-session)
          (lambda (&rest _arguments) home-session))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (list :status 200 :body html)))
         ((symbol-function 'douban--content-mutation-request)
          (lambda (&rest _arguments) response)))
      (setq response '(:status 200 :json (:r 1) :body "rejected"))
      (should-error
       (douban--remove-created-review-broadcast review-session "123")
       :type 'user-error)
      (setq response '(:status 200 :json (:ok t) :body "{}"))
      (should-error
       (douban--remove-created-review-broadcast review-session "123")
       :type 'error))))

(ert-deftest douban-test-remove-created-review-broadcast-retries-only-get ()
  (let ((review-session (douban--make-session :kind 'review :ck "ck"))
        (home-session
         (douban--make-session
          :kind 'status :ck "ck" :host "www.douban.com"
          :referer douban--status-home-url))
        (pages
         (list
          "<html></html>"
          "<html></html>"
          (douban-test--review-broadcast-item-html "9001" "123")))
        (get-count 0)
        (post-count 0))
    (cl-letf
        (((symbol-function 'sleep-for)
          (lambda (&rest _arguments) nil))
         ((symbol-function 'douban--browser-session)
          (lambda (&rest _arguments) home-session))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (cl-incf get-count)
            (list :status 200 :body (pop pages))))
         ((symbol-function 'douban--content-mutation-request)
          (lambda (&rest _arguments)
            (cl-incf post-count)
            '(:status 200 :json (:r 0) :body "{\"r\":0}"))))
      (should
       (equal
        (douban--remove-created-review-broadcast
         review-session "123")
        "9001")))
    (should (= get-count 3))
    (should (= post-count 1))))

(ert-deftest douban-test-remove-created-review-broadcast-never-guesses-sid ()
  (let ((review-session (douban--make-session :kind 'review :ck "ck"))
        (home-session
         (douban--make-session
          :kind 'status :ck "ck" :host "www.douban.com"
          :referer douban--status-home-url))
        (get-count 0))
    (cl-letf
        (((symbol-function 'sleep-for)
          (lambda (&rest _arguments) nil))
         ((symbol-function 'douban--browser-session)
          (lambda (&rest _arguments) home-session))
         ((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (cl-incf get-count)
            '(:status 200 :body "<html></html>")))
         ((symbol-function 'douban--content-mutation-request)
          (lambda (&rest _arguments)
            (ert-fail "没有唯一匹配时不得发送删除请求"))))
      (should-error
       (douban--remove-created-review-broadcast review-session "123")
       :type 'error))
    (should (= get-count 3))))

(ert-deftest douban-test-status-create-returns-topic-id-after-2xx ()
  (should
   (equal
    (douban--status-create-result
     '(:status 200
       :json (:id "495304730")
       :body "{\"id\":\"495304730\"}"))
    '(:id "495304730"))))

(ert-deftest douban-test-status-create-classifies-failures ()
  (dolist (status '(400 401 403 422))
    (should-error
     (douban--status-create-result
      (list :status status :json '(:msg "rejected")))
     :type 'user-error))
  (dolist (status '(302 408 500))
    (should-error
     (douban--status-create-result
      (list :status status :body "unknown"))
     :type 'douban-create-result-unknown)))

(ert-deftest douban-test-mutation-http-success-boundary ()
  (dolist (create-p '(t nil))
    (dolist (status '(200 299))
      (let ((response (list :status status :body "ok")))
        (should
         (eq
          (douban--require-mutation-success
           response create-p "测试变更" "请检查远端。")
          response)))))
  (dolist (status '(400 401 403 422 499))
    (should-error
     (douban--require-mutation-success
      (list :status status :body "rejected")
      t "测试创建" "请检查远端。")
     :type 'user-error))
  (dolist (status '(300 302 399 408 500 599 nil))
    (should-error
     (douban--require-mutation-success
      (list :status status :body "unknown")
      t "测试创建" "请检查远端。")
     :type 'douban-create-result-unknown))
  (dolist (status '(300 302 399 400 401 403 408 499 500 599 nil))
    (should-error
     (douban--require-mutation-success
      (list :status status :body "failed")
      nil "测试更新" nil)
     :type 'user-error)))

(ert-deftest douban-test-status-accepted-without-topic-id-is-not-checkpointed ()
  (dolist (json '(nil (:id nil) (:id "") (:id "bad")))
    (should-error
     (douban--status-create-result
      (list :status 200 :json json :body "{}"))
     :type 'douban-published-but-not-checkpointed)))

(ert-deftest douban-test-submit-status-uses-current-personal-topic-api ()
  (let* ((douban-default-reply-limit 'following)
         (raw (douban-test--status-raw
               "第一段广播 & 不带标题" "第二段"))
         (session
          (douban--make-session
           :kind 'status
           :cookies '(("ck" . "fresh-ck")
                      ("dbcl2" . "login"))
           :ck "fresh-ck"
           :referer douban--status-home-url
           :host "m.douban.com"))
         post-calls)
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (push (list method url arguments) post-calls)
            '(:status 200
              :json (:id "495304730")
              :body "{\"id\":\"495304730\"}"))))
      (should
       (equal
        (douban--submit-status
         '(:kind status
           :explanation-types "marketing")
         session raw)
        '(:id "495304730"))))
    (should (= (length post-calls) 1))
    (pcase-let
        ((`(,method ,url ,arguments) (car post-calls)))
      (should (equal method "POST"))
      (should (equal url douban--topic-post-endpoint))
      (should (eq (plist-get arguments :session) session))
      (should
       (plist-get arguments :allow-redirect-response))
      (should
       (equal
        (plist-get arguments :content-type)
        "application/json;charset=utf-8"))
      (let ((headers (plist-get arguments :extra-headers)))
        (should
         (equal (cdr (assoc-string "Accept" headers))
                "application/json"))
        (should
         (equal (cdr (assoc-string "X-CSRF-TOKEN" headers))
                "fresh-ck"))
        (should
         (equal (cdr (assoc-string "Referer" headers))
                douban--status-home-url))
        (should
         (equal (cdr (assoc-string "Origin" headers))
                "https://www.douban.com")))
      (let ((outer
             (json-parse-string
              (plist-get arguments :body)
              :object-type 'plist
              :null-object :json-null
              :false-object :json-false)))
        (should (equal (plist-get outer :group_id) "0"))
        (should (stringp (plist-get outer :content)))
        (should (equal (plist-get outer :accessible) "public"))
        (should (equal (plist-get outer :reply_limit) "F"))
        (should (eq (plist-get outer :original) t))
        (should
         (equal (plist-get outer :explanation_types) "K"))
        (should-not (plist-member outer :anthology_id))
        (should-not (plist-member outer :ck))
        (should-not (plist-member outer :comment))))))

(ert-deftest douban-test-submit-status-update-uses-topic-endpoint-once ()
  (let* ((topic-id "495304730")
         (edit-url
          (format
           "https://www.douban.com/topic/%s/edit"
           topic-id))
         (meta
         (list
           :kind 'status
           :status-id topic-id
           :anthology-id "81"))
         (session
          (douban--make-session
           :kind 'status
           :cookies '(("ck" . "fresh-ck"))
           :ck "fresh-ck"
           :referer edit-url
           :host "m.douban.com"
           :state
           '(:photos [(:id "42" :seq_id "9")]
             :image-layout "horizontal"
             :reply-limit "F"
             :accessible "friends"
             :interest-tags "电影#随笔"
             :explanation-types "spoiler"
             :original t
             :video-info :json-null)))
         (raw (douban-test--topic-image-raw "42" "7"))
         request)
    (cl-letf
        (((symbol-function 'douban--create-request)
          (lambda (&rest _arguments)
            (ert-fail "广播更新不能进入创建请求路径")))
         ((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (setq request (list method url arguments))
            '(:status 200 :json nil :body "{}"))))
      (should
      (equal
        (douban--submit-status meta session raw)
        (list :id topic-id))))
    (pcase-let ((`(,method ,url ,arguments) request))
      (should (equal method "POST"))
      (should
       (equal
        url
        (format douban--topic-update-endpoint-format topic-id)))
      (let* ((headers (plist-get arguments :extra-headers))
             (payload
              (json-parse-string
               (plist-get arguments :body)
               :object-type 'plist
               :null-object :json-null
               :false-object :json-false)))
        (should (eq (plist-get arguments :session) session))
        (should-not
         (plist-get arguments :allow-redirect-response))
        (should-not (plist-member payload :id))
        (should-not (plist-member payload :title))
        (should-not (plist-member payload :group_id))
        (should (eq (plist-get payload :video_info) :json-null))
        (should
         (equal (plist-get payload :image_ids) "9_42,10_7"))
        (should
         (equal (plist-get payload :image_layout) "horizontal"))
        (should (equal (plist-get payload :anthology_id) "81"))
        (should (equal (plist-get payload :reply_limit) "F"))
        (should (equal (plist-get payload :accessible) "friends"))
        (should
         (equal (plist-get payload :interest_tags) "电影#随笔"))
        (should (equal (plist-get payload :topic_tag_ids) ""))
        (should
         (equal (plist-get payload :explanation_types) "spoiler"))
        (should (eq (plist-get payload :original) t))
        (should
         (equal
          (cdr (assoc-string "Referer" headers))
          edit-url))))))

(ert-deftest douban-test-status-update-failure-is-never-reclassified-as-create ()
  (let ((meta
         '(:kind status
           :status-id "7003"))
        (session
         (douban--make-session
          :kind 'status
          :ck "fresh-ck"
          :referer "https://www.douban.com/topic/7003/edit"
          :state '(:video-info :json-null)))
        (raw (douban-test--status-raw "更新正文")))
    (dolist (status '(408 500))
      (let ((condition
             (condition-case err
                 (cl-letf
                     (((symbol-function 'douban--http-json)
                       (lambda (&rest _arguments)
                         (list :status status :body "failed"))))
                   (douban--submit-status meta session raw)
                   nil)
               (error err))))
        (should condition)
        (should-not
         (eq (car condition) 'douban-create-result-unknown))))))

(ert-deftest douban-test-status-update-requires-truthy-response-data ()
  (let ((meta '(:status-id "7003")))
    (dolist
        (response
         '((:status 204 :body "")
           (:status 200 :body "null" :json :json-null)
           (:status 200 :body "false" :json :json-false)
           (:status 200 :body "0" :json 0)
           (:status 200 :body "\"\"" :json "")))
      (should-error
       (douban--status-update-result response meta)
       :type 'error))
    (should
     (equal
      (douban--status-update-result
       '(:status 200 :body "{}" :json nil)
       meta)
       '(:id "7003")))))

(ert-deftest douban-test-submit-status-does-not-retry-ambiguous-post ()
  (let ((session
         (douban--make-session
          :kind 'status
          :cookies '(("ck" . "fresh-ck")
                     ("dbcl2" . "login"))
          :ck "fresh-ck"
          :referer douban--topic-post-endpoint
          :host "m.douban.com"))
        (raw (douban-test--status-raw "可能已经发出的广播"))
        (calls 0))
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (&rest _arguments)
            (cl-incf calls)
            (signal 'plz-curl-error '("connection lost")))))
      (should-error
       (douban--submit-status '(:kind status) session raw)
       :type 'douban-create-result-unknown))
    (should (= calls 1))))

(defun douban-test--decode-form-body (body)
  "将 application/x-www-form-urlencoded 格式的 BODY 解码为关联列表。"
  (mapcar
   (lambda (part)
     (let* ((pieces (split-string part "="))
            (name
             (decode-coding-string
              (url-unhex-string (car pieces)) 'utf-8))
            (value
             (decode-coding-string
              (url-unhex-string
               (mapconcat #'identity (cdr pieces) "="))
              'utf-8)))
       (cons name value)))
   (split-string body "&" t)))

(defun douban-test--note-editor-html (action &optional extra)
  "返回 ACTION 已绑定且包含 EXTRA 的典型富文本日记编辑页。"
  (concat
   "<html><body>"
   "<form>"
   "<input name=\"note_id\" value=\"42\">"
   "<input name=\"ck\" value=\"page-ck\">"
   (format
    "<input type=\"hidden\" name=\"action\" value=\"%s\">"
    action)
   "<input type=\"radio\" name=\"note_privacy\" "
   "value=\"P\" checked=\"checked\">"
   "<input type=\"radio\" name=\"note_privacy\" value=\"F\">"
   (or extra "")
   "</form>"
   "<script>"
   "_POST_PARAMS = {siteCookie: {"
   "name: \"upload_auth_token\", value: \"upload-token\""
   "}};"
   "</script>"
   "</body></html>"))

(defun douban-test--note-session (&optional action)
  "返回一个字段完整且绑定 ACTION 的日记写操作会话。"
  (douban--make-session
   :kind 'note
   :cookies '(("dbcl2" . "login-cookie"))
   :ck "page-ck"
   :referer "https://www.douban.com/note/42/edit"
   :host "www.douban.com"
   :state
   (list
    :note-id "42"
    :default-privacy "P"
    :action (or action "new")
    :upload-field "upload_auth_token"
    :upload-token "upload-token")))

(ert-deftest douban-test-note-session-populates-page-bound-state ()
  (cl-letf
      (((symbol-function 'douban--read-browser-cookies)
        (lambda (_url)
          '(("dbcl2" . "login-cookie"))))
       ((symbol-function 'douban--http)
        (lambda (_method _url &rest _arguments)
          (list
           :status 200
           :body
           (douban-test--note-editor-html
            "page-update-action")))))
    (let ((session
           (douban--note-session
            '(:kind note
              :note-id "42"))))
      (should (eq (douban--session-kind session) 'note))
      (should
       (equal
        (douban--session-state-get session :note-id)
        "42"))
      (should (equal (douban--session-ck session) "page-ck"))
      (should
       (equal
        (douban--session-state session)
        '(:note-id "42"
          :ck "page-ck"
          :default-privacy "P"
          :action "page-update-action"
          :upload-field "upload_auth_token"
          :upload-token "upload-token")))
      (should
       (equal
        (douban--session-state-get
         session :upload-field)
        "upload_auth_token"))
      (should
       (equal
        (douban--session-state-get
         session :upload-token)
        "upload-token"))
      (should
       (equal
        (cdr (assoc "ck" (douban--session-cookies session)))
        "page-ck")))))

(ert-deftest douban-test-note-session-rejects-id-mismatch ()
  (cl-letf
      (((symbol-function 'douban--read-browser-cookies)
        (lambda (_url)
          '(("dbcl2" . "login-cookie"))))
       ((symbol-function 'douban--http)
        (lambda (_method _url &rest _arguments)
          (list
           :status 200
           :body
           (douban-test--note-editor-html
            "page-update-action")))))
    (should-error
     (douban--note-session
      '(:kind note
        :note-id "99"))
     :type 'error)))

(ert-deftest douban-test-note-editor-state-reads-post-params-action ()
  (let* ((html
          (douban-test--note-editor-html
           "removed-hidden-action"))
         (html
          (replace-regexp-in-string
           "<input type=\"hidden\" name=\"action\"[^>]*>"
           ""
           html))
         (html
          (replace-regexp-in-string
           "_POST_PARAMS = {"
           "_POST_PARAMS = {action: \"page-update-action\", "
           html
           t
           t))
         (session
          (cl-letf
              (((symbol-function 'douban--read-browser-cookies)
                (lambda (_url)
                  '(("dbcl2" . "login-cookie"))))
               ((symbol-function 'douban--http)
                (lambda (_method _url &rest _arguments)
                  (list :status 200 :body html))))
            (douban--note-session
             '(:kind note
               :note-id "42")))))
    (should
     (equal
      (douban--session-state-get session :action)
      "page-update-action"))))

(ert-deftest douban-test-note-session-binds-update-to-edit-page ()
  (let ((expected-url "https://www.douban.com/note/42/edit")
        cookie-url
        request-url)
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (setq cookie-url url)
            '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (method url &rest _arguments)
            (should (equal method "GET"))
            (setq request-url url)
            (list
             :status 200
             :body
             (douban-test--note-editor-html
              "page-update-action")))))
      (let ((session
             (douban--note-session
              '(:kind note
                :note-id "42"))))
        (should (equal cookie-url expected-url))
        (should (equal request-url expected-url))
        (should
         (equal
          (douban--session-state-get session :note-id)
          "42"))
        (should
         (equal
          (douban--session-state-get session :action)
          "page-update-action"))))))

(ert-deftest douban-test-note-session-enforces-three-state-actions ()
  (dolist
      (case
       '(((:kind note)
          "new"
          "https://www.douban.com/note/create")
         ((:kind note :note-id "42")
          "new"
          "https://www.douban.com/note/42/edit")
         ((:kind note
           :note-id "42")
          "page-update-action"
          "https://www.douban.com/note/42/edit")))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (_url)
            '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (_method url &rest _arguments)
            (should (equal url (nth 2 case)))
            (list
             :status 200
             :body (douban-test--note-editor-html (nth 1 case))))))
      (should
       (eq
        (douban--session-kind
         (douban--note-session (car case)))
        'note))))
  (dolist
      (case
       '(((:kind note)
          "page-update-action"
          "https://www.douban.com/note/create")))
    (cl-letf
        (((symbol-function 'douban--read-browser-cookies)
          (lambda (_url)
            '(("dbcl2" . "login-cookie"))))
         ((symbol-function 'douban--http)
          (lambda (_method url &rest _arguments)
            (should (equal url (nth 2 case)))
            (list
             :status 200
             :body (douban-test--note-editor-html (nth 1 case))))))
      (should-error
       (douban--note-session (car case))
       :type 'error))))

(ert-deftest douban-test-note-editor-state-uses-first-note-form ()
  (let ((html
         (concat
          "<html><body>"
          "<form><input name=\"note_id\" value=\"42\">"
          "<input name=\"ck\" value=\"first-ck\">"
          "<input type=\"hidden\" name=\"action\" value=\"new\">"
          "<input type=\"radio\" name=\"note_privacy\" "
          "value=\"P\" checked=\"checked\">"
          "</form><form>"
          "<input name=\"note_id\" value=\"99\">"
          "<input name=\"ck\" value=\"second-ck\">"
          "<input type=\"hidden\" name=\"action\" value=\"update\">"
          "<input type=\"radio\" name=\"note_privacy\" "
          "value=\"F\" checked=\"checked\">"
          "</form></body></html>")))
    (let ((state
           (douban--note-editor-state
            html "https://www.douban.com/note/create")))
      (should (equal (plist-get state :note-id) "42"))
      (should (equal (plist-get state :ck) "first-ck"))
      (should (equal (plist-get state :default-privacy) "P"))
      (should (equal (plist-get state :action) "new")))))

(ert-deftest douban-test-note-session-rejects-login ()
  (cl-letf
      (((symbol-function 'douban--read-browser-cookies)
        (lambda (_url)
          '(("dbcl2" . "login-cookie"))))
       ((symbol-function 'douban--http)
        (lambda (_method _url &rest _arguments)
          '(:status 403 :body "login required"))))
    (should-error
     (douban--note-session '(:kind note))
     :type 'user-error)))

(ert-deftest douban-test-note-editor-state-requires-selected-privacy ()
  (should-error
   (douban--note-editor-state
    (replace-regexp-in-string
     " checked=\"checked\"" ""
     (douban-test--note-editor-html "new"))
    "https://www.douban.com/note/create")
   :type 'user-error))

(ert-deftest douban-test-note-editor-state-rejects-empty-default-privacy ()
  (let ((html
         (replace-regexp-in-string
          "value=\"P\" checked=\"checked\""
          "value=\"\" checked=\"checked\""
          (douban-test--note-editor-html "new")
          t
          t)))
    (should-error
     (douban--note-editor-state
      html "https://www.douban.com/note/create")
     :type 'user-error)))

(ert-deftest douban-test-note-form-fields-use-page-privacy-default ()
  (let* ((session (douban-test--note-session))
         (raw
          (list
           :blocks
           (vector
            '(:key "a" :text "正文" :type "unstyled"
                   :depth 0 :inlineStyleRanges []
                   :entityRanges [] :data nil))
           :entityMap (make-hash-table :test 'equal)))
         (meta
          '(:note-privacy "friends"
            :cannot-reply t
            :author-tags ("随笔" "生活")))
         (fields
          (douban--note-form-fields
           meta raw session "日记标题" "F")))
    (should
     (equal
      fields
      (append
       (list
        '("is_rich" . "1")
        '("note_id" . "42")
        '("note_title" . "日记标题")
        (cons
         "note_text"
         (json-serialize
          raw :null-object :json-null
          :false-object :json-false))
        '("introduction" . "")
        '("note_privacy" . "F")
        '("cannot_reply" . "on")
        '("author_tags" . "随笔 生活")
        '("accept_donation" . "")
        '("donation_notice" . "")
        '("is_original" . "")
        '("ck" . "page-ck"))
       '(("action" . "new")))))
    (should
     (equal
      (douban--note-privacy-value nil session)
      "P"))
    (should
     (equal
      (douban--note-privacy-value
       '(:note-privacy "friends")
       session)
      "F"))))

(ert-deftest douban-test-note-form-action-comes-from-bound-page ()
  (let ((raw
         (list
          :blocks []
          :entityMap (make-hash-table :test 'equal))))
    (dolist (action '("new" "page-update-action"))
      (let* ((session (douban-test--note-session action))
             (fields
              (douban--note-form-fields
               nil raw session "日记标题" "P")))
        (should (equal (cdr (assoc "action" fields)) action))))))

(ert-deftest douban-test-note-publish-success-includes-action ()
  (let ((session (douban-test--note-session))
        (raw
         (list
          :blocks []
          :entityMap (make-hash-table :test 'equal)))
        captured)
    (cl-letf
        (((symbol-function 'douban--http)
          (lambda (method url &rest arguments)
            (setq captured (list method url arguments))
            '(:status 200
              :headers nil
              :body "{\"r\":0,\"url\":\"/note/42/\"}"))))
      (should
       (equal
        (douban--submit-note nil raw session "日记标题" "P")
        '(:id "42" :url "https://www.douban.com/note/42/"))))
    (should (equal (car captured) "POST"))
    (should
     (equal
      (cadr captured)
      "https://www.douban.com/j/note/publish"))
    (let* ((arguments (nth 2 captured))
           (fields
            (douban-test--decode-form-body
             (plist-get arguments :body))))
      (should
       (plist-get arguments :allow-redirect-response))
      (should (equal (cdr (assoc "action" fields)) "new"))
      (should (equal (cdr (assoc "ck" fields)) "page-ck")))))

(ert-deftest douban-test-note-update-submits-once-with-page-action ()
  (let ((meta
         '(:note-id "42"))
        (session
         (douban-test--note-session "page-update-action"))
        (raw
         (list
          :blocks []
          :entityMap (make-hash-table :test 'equal)))
        (request-count 0)
        captured)
    (cl-letf
        (((symbol-function 'douban--http)
          (lambda (method url &rest arguments)
            (cl-incf request-count)
            (setq captured (list method url arguments))
            '(:status 200
              :headers nil
              :body "{\"r\":0,\"url\":\"/note/42/\"}"))))
      (should
       (equal
        (douban--submit-note
         meta raw session "更新后的标题" "P")
        '(:id "42" :url "https://www.douban.com/note/42/"))))
    (should (= request-count 1))
    (should (equal (car captured) "POST"))
    (should
     (equal
      (cadr captured)
      "https://www.douban.com/j/note/publish"))
    (let* ((arguments (nth 2 captured))
           (fields
            (douban-test--decode-form-body
             (plist-get arguments :body))))
      (should-not
       (plist-get arguments :allow-redirect-response))
      (should (equal (cdr (assoc "note_id" fields)) "42"))
      (should
       (equal
        (cdr (assoc "action" fields))
        "page-update-action")))))

(ert-deftest douban-test-note-update-failure-does-not-submit-new-action ()
  (let ((meta
         '(:note-id "42"))
        (session
         (douban-test--note-session "page-update-action"))
        (raw
         (list
          :blocks []
          :entityMap (make-hash-table :test 'equal)))
        actions)
    (cl-letf
        (((symbol-function 'douban--http)
          (lambda (_method _url &rest arguments)
            (push
             (cdr
              (assoc
               "action"
               (douban-test--decode-form-body
                (plist-get arguments :body))))
             actions)
            '(:status 422
              :headers nil
              :body "{\"error\":\"invalid note\"}"))))
      (should-error
       (douban--submit-note
        meta raw session "更新后的标题" "P")
       :type 'error))
    (should (equal actions '("page-update-action")))))

(ert-deftest douban-test-note-update-network-error-is-retriable-error ()
  (let ((meta
         '(:note-id "42"))
        (session
         (douban-test--note-session "page-update-action"))
        (raw
         (list
          :blocks []
          :entityMap (make-hash-table :test 'equal)))
        (request-count 0)
        caught)
    (cl-letf
        (((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (cl-incf request-count)
            (error "connection lost"))))
      (condition-case err
          (progn
            (douban--submit-note
             meta raw session "更新后的标题" "P")
            (ert-fail "更新请求中断必须报错"))
        (douban-create-result-unknown
         (ert-fail
          (format "更新被误报为不确定创建：%S" err)))
        (error
         (setq caught err))))
    (should caught)
    (should (= request-count 1))))

(ert-deftest douban-test-note-publish-network-error-is-ambiguous ()
  (let ((session (douban-test--note-session))
        (raw
         (list
          :blocks []
          :entityMap (make-hash-table :test 'equal))))
    (cl-letf
        (((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            (signal
             'plz-curl-error
             '("connection lost after request started")))))
      (let ((condition
             (should-error
              (douban--submit-note nil raw session "日记标题" "P")
              :type 'douban-create-result-unknown)))
        (should
         (string-match-p
          "note-id.*42"
          (error-message-string condition)))))))

(ert-deftest douban-test-note-publish-definite-4xx-is-not-ambiguous ()
  (let ((session (douban-test--note-session))
        (raw
         (list
          :blocks []
          :entityMap (make-hash-table :test 'equal)))
        caught)
    (cl-letf
        (((symbol-function 'douban--http)
          (lambda (&rest _arguments)
            '(:status 422
              :headers nil
              :body "{\"error\":\"invalid note\"}"))))
      (condition-case err
          (progn
            (douban--submit-note nil raw session "日记标题" "P")
            (ert-fail "HTTP 422 must fail"))
        (douban-create-result-unknown
         (ert-fail
          (format "definite failure became ambiguous: %S" err)))
        (error
         (setq caught err))))
    (should caught)
    (should (string-match-p "HTTP 422" (error-message-string caught)))))

(ert-deftest douban-test-note-publish-http-408-is-ambiguous ()
  (let ((session (douban-test--note-session)))
    (dolist (status '(302 408 500))
      (should-error
       (douban--note-response-result
        (list
         :status status
         :body "result unknown"
         :json nil)
        session nil)
       :type 'douban-create-result-unknown))))

(ert-deftest douban-test-note-image-upload-uses-note-multipart-contract ()
  (let* ((session (douban-test--note-session))
         (bytes (unibyte-string 137 80 78 71 13 10 26 10))
         captured)
    (cl-letf
        (((symbol-function 'douban--http)
          (lambda (method url &rest arguments)
            (setq captured (list method url arguments))
            (list
             :status 200
             :headers nil
             :body
             (concat
              "{\"r\":0,\"photo\":{\"url\":"
              "\"https://img1.doubanio.com/"
              "view/note/l/public/p42.webp\"}}")))))
      (should
       (equal
        (douban--photo-url
         (douban--upload-image-bytes
          session bytes "image/png")
         'note)
        (concat
         "https://img1.doubanio.com/"
         "view/note/l/public/p42.webp"))))
    (should (equal (car captured) "POST"))
    (should
     (equal
      (cadr captured)
      "https://www.douban.com/j/note/add_photo"))
    (let* ((arguments (nth 2 captured))
           (body (plist-get arguments :body)))
      (should (plist-get arguments :raw-body))
      (should
       (string-prefix-p
        "multipart/form-data; boundary="
        (plist-get arguments :content-type)))
      (should (string-match-p "name=\"image_file\"" body))
      (should (string-match-p "filename=\"image.png\"" body))
      (should
       (string-match-p
        (regexp-quote "name=\"note_id\"\r\n\r\n42\r\n")
        body))
      (should-not (string-match-p "name=\"review_id\"" body))
      (should
       (string-match-p
        (regexp-quote
         "name=\"upload_auth_token\"\r\n\r\nupload-token")
        body))
      (should (string-match-p (regexp-quote bytes) body)))))

(ert-deftest douban-test-topic-upload-token-accepts-current-page-shapes ()
  (should
   (equal
    (douban--global-upload-token
     "__INIT_STATE__.upload_auth_token = 'lower-token';")
    "lower-token"))
  (should
   (equal
    (douban--global-upload-token
     "window.STATE = {UPLOAD_AUTH_TOKEN: \"upper-token\"};")
    "upper-token"))
  (should-not
   (douban--global-upload-token
    "{\"unrelated_token\":\"missing\"}")))

(ert-deftest douban-test-status-page-context-rejects-non-2xx ()
  (dolist (status '(401 403 500))
    (let (page-cookie-url
          page-request
          (http-count 0))
      (cl-letf
          (((symbol-function 'douban--read-browser-cookies)
            (lambda (url)
              (setq page-cookie-url url)
              '(("www-cookie" . "www-only"))))
           ((symbol-function 'douban--http)
            (lambda (method url &rest _arguments)
              (cl-incf http-count)
              (setq page-request (list method url))
              (list
               :status status
               :headers nil
               :body
               "<script>upload_auth_token: \"page-token\"</script>"))))
        (should-error
         (douban--status-page-context
          douban--status-home-url "topic-ck" t "广播发布页")
         :type 'error))
      (should (= http-count 1))
      (should (equal page-cookie-url douban--status-home-url))
      (should
       (equal page-request (list "GET" douban--status-home-url))))))

(ert-deftest douban-test-annotation-images-use-topic-contract ()
  (let* ((referer (douban--annotation-create-url "123"))
         (session
          (douban--make-session
           :kind 'annotation
           :ck "topic-ck"
           :referer referer
           :host "m.douban.com"
           :state
           '(:upload-field "upload_auth_token"
             :upload-token "topic-token")))
         (source "https://example.org/remote.webp")
         captured)
    (should (douban--topic-kind-p 'annotation))
    (should
     (equal
      (douban--upload-common-fields session)
      '(("ck" . "topic-ck")
        ("primary_color" . "")
        ("upload_auth_token" . "topic-token"))))
    (cl-letf
        (((symbol-function 'douban--download-image-url)
          (lambda (&rest _arguments)
            (ert-fail "annotation 远程图应使用 topic fetch_photo")))
         ((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (setq captured (list method url arguments))
            '(:status 200
              :json
              (:r 0
               :photo
               (:id 91
                :url
                "https://img1.doubanio.com/view/group_topic/l/public/p91.webp"))))))
      (let* ((photo (douban--upload-image-url session source))
             (url (plist-get photo :url))
             (data
              (douban--normalized-image-data
               url "图注" photo 'annotation)))
        (should (equal (plist-get data :id) "91"))
        (should (equal (plist-get data :src) url))
        (should (equal (plist-get data :raw_src) url))
        (should (equal (plist-get data :caption) "图注"))))
    (pcase-let ((`(,method ,url ,arguments) captured))
      (should (equal method "POST"))
      (should (equal url douban--topic-fetch-image-endpoint))
      (should (eq (plist-get arguments :session) session))
      (let ((headers (plist-get arguments :extra-headers)))
        (should (equal (cdr (assoc-string "Referer" headers)) referer))
        (should (equal (cdr (assoc-string "X-CSRF-TOKEN" headers))
                       "topic-ck"))))))

(ert-deftest douban-test-topic-image-upload-uses-shared-multipart-contract ()
  (let* ((session
          (douban--make-session
           :kind 'status
           :ck "topic-ck"
           :referer douban--status-home-url
           :host "m.douban.com"
           :state
           '(:upload-field "upload_auth_token"
             :upload-token "topic-token")))
         (bytes
          (unibyte-string 137 80 78 71 13 10 26 10))
         captured)
    (cl-letf
        (((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (setq captured (list method url arguments))
            '(:status 200
              :body
              "{\"r\":0,\"photo\":{\"id\":42,\"url\":\"https://img1.doubanio.com/view/group_topic/l/public/p42.webp\"}}"
              :json
              (:r 0
               :photo
               (:id 42
                :url
                "https://img1.doubanio.com/view/group_topic/l/public/p42.webp"))))))
      (should
       (equal
        (plist-get
         (douban--upload-image-bytes
          session bytes "image/png")
         :id)
        42)))
    (should (equal (car captured) "POST"))
    (should
     (equal (cadr captured) douban--topic-image-endpoint))
    (let* ((arguments (nth 2 captured))
           (body (plist-get arguments :body))
           (headers (plist-get arguments :extra-headers)))
      (should (eq (plist-get arguments :session) session))
      (should (plist-get arguments :raw-body))
      (should
       (string-prefix-p
        "multipart/form-data; boundary="
        (plist-get arguments :content-type)))
      (should (string-match-p "name=\"image_file\"" body))
      (should (string-match-p "filename=\"image.png\"" body))
      (should
       (string-match-p
        (regexp-quote "name=\"ck\"\r\n\r\ntopic-ck\r\n")
        body))
      (should
       (string-match-p
        (regexp-quote
         "name=\"primary_color\"\r\n\r\n\r\n")
        body))
      (should
       (string-match-p
        (regexp-quote
         "name=\"upload_auth_token\"\r\n\r\ntopic-token")
        body))
      (should (string-match-p (regexp-quote bytes) body))
      (should
       (equal
        (cdr (assoc-string "Referer" headers))
        douban--status-home-url))
      (should
       (equal
        (cdr (assoc-string "Origin" headers))
        "https://www.douban.com")))))

(ert-deftest douban-test-topic-remote-image-uses-fetch-photo-api ()
  (let* ((session
          (douban--make-session
           :kind 'status
           :ck "topic-ck"
           :referer douban--status-home-url
           :host "m.douban.com"))
         (source "https://example.org/remote.webp")
         captured)
    (cl-letf
        (((symbol-function 'douban--download-image-url)
          (lambda (&rest _arguments)
            (ert-fail
             "topic 远程图应交给豆瓣 fetch_photo，不应由本地下载")))
         ((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (setq captured (list method url arguments))
            '(:status 200
              :body
              "{\"r\":0,\"photo\":{\"id\":91,\"url\":\"https://img1.doubanio.com/view/group_topic/l/public/p91.webp\"}}"
              :json
              (:r 0
               :photo
               (:id 91
                :url
                "https://img1.doubanio.com/view/group_topic/l/public/p91.webp"))))))
      (should
       (equal
        (plist-get
         (douban--upload-image-url session source)
         :id)
        91)))
    (should (equal (car captured) "POST"))
    (should
     (equal
      (cadr captured)
      douban--topic-fetch-image-endpoint))
    (let* ((arguments (nth 2 captured))
           (payload
            (json-parse-string
             (plist-get arguments :body)
             :object-type 'plist))
           (headers (plist-get arguments :extra-headers)))
      (should (eq (plist-get arguments :session) session))
      (should
       (equal
        (plist-get arguments :content-type)
        "application/json;charset=utf-8"))
      (should (equal (plist-get payload :photo_url) source))
      (should
       (equal
        (cdr (assoc-string "X-CSRF-TOKEN" headers))
        "topic-ck"))
      (should
       (equal
        (cdr (assoc-string "Referer" headers))
        douban--status-home-url))
      (should
       (equal
        (cdr (assoc-string "Origin" headers))
        "https://www.douban.com")))))

(ert-deftest douban-test-topic-photo-id-only-accepts-topic-cdn-path ()
  (dolist
      (case
       '(("https://img1.doubanio.com/view/group_topic/l/public/p42.webp"
          "42")
         ("https://doubanio.com/view/group_topic/raw/public/p7.jpeg"
          "7")))
    (should
     (equal
      (douban--topic-photo-id-from-url (car case))
      (cadr case)))
    (should
     (equal
      (plist-get
       (douban--normalized-image-data
        (car case) "" nil 'status)
       :id)
      (cadr case))))
  (dolist
      (url
       '("https://img1.doubanio.com/view/status/l/public/p42.webp"
         "https://img1.doubanio.com/view/subject/l/public/s42.webp"
         "https://img1.doubanio.com/view/photo/l/public/p42.webp"
         "https://img1.doubanio.com/view/group_topic/l/public/s42.webp"
         "https://img1.doubanio.com/view/group_topic/l/public/p0.webp"
         "http://img1.doubanio.com/view/group_topic/l/public/p42.webp"
         "https://example.org/view/group_topic/l/public/p42.webp"))
    (should-not (douban--topic-photo-id-from-url url))
    (should-error
     (douban--normalized-image-data url "" nil 'status)
     :type 'error)))

(ert-deftest douban-test-topic-rewrite-trusts-only-topic-cdn-images ()
  (let* ((source
          "https://img1.doubanio.com/view/group_topic/l/public/p42.webp")
         (raw
          (douban--html-to-draft
           (format "<img src=\"%s\">" source)))
         (session (douban--make-session :kind 'status)))
    (cl-letf
        (((symbol-function 'douban--upload-image-url)
          (lambda (&rest _arguments)
            (ert-fail
             "group_topic CDN 图片不应重复调用 fetch_photo")))
         ((symbol-function 'douban--upload-image-bytes)
          (lambda (&rest _arguments)
            (ert-fail "CDN 图片不应走本地上传"))))
      (douban--rewrite-draft-images
       raw session default-directory))
    (let ((data
           (plist-get
            (douban-test--first-draft-entity raw)
            :data)))
      (should (equal (plist-get data :id) "42"))
      (should (equal (plist-get data :src) source))))
  (dolist
      (case
       '(("https://img1.doubanio.com/view/status/l/public/p51.webp"
          "51")
         ("https://img9.doubanio.com/view/subject/l/public/s4468484.jpg"
          "52")))
    (let* ((source (nth 0 case))
           (id (nth 1 case))
           (registered
            (format
             "https://img1.doubanio.com/view/group_topic/l/public/p%s.webp"
             id))
           (raw
            (douban--html-to-draft
             (format "<img src=\"%s\">" source)))
           (session (douban--make-session :kind 'status))
           fetched)
      (cl-letf
          (((symbol-function 'douban--upload-image-url)
            (lambda (actual-session actual-source)
              (should (eq actual-session session))
              (should (equal actual-source source))
              (setq fetched t)
              (list :id id :url registered)))
           ((symbol-function 'douban--upload-image-bytes)
            (lambda (&rest _arguments)
              (ert-fail "远程 CDN 图片不应走本地上传"))))
        (douban--rewrite-draft-images
         raw session default-directory))
      (should fetched)
      (let ((data
             (plist-get
              (douban-test--first-draft-entity raw)
              :data)))
        (should (equal (plist-get data :id) id))
        (should (equal (plist-get data :src) registered))))))

(ert-deftest douban-test-topic-image-session-keeps-cookie-scopes-separate ()
  (let* ((api-session
          (douban--make-session
           :kind 'status
           :cookies '(("api-cookie" . "m-only"))
           :ck "topic-ck"
           :referer douban--topic-post-endpoint
           :host "m.douban.com"))
         page-request
         upload-request)
    (cl-letf
        (((symbol-function 'douban--cookie-session)
          (lambda (kind url)
            (should (eq kind 'status))
            (should (equal url douban--topic-post-endpoint))
            api-session))
         ((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (should (equal url douban--status-home-url))
            '(("www-cookie" . "www-only"))))
         ((symbol-function 'douban--http)
          (lambda (method url &rest arguments)
            (setq page-request (list method url arguments))
            '(:status 200
              :headers nil
              :body
              "<script>upload_auth_token: \"page-token\"</script>")))
         ((symbol-function 'douban--http-json)
          (lambda (method url &rest arguments)
            (setq upload-request (list method url arguments))
            '(:status 200
              :headers nil
              :body
              "{\"r\":0,\"photo\":{\"id\":42,\"url\":\"https://img1.doubanio.com/view/group_topic/l/public/p42.webp\"}}"
              :json
              (:r 0
               :photo
               (:id 42
                :url
                "https://img1.doubanio.com/view/group_topic/l/public/p42.webp"))))))
      (pcase-let*
          ((`(,actual-api-session . ,page-session)
            (douban--status-sessions '(:kind status) t))
           (bytes (unibyte-string 137 80 78 71 13 10 26 10)))
        (should (eq actual-api-session api-session))
        (should-not (eq page-session api-session))
        (should
         (equal
          (douban--session-state-get
           page-session :upload-field)
          "upload_auth_token"))
        (should
         (equal
          (douban--session-state-get
           page-session :upload-token)
          "page-token"))
        (should
         (equal
          (douban--session-cookies page-session)
          '(("ck" . "topic-ck")
            ("www-cookie" . "www-only"))))
        (should (eq (douban--session-kind page-session) 'status))
        (should
         (equal
          (douban--session-referer page-session)
          douban--status-home-url))
        (should
         (equal
          (douban--session-host page-session)
          "www.douban.com"))
        (should
         (equal
          (plist-get
           (douban--upload-image-bytes
            page-session bytes "image/png")
           :id)
          42))))
    (should (equal (douban--session-host api-session) "m.douban.com"))
    (should
     (equal
      (douban--session-cookies api-session)
      '(("api-cookie" . "m-only"))))
    (pcase-let ((`(,method ,url ,arguments) page-request))
      (should (equal method "GET"))
      (should (equal url douban--status-home-url))
      (let ((page-session (plist-get arguments :session)))
        (should (equal (douban--session-host page-session)
                       "www.douban.com"))
        (should
         (equal
          (douban--session-cookies page-session)
          '(("ck" . "topic-ck")
            ("www-cookie" . "www-only"))))))
    (pcase-let ((`(,method ,url ,arguments) upload-request))
      (should (equal method "POST"))
      (should (equal url douban--topic-image-endpoint))
      (let ((upload-session (plist-get arguments :session)))
        (should-not (eq upload-session api-session))
        (should
         (equal
          (douban--session-cookies upload-session)
          '(("ck" . "topic-ck")
            ("www-cookie" . "www-only"))))))))

(ert-deftest douban-test-status-update-image-session-reads-edit-state ()
  (let* ((topic-id "495304730")
         (edit-url
          (format
           "https://www.douban.com/topic/%s/edit"
           topic-id))
         (meta
         (list
           :kind 'status
           :status-id topic-id))
         (api-session
          (douban--make-session
           :kind 'status
           :cookies '(("api-cookie" . "m-only"))
           :ck "topic-ck"
           :host "m.douban.com"))
         requested)
    (cl-letf
        (((symbol-function 'douban--cookie-session)
          (lambda (kind url)
            (should (eq kind 'status))
            (should (equal url douban--topic-post-endpoint))
            api-session))
         ((symbol-function 'douban--read-browser-cookies)
          (lambda (url)
            (should (equal url edit-url))
            '(("www-cookie" . "www-only"))))
         ((symbol-function 'douban--http)
          (lambda (method url &rest _arguments)
            (setq requested (list method url))
            (list
             :status 200
             :headers nil
             :body
             (concat
              "<script>upload_auth_token: \"page-token\"</script>\n"
              "__INIT_STATE__.topic = "
              "{\"id\":\"495304730\","
              "\"subtype\":\"personal\","
              "\"reply_limit\":\"F\","
              "\"accessible\":\"friends\","
              "\"interest_tags\":["
              "{\"id\":\"1\",\"name\":\"电影\"},"
              "{\"id\":\"2\",\"name\":\"随笔\"}],"
              "\"topic_tags\":[{\"id\":\"11\"},{\"id\":\"12\"}],"
              "\"explanation_types\":[\"spoiler\"],"
              "\"is_original\":true,"
              "\"video_info\":null,"
              "\"anthology_id\":\"82\","
              "\"image_layout\":\"horizontal\","
              "\"text\":\"{\\\"blocks\\\":[]}\"}\n"
              "__INIT_STATE__.topic.photos = "
              "[{\"id\":\"42\",\"seq_id\":\"9\"}]\n"
              "__INIT_STATE__.topic = "
              "{\"id\":\"1\",\"subtype\":\"group\"}\n"
              "__INIT_STATE__.topic.photos = [{\"id\":\"99\"}]\n")))))
      (pcase-let ((`(,actual-api-session . ,page-session)
                   (douban--status-sessions meta t)))
        (should (eq actual-api-session api-session))
        (should page-session)
        (should-not (eq page-session api-session))
        (should
         (equal
          (douban--session-state-get
           page-session :upload-token)
          "page-token"))))
    (should (equal requested (list "GET" edit-url)))
    (should (equal (douban--session-referer api-session) edit-url))
    (should
     (equal
      (douban--session-state api-session)
      '(:photos [(:id "42" :seq_id "9")]
        :image-layout "horizontal"
        :reply-limit "F"
        :accessible "friends"
        :interest-tags "电影#随笔"
        :explanation-types "spoiler"
        :original t
        :video-info :json-null
        :anthology-id "82")))
    (should-not
     (douban--session-state-get api-session :upload-token))))

(ert-deftest douban-test-status-update-without-images-still-validates-edit-state ()
  (let* ((topic-id "495304730")
         (edit-url
          (format
           "https://www.douban.com/topic/%s/edit"
           topic-id))
         (meta
         (list
           :kind 'status
           :status-id topic-id))
         (api-session
          (douban--make-session
           :kind 'status
           :cookies '(("api-cookie" . "m-only"))
           :ck "topic-ck"
           :host "m.douban.com"))
         requested)
    (cl-letf
        (((symbol-function 'douban--cookie-session)
          (lambda (_kind _url) api-session))
         ((symbol-function 'douban--read-browser-cookies)
           (lambda (_url) '(("www-cookie" . "www-only"))))
         ((symbol-function 'douban--global-upload-token)
          (lambda (&rest _arguments)
            (ert-fail
             "无图广播更新不应解析图片上传凭据")))
         ((symbol-function 'douban--http)
          (lambda (method url &rest _arguments)
            (setq requested (list method url))
            (list
             :status 200
             :headers nil
             :body
             (concat
              "__INIT_STATE__.topic = "
              "{\"id\":\"495304730\","
              "\"subtype\":\"personal\","
              "\"reply_limit\":\"A\","
              "\"accessible\":\"public\","
              "\"interest_tags\":[],"
              "\"explanation_types\":[],"
              "\"is_original\":false,"
              "\"video_info\":null}\n"
              "__INIT_STATE__.topic.photos = []\n")))))
      (pcase-let ((`(,actual-api-session . ,page-session)
                   (douban--status-sessions meta nil)))
        (should (eq actual-api-session api-session))
        (should-not page-session)))
    (should (equal requested (list "GET" edit-url)))
    (should-not
     (douban--session-state-get api-session :upload-token))
    (should
     (equal
      (douban--session-state api-session)
      '(:photos []
        :image-layout nil
        :reply-limit "A"
        :accessible "public"
        :interest-tags ""
        :explanation-types ""
        :original :json-false
        :video-info :json-null
        :anthology-id nil)))))

(ert-deftest douban-test-status-edit-state-requires-preserved-fields ()
  (let ((topic
         (concat
          "__INIT_STATE__.topic = "
          "{\"id\":\"495304730\","
          "\"subtype\":\"personal\","
          "\"reply_limit\":\"A\","
          "\"accessible\":\"public\","
          "\"interest_tags\":[],"
          "\"explanation_types\":[],"
          "\"is_original\":false,"
          "\"video_info\":null}\n")))
    (dolist
        (photos
         '("" "__INIT_STATE__.topic.photos = {}\n"))
      (should-error
       (douban--status-edit-state
        (concat topic photos) "495304730")
       :type 'user-error)))
  (should-error
   (douban--status-edit-state
    (concat
     "__INIT_STATE__.topic = "
     "{\"id\":\"495304730\",\"subtype\":\"personal\","
     "\"reply_limit\":\"A\",\"accessible\":\"public\","
     "\"interest_tags\":[],\"is_original\":false,"
     "\"video_info\":null}\n"
     "__INIT_STATE__.topic.photos = []\n")
    "495304730")
   :type 'user-error))

(ert-deftest douban-test-note-workflow-checkpoints-id-before-one-submit ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\n"
    "title: '日记标题'\n"
    "douban:\n"
    "  note: {}\n"
    "---\n\n正文\n")
   (let* ((meta (douban--read-meta file))
          (raw '(:blocks [] :entityMap nil))
          (session (douban-test--note-session))
          (submit-count 0)
          (checkpoint-count 0)
          (published-checkpoint-count 0)
          (checkpoint-meta
           (symbol-function 'douban--checkpoint-meta))
          (checkpoint-published-content
           (symbol-function
            'douban--checkpoint-published-content))
          events)
     (should-not (fboundp 'douban--autosave-note))
     (cl-letf
         (((symbol-function 'yes-or-no-p)
           (lambda (&rest _arguments)
             (ert-fail "publish must not ask for confirmation")))
          ((symbol-function 'douban--source-html)
           (lambda (&rest _arguments)
             "<p>正文</p>"))
          ((symbol-function 'douban--html-to-draft)
           (lambda (_html) raw))
          ((symbol-function 'douban--rewrite-draft-cards)
           (lambda (actual-raw)
             (should (eq actual-raw raw))
             (push 'cards events)
             raw))
          ((symbol-function 'douban--validate-content-draft)
           (lambda (_raw _label) 2))
          ((symbol-function 'douban--note-session)
           (lambda (actual-meta)
             (should (eq (plist-get actual-meta :kind) 'note))
             (should-not (plist-member actual-meta :note-id))
             (should-not (plist-get actual-meta :note-id))
             (push 'context events)
             session))
          ((symbol-function 'douban--checkpoint-meta)
           (lambda (&rest arguments)
             (cl-incf checkpoint-count)
             (push 'checkpoint events)
             (apply checkpoint-meta arguments)))
          ((symbol-function 'douban--checkpoint-published-content)
           (lambda (&rest arguments)
             (cl-incf published-checkpoint-count)
             (push 'published-checkpoint events)
             (apply checkpoint-published-content arguments)))
          ((symbol-function 'douban--rewrite-draft-images)
           (lambda
               (actual-raw actual-session base-directory)
             (should (eq actual-raw raw))
             (should (eq actual-session session))
             (should
              (equal
               base-directory
               (file-name-directory (expand-file-name file))))
             (let ((saved (douban--read-meta file)))
               (should (plist-member saved :note-id))
               (should (equal (plist-get saved :note-id) "42")))
             (push 'images events)
             raw))
          ((symbol-function 'douban--submit-note)
           (lambda (actual-meta actual-raw actual-session title privacy)
             (cl-incf submit-count)
             (should (eq actual-raw raw))
             (should (eq actual-session session))
             (should (equal title "日记标题"))
             (should (equal privacy "P"))
             (should (equal (plist-get actual-meta :note-id) "42"))
             (should (= checkpoint-count 1))
             (should (= published-checkpoint-count 0))
             (let ((saved (douban--read-meta file)))
               (should (equal (plist-get saved :note-id) "42")))
             (push 'submit events)
             '(:id "42"
               :url "https://www.douban.com/note/42/"))))
       (should (equal (douban--publish-note-file file meta) "42")))
     (should (= submit-count 1))
     (should (= checkpoint-count 1))
     (should (= published-checkpoint-count 0))
     (should
      (equal
       (nreverse events)
       '(cards context checkpoint images submit)))
     (let ((saved (douban--read-meta file)))
       (should (equal (plist-get saved :note-id) "42"))))))

(ert-deftest douban-test-note-update-workflow-reuses-id ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\n"
    "title: '原日记标题'\n"
    "douban:\n"
    "  note:\n"
    "    id: '42'\n"
    "---\n\n更新后的正文\n")
   (let* ((meta (douban--read-meta file))
          (raw '(:blocks [] :entityMap nil))
          (session
           (douban-test--note-session "page-update-action"))
          (open-count 0)
          (submit-count 0)
          (checkpoint-count 0)
          events)
     (cl-letf
         (((symbol-function 'douban--source-html)
           (lambda (&rest _arguments)
             "<p>更新后的正文</p>"))
          ((symbol-function 'douban--html-to-draft)
           (lambda (_html) raw))
          ((symbol-function 'douban--rewrite-draft-cards)
           (lambda (actual-raw)
             (should (eq actual-raw raw))
             (push 'cards events)
             raw))
          ((symbol-function 'douban--validate-content-draft)
           (lambda (_raw _label) 6))
          ((symbol-function 'douban--note-session)
           (lambda (actual-meta)
             (cl-incf open-count)
             (should (equal (plist-get actual-meta :note-id) "42"))
             (push 'context events)
             session))
          ((symbol-function 'douban--rewrite-draft-images)
           (lambda
               (actual-raw actual-session base-directory)
             (should (eq actual-raw raw))
             (should (eq actual-session session))
             (should
              (equal
               base-directory
               (file-name-directory (expand-file-name file))))
             (should (= checkpoint-count 0))
             (let ((saved (douban--read-meta file)))
               (should (equal (plist-get saved :note-id) "42")))
             (push 'images events)
             raw))
          ((symbol-function 'douban--checkpoint-meta)
           (lambda (&rest _arguments)
             (cl-incf checkpoint-count)))
          ((symbol-function 'douban--submit-note)
           (lambda (actual-meta actual-raw actual-session title privacy)
             (cl-incf submit-count)
             (should (= checkpoint-count 0))
             (should (eq actual-raw raw))
             (should (eq actual-session session))
             (should
              (equal
               (douban--session-state-get
                actual-session :action)
               "page-update-action"))
             (should (equal title "原日记标题"))
             (should (equal privacy "P"))
             (should (equal (plist-get actual-meta :note-id) "42"))
             (push 'submit events)
             '(:id "42"
               :url "https://www.douban.com/note/42/"))))
     (should (equal (douban--publish-note-file file meta) "42")))
     (should (= open-count 1))
     (should (= submit-count 1))
     (should (= checkpoint-count 0))
     (should
     (equal
       (nreverse events)
       '(cards context images submit)))
     (let ((saved (douban--read-meta file)))
       (should (equal (plist-get saved :note-id) "42"))))))

(ert-deftest douban-test-note-update-failure-does-not-recreate ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\n"
    "title: '原日记标题'\n"
    "douban:\n"
    "  note:\n"
    "    id: '42'\n"
    "---\n\n更新后的正文\n")
   (let* ((meta (douban--read-meta file))
          (raw '(:blocks [] :entityMap nil))
          (session
           (douban-test--note-session "page-update-action"))
          (open-count 0)
          (submit-count 0)
          (checkpoint-count 0)
          (published-checkpoint-count 0)
          events)
     (cl-letf
         (((symbol-function 'douban--source-html)
           (lambda (&rest _arguments)
             "<p>更新后的正文</p>"))
          ((symbol-function 'douban--html-to-draft)
           (lambda (_html) raw))
          ((symbol-function 'douban--rewrite-draft-cards)
           (lambda (actual-raw)
             (should (eq actual-raw raw))
             (push 'cards events)
             raw))
          ((symbol-function 'douban--validate-content-draft)
           (lambda (_raw _label) 6))
          ((symbol-function 'douban--note-session)
           (lambda (actual-meta)
             (cl-incf open-count)
             (unless
                 (equal (plist-get actual-meta :note-id) "42")
               (ert-fail
                "update failure must not open the create session"))
             (push 'context events)
             session))
          ((symbol-function 'douban--rewrite-draft-images)
           (lambda (&rest _arguments)
             (push 'images events)
             raw))
          ((symbol-function 'douban--checkpoint-meta)
           (lambda (&rest _arguments)
             (cl-incf checkpoint-count)))
          ((symbol-function 'douban--submit-note)
           (lambda (actual-meta _raw actual-session _title _privacy)
             (cl-incf submit-count)
             (should (equal (plist-get actual-meta :note-id) "42"))
             (should
              (equal
               (douban--session-state-get
                actual-session :action)
               "page-update-action"))
             (push 'submit events)
             (user-error "豆瓣拒绝更新")))
          ((symbol-function 'douban--checkpoint-published-content)
           (lambda (&rest _arguments)
             (cl-incf published-checkpoint-count)
             (ert-fail
              "failed update must not checkpoint a publish result"))))
       (should-error
        (douban--publish-note-file file meta)
        :type 'user-error))
     (should (= open-count 1))
     (should (= submit-count 1))
     (should (= checkpoint-count 0))
     (should (= published-checkpoint-count 0))
     (should
     (equal
       (nreverse events)
       '(cards context images submit)))
     (let ((saved (douban--read-meta file)))
       (should (equal (plist-get saved :note-id) "42"))))))

(ert-deftest douban-test-status-publish-submits-once-and-writes-result ()
  (douban-test--with-temp-file
   ".md"
   "---\ndouban:\n  status: {}\n---\n\n广播正文\n"
   (let ((meta (douban--read-meta file))
         (raw (douban-test--status-raw "广播正文"))
         (session
          (douban--make-session
           :kind 'status
           :ck "status-ck"
           :referer douban--status-home-url
           :host "m.douban.com"))
         (submit-count 0))
     (cl-letf
         (((symbol-function 'yes-or-no-p)
           (lambda (&rest _arguments)
             (ert-fail "publish must not ask for confirmation")))
          ((symbol-function 'douban--source-html)
           (lambda (_file) "<p>广播正文</p>"))
          ((symbol-function 'douban--html-to-draft)
           (lambda (html)
             (should (equal html "<p>广播正文</p>"))
             raw))
          ((symbol-function 'douban--rewrite-draft-cards)
           (lambda (actual-raw)
             (should (eq actual-raw raw))
             raw))
          ((symbol-function 'douban--status-sessions)
           (lambda (actual-meta images-p)
             (should (eq actual-meta meta))
             (should-not images-p)
             (cons session nil)))
          ((symbol-function 'douban--submit-status)
           (lambda (actual-meta actual-session actual-raw)
             (cl-incf submit-count)
             (should (eq actual-meta meta))
             (should (eq actual-session session))
             (should (eq actual-raw raw))
             (should-not (plist-get actual-meta :anthology-id))
             (let ((saved (douban--read-meta file)))
               (should-not (plist-member saved :status-id))
               (should-not (plist-get saved :status-id)))
             '(:id "7003"))))
       (should (equal (douban--publish-status-file file meta) "7003")))
     (should (= submit-count 1))
     (let ((saved (douban--read-meta file)))
       (should (equal (plist-get saved :status-id) "7003"))))))

(ert-deftest douban-test-status-update-reuses-topic-id ()
  (douban-test--with-temp-file
   ".md"
   (concat
    "---\n"
    "douban:\n"
    "  status:\n"
    "    id: '7003'\n"
    "---\n\n更新后的广播正文\n")
   (let* ((meta (douban--read-meta file))
          (raw (douban-test--status-raw "更新后的广播正文"))
          (session
           (douban--make-session
            :kind 'status
            :ck "status-ck"
            :referer "https://www.douban.com/topic/7003/edit"
            :host "m.douban.com"))
          (submit-count 0))
     (cl-letf
         (((symbol-function 'yes-or-no-p)
           (lambda (&rest _arguments)
             (ert-fail "更新不能询问确认")))
          ((symbol-function 'douban--source-html)
           (lambda (_file) "<p>更新后的广播正文</p>"))
          ((symbol-function 'douban--html-to-draft)
           (lambda (_html) raw))
          ((symbol-function 'douban--rewrite-draft-cards)
           (lambda (actual-raw)
             (should (eq actual-raw raw))
             raw))
          ((symbol-function 'douban--status-sessions)
           (lambda (actual-meta images-p)
             (should (eq actual-meta meta))
             (should-not images-p)
             (cons session nil)))
          ((symbol-function 'douban--submit-status)
           (lambda (actual-meta actual-session actual-raw)
             (cl-incf submit-count)
             (should (eq actual-meta meta))
             (should (eq actual-session session))
             (should (eq actual-raw raw))
             '(:id "7003")))
          ((symbol-function 'douban--checkpoint-published-content)
           (lambda (&rest _arguments)
             (ert-fail "广播更新不能写入新的发布标识"))))
       (should (equal (douban--publish-status-file file meta) "7003")))
     (should (= submit-count 1))
     (let ((saved (douban--read-meta file)))
       (should (equal (plist-get saved :status-id) "7003"))))))

(ert-deftest douban-test-upload-response-rejects-missing-photo ()
  (should-error
   (douban--upload-response-photo
    '(:status 200
      :body "{\"r\":0}"
      :json (:r 0)))
   :type 'error))

(ert-deftest douban-test-note-response-rejects-unknown-success-json ()
  (let ((session (douban-test--note-session)))
    (should
     (equal
      (douban--note-response-result
       '(:status 200
         :body "{\"r\":false}"
         :json (:r :json-false))
       session nil)
      '(:id "42"
        :url "https://www.douban.com/note/42/")))
    (dolist
        (response
         '((:status 200
            :body "{\"unexpected\":true}"
            :json (:unexpected t))
           (:status 200
            :body
            "{\"error\":false,\"url\":\"/note/42/\"}"
            :json (:error :json-false :url "/note/42/"))
           (:status 200
            :body "{\"url\":\"/note/42/\"}"
            :json (:url "/note/42/"))))
      (should-error
       (douban--note-response-result response session nil)
       :type 'douban-create-result-unknown))))

(ert-deftest
    douban-test-markdown-subject-id-capf-is-lazy-fuzzy-cached-and-writes-id
    ()
  (let* ((subject
          '(:subject-id "4908885"
            :subject-type "book"
            :title "局外人"
            :summary "阿尔贝·加缪 / 上海译文出版社"))
         (label (douban--subject-candidate-label subject))
         (source
          (generate-new-buffer
           " *douban-subject-id-capf-source*"))
         (searches 0))
    (unwind-protect
        (with-current-buffer source
          (setq buffer-file-name "/tmp/review.md")
          (insert
           (concat
            "---\n"
            "title: '长评'\n"
            "douban:\n"
            "  review:\n"
            "    subject-id: 加缪\n"
            ;; The dependency deliberately follows the completed field.
            "    subject-type: book\n"
            "---\n\n"
            "正文\n"))
          (goto-char (point-min))
          (search-forward "subject-id: 加缪")
          (cl-letf
              (((symbol-function 'douban--search-subjects)
                (lambda (query subject-type)
                  (cl-incf searches)
                  (should (equal query "加缪"))
                  (should (equal subject-type "book"))
                  (list subject))))
            (let* ((capf
                    (douban-metadata-completion-at-point))
                   (start (nth 0 capf))
                   (end (nth 1 capf))
                   (table (nth 2 capf))
                   (properties (nthcdr 3 capf))
                   (exit-function
                    (plist-get properties :exit-function)))
              (should capf)
              (should (= searches 0))
              (should
               (equal
                (buffer-substring-no-properties start end)
                "加缪"))
              ;; Remote search is relevance based.  Its label need not begin
              ;; with the query, so the completion table must not filter this
              ;; valid result out a second time using basic prefix matching.
              (should-not (string-prefix-p "加缪" label))
              (should
               (equal
                (all-completions "加缪" table)
                (list label)))
              (should (= searches 1))
              (should
               (equal
                (all-completions "加缪" table)
                (list label)))
              (should (= searches 1))
              (should
               (eq
                (plist-get properties :exclusive)
                t))
              (should-not
               (plist-member properties :company-prefix-length))
              (delete-region start end)
              (goto-char start)
              (insert label)
              ;; Completion frontends may invoke the callback outside the
              ;; source buffer.  The callback must follow its saved markers.
              (with-temp-buffer
                (funcall exit-function label 'finished))
              (should
               (string-match-p
                "^    subject-id: '4908885'$"
                (buffer-string)))
              (let ((meta (douban--current-buffer-meta)))
                (should
                 (equal
                  (plist-get meta :subject-id)
                  "4908885"))
                (should
                 (equal
                  (plist-get meta :subject-type)
                  "book"))))))
      (when (buffer-live-p source)
        (kill-buffer source)))))


(ert-deftest douban-test-annotation-subject-id-capf-is-fixed-to-book ()
  (let* ((subject
          '(:subject-id "4908885"
            :subject-type "book"
            :title "局外人"))
         (label (douban--subject-candidate-label subject))
         (searches 0))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/annotation.md")
      (insert
       (concat
        "---\ntitle: 笔记\ndouban:\n"
        "  annotation:\n"
        "    subject-id: 加缪\n"
        "---\n"))
      (goto-char (point-min))
      (search-forward "subject-id: 加缪")
      (cl-letf
          (((symbol-function 'douban--search-subjects)
            (lambda (query subject-type)
              (cl-incf searches)
              (should (equal query "加缪"))
              (should (equal subject-type "book"))
              (list subject))))
        (let* ((capf (douban-metadata-completion-at-point))
               (table (nth 2 capf)))
          (should capf)
          (should (equal (all-completions "加缪" table) (list label)))
          (should (= searches 1)))))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/annotation.md")
      (insert
       (concat
        "---\ndouban:\n  annotation:\n"
        "    id: '456'\n"
        "    subject-id: 加缪\n---\n"))
      (goto-char (point-min))
      (search-forward "subject-id: 加缪")
      (cl-letf
          (((symbol-function 'douban--search-subjects)
            (lambda (&rest _arguments)
              (ert-fail "已有 annotation ID 时不得重新搜索图书"))))
        (let ((capf (douban-metadata-completion-at-point)))
          (when capf
            (should-not
             (all-completions "加缪" (nth 2 capf)))))))))

(ert-deftest douban-test-subject-id-capf-isolates-cache-by-subject-type ()
  (let* ((book
          '(:subject-id "1"
            :subject-type "book"
            :title "同名条目"))
         (movie
          '(:subject-id "2"
            :subject-type "movie"
            :title "同名条目"))
         (old-label (douban--subject-candidate-label book))
         (searches 0))
    (with-temp-buffer
      (setq
       douban--subject-completion-cache
       (list
        :subject-type "book"
        :query "同名"
        :subjects (list book)))
      (cl-letf
          (((symbol-function 'douban--search-subjects)
            (lambda (query subject-type)
              (cl-incf searches)
              (should (equal query old-label))
              (should (equal subject-type "movie"))
              (list movie))))
        (let ((entries
               (douban--subject-completion-entries-for-query
                (current-buffer) "movie" old-label)))
          (should (= searches 1))
          (should
           (equal
            (plist-get (cdar entries) :subject-type)
            "movie"))
          (should
           (equal
            (plist-get
             douban--subject-completion-cache
             :subject-type)
            "movie")))))))

(ert-deftest douban-test-subject-id-capf-rejects-wrong-context-without-search ()
  (dolist
      (case
       '(("/tmp/review.md"
         "---\ndouban:\n  review:\n    subject-id: 加缪\n---\n"
          "subject-id: 加缪")
         ("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-type: book\n    nested:\n      subject-id: 加缪\n---\n"
          "subject-id: 加缪")
         ("/tmp/status.md"
          "---\ndouban:\n  status:\n    subject-id: 加缪\n---\n"
          "subject-id: 加缪")))
    (with-temp-buffer
      (setq buffer-file-name (nth 0 case))
      (insert (nth 1 case))
      (goto-char (point-min))
      (search-forward (nth 2 case))
      (cl-letf
          (((symbol-function 'douban--search-subjects)
            (lambda (&rest _arguments)
              (ert-fail
               "wrong metadata context must not search subjects"))))
        (should-not
         (douban-metadata-completion-at-point))))))

(ert-deftest
    douban-test-subject-id-capf-does-not-search-existing-review-or-numeric-id
    ()
  (dolist
      (case
       '(("/tmp/review.md"
          "---\ndouban:\n  review:\n    id: '77'\n    subject-id: 加缪\n    subject-type: book\n---\n"
          "加缪")
         ("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-id: '2046'\n    subject-type: movie\n---\n"
          "2046")))
    (with-temp-buffer
      (setq buffer-file-name (nth 0 case))
      (insert (nth 1 case))
      (goto-char (point-min))
      (search-forward (nth 2 case))
      (let ((searches 0))
        (cl-letf
            (((symbol-function 'douban--search-subjects)
              (lambda (&rest _arguments)
                (cl-incf searches)
                nil)))
          (let ((capf
                 (douban-metadata-completion-at-point)))
            ;; Returning nil is preferred, but a deliberately empty exclusive
            ;; CAPF is also acceptable as long as enumeration stays offline.
            (when capf
              (should-not
               (all-completions
                (buffer-substring-no-properties
                 (nth 0 capf) (nth 1 capf))
                (nth 2 capf))))
            (should (= searches 0))))))))

(ert-deftest douban-test-game-platforms-validates-normalizes-and-deduplicates ()
  (let ((requests 0)
        request-options
        (payload
         `((id . 123)
           (type . "game")
           (platforms
            . ,(vector
                '((id . 1)
                  (name . "PC")
                  (cn_name . "电脑")
                  (abbreviation . "PC"))
                '((id . "2")
                  (name . "PlayStation 5")
                  (cn_name . "PlayStation 5")
                  (abbreviation . "PS5"))
                ;; A duplicate ID is valid but the first occurrence wins.
                '((id . 1)
                  (name . "Duplicate PC")
                  (cn_name . "重复电脑")
                  (abbreviation . "DPC")))))))
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method url &rest options)
            (cl-incf requests)
            (setq request-options options)
            (should (equal method "GET"))
            (should (string-match-p "123" url))
            (make-plz-response
             :status 200
             :body (json-encode payload)))))
      (should
       (equal
        (douban--game-platforms "123")
        '((:id "1"
           :name "PC"
           :cn-name "电脑"
           :abbreviation "PC")
          (:id "2"
           :name "PlayStation 5"
           :cn-name "PlayStation 5"
           :abbreviation "PS5"))))
      (should (= requests 1))
      (let ((headers (plist-get request-options :headers)))
        (should
         (equal
          (cdr (assoc-string "Accept" headers t))
          "application/json"))
        (should
         (equal
          (cdr (assoc-string "Referer" headers t))
          "https://www.douban.com/game/123/"))
        (should-not (assoc-string "Cookie" headers t)))))
  ;; An explicitly empty array is valid and distinct from a missing field.
  (cl-letf
      (((symbol-function 'douban--plz-request)
        (lambda (&rest _arguments)
          (make-plz-response
           :status 200
           :body
           (json-encode
            '((id . 123)
              (type . "game")
              (platforms . [])))))))
    (should-not (douban--game-platforms "123"))))

(ert-deftest douban-test-game-platforms-rejects-invalid-json-shapes ()
  (let (payload)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (&rest _arguments)
            (make-plz-response
             :status 200
             :body (json-encode payload)))))
      (dolist
          (invalid
           (list
            ;; The response must bind both the requested subject and game type.
            '((type . "game") (platforms . []))
            '((id . 124) (type . "game") (platforms . []))
            '((id . 123) (type . "book") (platforms . []))
            '((id . 123) (type . "game"))
            '((id . 123)
              (type . "game")
              (platforms . ((id . 1) (name . "not an array"))))
            ;; Every item needs a positive integer ID.
            `((id . 123)
              (type . "game")
              (platforms
               . ,(vector
                   '((id . 0)
                     (name . "PC")
                     (cn_name . "电脑")
                     (abbreviation . "PC")))))
            `((id . 123)
              (type . "game")
              (platforms
               . ,(vector
                   '((id . "bad")
                     (name . "PC")
                     (cn_name . "电脑")
                     (abbreviation . "PC")))))
            ;; At least one of the three display names must be nonempty.
            `((id . 123)
              (type . "game")
              (platforms
               . ,(vector
                   '((id . 1)
                     (name . "")
                     (cn_name . " ")
                     (abbreviation . "")))))))
        (setq payload invalid)
        (should-error
         (douban--game-platforms "123")
         :type 'error)))))

(ert-deftest douban-test-platform-metadata-context-finds-supported-value-shapes ()
  (dolist
      (case
       '(("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-id: '123'\n    subject-type: game\n    platforms: Pla\n---\n"
          "platforms: Pla"
          "Pla"
          markdown)
         ("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-id: '123'\n    subject-type: game\n    platforms:\n      - '1'\n      - Pla\n---\n"
          "- Pla"
          "Pla"
          markdown)))
    (with-temp-buffer
      (setq buffer-file-name (nth 0 case))
      (insert (nth 1 case))
      (goto-char (point-min))
      (search-forward (nth 2 case))
      (let ((info (douban--metadata-context)))
        (should info)
        (should (eq (plist-get info :kind) 'review))
        (should (eq (plist-get info :field) :platforms))
        (should (eq (plist-get info :slot) 'value))
        (should (eq (plist-get info :format) (nth 4 case)))
        (should
         (equal
          (buffer-substring-no-properties
           (plist-get info :completion-start)
           (plist-get info :completion-end))
          (nth 3 case)))))))

(ert-deftest
    douban-test-markdown-platform-scalar-capf-is-lazy-cached-and-writes-id
    ()
  (let ((platforms
         '((:id "1" :name "PC" :cn-name "PC" :abbreviation "PC")
           (:id "2"
            :name "PlayStation 5"
            :cn-name "PlayStation 5"
            :abbreviation "PS5")))
        (reads 0))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/review.md")
      (insert
       (concat
        "---\n"
        "title: '游戏长评'\n"
        "douban:\n"
        "  review:\n"
        "    subject-id: '123'\n"
        "    subject-type: game\n"
        "    platforms: Pla\n"
        "---\n\n"
        "正文\n"))
      (goto-char (point-min))
      (search-forward "platforms: Pla")
      (cl-letf
          (((symbol-function 'douban--game-platforms)
            (lambda (subject-id)
              (cl-incf reads)
              (should (equal subject-id "123"))
              platforms)))
        (let* ((capf
                (douban-metadata-completion-at-point))
               (start (nth 0 capf))
               (end (nth 1 capf))
               (table (nth 2 capf))
               (properties (nthcdr 3 capf))
               (exit-function
                (plist-get properties :exit-function)))
          (should capf)
          (should (= reads 0))
          (should
           (equal
            (buffer-substring-no-properties start end)
            "Pla"))
          (should
           (equal
            (all-completions "Pla" table)
            '("PlayStation 5")))
          (should (= reads 1))
          (should
           (equal
            (all-completions "Pla" table)
            '("PlayStation 5")))
          (should (= reads 1))
          (should
           (eq
            (plist-get properties :exclusive)
            t))
          (delete-region start end)
          (goto-char start)
          (insert "PlayStation 5")
          (funcall
           exit-function "PlayStation 5" 'finished)
          (should
           (string-match-p
            "^    platforms: '2'$"
            (buffer-string)))
          (should
           (equal
            (plist-get
             (douban--current-buffer-meta)
             :platforms)
            '("2"))))))))

(ert-deftest
    douban-test-markdown-platform-sequence-excludes-selected-and-edits-id
    ()
  (let ((platforms
         '((:id "1" :name "PC" :cn-name "PC" :abbreviation "PC")
           (:id "2"
            :name "PlayStation 5"
            :cn-name "PlayStation 5"
            :abbreviation "PS5")
           (:id "3"
            :name "Xbox Series"
            :cn-name "Xbox Series"
            :abbreviation "Xbox")))
        (reads 0))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/review.md")
      (insert
       (concat
        "---\n"
        "title: '游戏长评'\n"
        "douban:\n"
        "  review:\n"
        "    subject-id: '123'\n"
        "    subject-type: game\n"
        "    platforms:\n"
        "      - '1'\n"
        "      - Pla\n"
        "---\n\n"
        "正文\n"))
      (cl-letf
          (((symbol-function 'douban--game-platforms)
            (lambda (subject-id)
              (cl-incf reads)
              (should (equal subject-id "123"))
              platforms)))
        ;; Complete a new sequence item; the already selected PC is absent.
        (goto-char (point-min))
        (search-forward "- Pla")
        (let* ((capf
                (douban-metadata-completion-at-point))
               (start (nth 0 capf))
               (end (nth 1 capf))
               (table (nth 2 capf))
               (properties (nthcdr 3 capf))
               (exit-function
                (plist-get properties :exit-function))
               (candidates (all-completions "" table)))
          (should capf)
          (should (member "PlayStation 5" candidates))
          (should (member "Xbox Series" candidates))
          (should-not (member "PC" candidates))
          (should (= reads 1))
          (delete-region start end)
          (goto-char start)
          (insert "PlayStation 5")
          (funcall
           exit-function "PlayStation 5" 'finished))
        (should
         (string-match-p
          "^      - '2'$"
          (buffer-string)))
        ;; A stored protocol value must remain editable by its display name.
        ;; The other selected value stays excluded, while the current value is
        ;; allowed and the per-subject platform list remains cached.
        (goto-char (point-min))
        (search-forward "- '1")
        (let* ((capf
                (douban-metadata-completion-at-point))
               (start (nth 0 capf))
               (end (nth 1 capf))
               (table (nth 2 capf))
               (exit-function
                (plist-get
                 (nthcdr 3 capf)
                 :exit-function))
               (candidates (all-completions "1" table)))
          (should capf)
          (should (member "PC" candidates))
          (should (member "Xbox Series" candidates))
          (should-not (member "PlayStation 5" candidates))
          (should (= reads 1))
          (delete-region start end)
          (goto-char start)
          (insert "Xbox Series")
          (funcall exit-function "Xbox Series" 'finished))
        (should
         (equal
          (plist-get
           (douban--current-buffer-meta)
           :platforms)
          '("3" "2")))))))


(ert-deftest douban-test-platform-capf-rejects-wrong-context-without-network ()
  (dolist
      (case
       '(("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-id: '123'\n    subject-type: book\n    platforms: PC\n---\n"
          "platforms: PC")
         ("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-type: game\n    platforms: PC\n---\n"
          "platforms: PC")
         ("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-id: game-name\n    subject-type: game\n    platforms: PC\n---\n"
          "platforms: PC")
         ("/tmp/review.md"
          "---\ndouban:\n  review:\n    subject-id: '123'\n    subject-type: game\n    nested:\n      platforms:\n        - PC\n---\n"
          "- PC")))
    (with-temp-buffer
      (setq buffer-file-name (nth 0 case))
      (insert (nth 1 case))
      (goto-char (point-min))
      (search-forward (nth 2 case))
      (cl-letf
          (((symbol-function 'douban--game-platforms)
            (lambda (&rest _arguments)
              (ert-fail
               "wrong platform context must not access the network"))))
        (should-not
         (douban-metadata-completion-at-point))))))

(ert-deftest douban-test-remote-metadata-table-supports-basic-completion-frontend ()
  (let* ((labels
          '("局外人 — 加缪 [图书 · 1]"
            "鼠疫 — 加缪 [图书 · 2]"))
         (table
          (douban--remote-metadata-completion-table
           (lambda (_query)
             (mapcar
              (lambda (label)
                (cons label label))
              labels))
           'douban-test)))
    ;; 相关性结果不以查询开头时，标准 `try-completion' 也不能清空值槽。
    (should (equal (try-completion "加缪" table) "加缪"))
    (should (equal (all-completions "加缪" table) labels))
    (should (test-completion (car labels) table))
    (should-not (test-completion "加缪" table))
    (should
     (equal
      (completion-boundaries "加缪" table nil "")
      '(0 . 0)))))

;; Link-card regression tests.
(defun douban-test--card-html (&optional url title)
  "返回 URL 和 TITLE 对应的 Microformats2 `h-cite' HTML。"
  (format
   (concat
    "<div class=\"h-cite\">"
    "<a class=\"u-url p-name\" href=\"%s\">%s</a>"
    "</div>")
   (or url "https://book.douban.com/subject/4908885/")
   (or title "局外人")))

(ert-deftest douban-test-card-html-becomes-atomic-entity ()
  (dolist
      (url
       '("https://example.com/articles/1?from=douban"
         "http://example.net/articles/1"
         "http://book.douban.com/subject/4908885/"))
    (let* ((raw
            (douban--html-to-draft
             (douban-test--card-html url "链接标题")))
           (blocks (plist-get raw :blocks))
           (block (aref blocks 0))
           (range (aref (plist-get block :entityRanges) 0))
           (entity (douban-test--first-draft-entity raw))
           (data (plist-get entity :data)))
      (should (= (length blocks) 1))
      (should (equal (plist-get block :type) "atomic"))
      (should (equal (plist-get block :text) " "))
      (should (= (plist-get range :offset) 0))
      (should (= (plist-get range :length) 1))
      (should (= (plist-get range :key) 0))
      (should (equal (plist-get entity :type) "LINK"))
      (should (equal (plist-get entity :mutability) "IMMUTABLE"))
      (should (equal (plist-get data :url) url))
      (should (equal (plist-get data :title) "链接标题"))
      (should (equal (plist-get data :display) "atomic")))))

(ert-deftest douban-test-card-html-rejects-invalid-placement-and-url ()
  (dolist
      (html
       (list
        (format
         "<section>前%s后</section>"
         (douban-test--card-html))
        (douban-test--card-html "../subject/4908885/")
        (douban-test--card-html "//example.com/articles/1")
        (douban-test--card-html "ftp://example.com/articles/1")
        (douban-test--card-html "https:///missing-host")))
    (should-error (douban--html-to-draft html) :type 'user-error)))

(ert-deftest douban-test-card-only-draft-is-nonempty-content ()
  (let ((raw
         (douban--html-to-draft
          (douban-test--card-html))))
    ;; 原子卡片不计入长评字数，但应足以构成日记或广播正文。
    (should (= (douban--draft-character-count raw) 0))
    (should (= (douban--validate-content-draft raw "日记") 0))))

(ert-deftest douban-test-rewrite-draft-cards-follows-first-reference-order ()
  (cl-labels
      ((link
        (name)
        (list
         :type "LINK"
         :mutability "IMMUTABLE"
         :data
         (list
          :url (format "https://example.org/%s" name)
          :display "atomic"))))
    (let* ((raw
            (douban-test--entity-raw
             `(("1" . ,(link "one"))
               ("9" . ,(link "orphan"))
               ("0" . ,(link "zero"))
               ("2" . ,(link "two")))
             '("2" "0" "2")
             '("1")))
           requests)
      (cl-letf
          (((symbol-function 'douban--resolve-card)
            (lambda (url)
              (push url requests)
              (list
               :type "LINK"
               :data
               (list
                :title url
                :url url
                :display "atomic")))))
        (should (eq (douban--rewrite-draft-cards raw) raw)))
      (should
       (equal
        (nreverse requests)
        '("https://example.org/two"
          "https://example.org/zero"
          "https://example.org/one"))))))

(ert-deftest douban-test-rewrite-draft-cards-resolves-and-caches-url ()
  (let* ((source-url "http://book.douban.com/subject/4908885/")
         (canonical-url
          "https://book.douban.com/subject/4908885/")
         (raw
          (douban--html-to-draft
           (concat
            (douban-test--card-html source-url "源稿标题")
            (douban-test--card-html source-url "另一个源稿标题"))))
         (requests 0)
         (response-data
          '(:id "4908885"
            :type "book"
            :title "局外人"
            :url "https://book.douban.com/subject/4908885/"
            :cover
            "https://img9.doubanio.com/view/subject/l/public/s4468484.jpg"
            :summary "[法] 阿尔贝·加缪 / 2010 / 上海译文出版社"
            :rating (:max 10.0 :value 9.1)
            :api-only "server-value")))
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method request-url &rest options)
            (cl-incf requests)
            (should (equal method "GET"))
            (should
             (string-match-p
              "/rexxar/api/v2/get_url_info?"
              request-url))
            (should (string-match-p "[?&]need_card=1\\(?:&\\|\\'\\)"
                                    request-url))
            (should (string-match-p "[?&]editor_type=group\\(?:&\\|\\'\\)"
                                    request-url))
            (should
             (string-match-p
              (regexp-quote (url-hexify-string source-url))
              request-url))
            (should-not
             (cl-find-if
              (lambda (header)
                (string-equal
                 (downcase (format "%s" (car header)))
                 "cookie"))
              (plist-get options :headers)))
            (make-plz-response
             :status 200
             :body
             (json-encode
              (list :type "SUBJECT" :data response-data))))))
      (should (eq (douban--rewrite-draft-cards raw) raw)))
    (should (= requests 1))
    (should (= (hash-table-count (plist-get raw :entityMap)) 2))
    (maphash
     (lambda (_key entity)
       (let ((data (plist-get entity :data)))
         (should (equal (plist-get entity :type) "SUBJECT"))
         (should (equal (plist-get data :id) "4908885"))
         (should (equal (plist-get data :type) "book"))
         (should (equal (plist-get data :title) "局外人"))
         (should (equal (plist-get data :url) canonical-url))
         (should
          (equal
           (plist-get data :cover)
           (plist-get response-data :cover)))
         (should
          (equal
           (plist-get data :summary)
           (plist-get response-data :summary)))
         (should
          (= (plist-get (plist-get data :rating) :value) 9.1))
         (should (equal (plist-get data :api-only) "server-value"))
         (should (equal (plist-get data :display) "atomic"))
         (dolist (alias '(:caption :cover_url :card_subtitle))
           (should-not (plist-member data alias)))
         (should
          (equal
           data
           (append response-data '(:display "atomic"))))))
     (plist-get raw :entityMap))))

(ert-deftest douban-test-rewrite-draft-cards-keeps-link-result ()
  (let* ((source-url "http://example.com/articles/1")
         (raw
          (douban--html-to-draft
           (douban-test--card-html source-url "源稿标题")))
         (response-data
          '(:title "服务端标题"
            :url "https://example.com/articles/1"
            :cover_url "https://example.com/cover.jpg"
            :card_subtitle "服务端摘要"
            :api-only "server-value")))
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (&rest _arguments)
            (make-plz-response
             :status 200
             :body
             (json-encode
              (list :type "LINK" :data response-data))))))
      (should (eq (douban--rewrite-draft-cards raw) raw)))
    (let* ((entity (douban-test--first-draft-entity raw))
           (data (plist-get entity :data)))
      (should (equal (plist-get entity :type) "LINK"))
      (should (equal (plist-get entity :mutability) "IMMUTABLE"))
      (should
       (equal
        data
        (append response-data '(:display "atomic")))))))

(ert-deftest douban-test-rewrite-draft-cards-ignores-ordinary-links ()
  (dolist
      (url
       '("https://book.douban.com/subject/4908885/"
         "https://example.com/articles/1"))
    (let* ((raw
            (douban--html-to-draft
             (format "<p><a href=\"%s\">普通链接</a></p>" url)))
           (before (copy-tree raw)))
      (cl-letf
          (((symbol-function 'douban--plz-request)
            (lambda (&rest _arguments)
              (ert-fail "普通链接不得调用卡片解析接口"))))
        (should (eq (douban--rewrite-draft-cards raw) raw)))
      (should (equal raw before))
      (let* ((entity (douban-test--first-draft-entity raw))
             (data (plist-get entity :data)))
        (should (equal (plist-get entity :type) "LINK"))
        (should (equal (plist-get entity :mutability) "MUTABLE"))
        (should (equal (plist-get data :url) url))
        (should-not (plist-member data :display))))))

(ert-deftest douban-test-rewrite-draft-cards-rejects-invalid-result ()
  (dolist
      (result
       '((:type "URL"
          :data (:url "https://example.com/articles/1"))
         (:type "LINK"
          :data (:title "相对地址" :url "../articles/1"))
         (:type "LINK"
          :data
          (:title "不安全封面"
           :url "https://example.com/articles/1"
           :cover_url "http://example.com/cover.jpg"))
         (:type "SUBJECT"
          :data
          (:id "4908885"
           :type "book"
           :title "局外人"
           :url "http://book.douban.com/subject/4908885/"))))
    (let ((raw
           (douban--html-to-draft
            (douban-test--card-html))))
      (cl-letf
          (((symbol-function 'douban--plz-request)
            (lambda (&rest _arguments)
              (make-plz-response
               :status 200
               :body (json-encode result)))))
        (should-error
         (douban--rewrite-draft-cards raw)
         :type 'error)))))

(ert-deftest douban-test-json-endpoint-redirect-stays-with-card-protocol ()
  (let (requests)
    (cl-letf
        (((symbol-function 'douban--plz-request)
          (lambda (method url &rest options)
            (push (list method url options) requests)
            (make-plz-response
             :status 302
             :headers '(("Location" . "https://evil.example/"))
             :body ""))))
      (let* ((source-url "https://example.org/article")
             (err
              (should-error
               (douban--resolve-card source-url)
               :type 'user-error)))
        (should
         (equal
          (error-message-string err)
          (format
           "douban: 无法解析卡片 %s（HTTP 302）"
           source-url)))))
    (should (= (length requests) 1))
    (pcase-let ((`(,method ,_url ,options) (car requests)))
      (should (equal method "GET"))
      (should
       (equal
        (cdr
         (assoc-string
          "Referer" (plist-get options :headers) t))
        "https://www.douban.com/")))))
(ert-deftest douban-test-pandoc-preserves-h-cite-card ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (url
       '("https://example.com/articles/1"
         "http://example.net/articles/1"
         "http://book.douban.com/subject/4908885/"))
    (let* ((html
            (douban--pandoc-to-html
             "gfm" (douban-test--card-html url "链接标题")))
           (document
            (douban--parse-html
             (concat "<html><body>" html "</body></html>")))
           (body (car (dom-by-tag document 'body)))
           (elements
            (cl-remove-if-not #'consp (dom-children body)))
           (raw (douban--html-to-draft html))
           (entity (douban-test--first-draft-entity raw)))
      (should (= (length elements) 1))
      (should (dom-by-class body "h-cite"))
      (should (dom-by-class body "u-url"))
      (should (dom-by-class body "p-name"))
      (should (equal (plist-get entity :type) "LINK"))
      (should
       (equal
        (plist-get (plist-get entity :data) :title)
        "链接标题")))))

(ert-deftest douban-test-pandoc-card-title-is-an-ordinary-link ()
  (skip-unless (executable-find "pandoc"))
  (let* ((url "https://book.douban.com/subject/4908885/")
         (html
          (douban--pandoc-to-html
           "gfm" (format "[局外人](%s \"card\")" url)))
         (raw (douban--html-to-draft html))
         (entity (douban-test--first-draft-entity raw)))
    (should-not (string-match-p "h-cite" html))
    (should (equal (plist-get entity :type) "LINK"))
    (should (equal (plist-get entity :mutability) "MUTABLE"))
    (should (equal (plist-get (plist-get entity :data) :url) url))))


(provide 'douban-test)
;;; douban-test.el ends here
