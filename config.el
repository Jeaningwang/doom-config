;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "dong.wang"
      user-mail-address "moneatts@outlook.com")

;; 常用的 editor 配置
(setq-default
 tab-width 4 ;; tab的宽度
 scroll-margin 2 ;; add a margin when scrolling vertically
 )

;; whitespace 展示
;; ·    183    b7    MIDDLE DOT
;; ¶    182    b6    PILCROW SIGN
;; ↵    8629    21b5    DOWNWARDS ARROW WITH CORNER LEFTWARDS
;; ↩    8617    21a9    LEFTWARDS ARROW WITH HOOK
;; ⏎    9166    23ce    RETURN SYMBOL
;; ▷    9655    25b7    WHITE RIGHT POINTING TRIANGLE
;; ▶    9654    25b6    BLACK RIGHT-POINTING TRIANGLE
;; →    8594    2192    RIGHTWARDS ARROW
;; ↦    8614    21a6    RIGHTWARDS ARROW FROM BAR
;; ⇥    8677    21e5    RIGHTWARDS ARROW TO BAR
;; ⇨    8680    21e8    RIGHTWARDS WHITE ARROW
(setq whitespace-style '(face spaces tabs newline space-mark tab-mark newline-mark))
(setq whitespace-display-mappings
      '(
        (space-mark 32 [183] [46])
        (tab-mark 9 [8677 9] [92 9])
        (newline-mark 10 [9166 10])
        )
      )
(global-whitespace-mode 1)

;; 设置自动换行
(global-visual-line-mode 1)

;; Emacs 启动的时候，使窗口最大化
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq +main-font "SauceCodePro NF")
(let ((monitor-attribe (assoc 'geometry (frame-monitor-attributes))))
  (setq +display-pixel-width (nth 3 monitor-attribe))
  (setq +display-pixel-height (car(last monitor-attribe)))
  )
(setq +main-font-size (cond ((> +display-pixel-width 2560) 30)((> +display-pixel-width 1920) 24)((>= +display-pixel-width 1680) 18)(t 16)))
(setq doom-font (font-spec :family +main-font :size +main-font-size :weight 'regular)
      doom-variable-pitch-font (font-spec :family +main-font :size +main-font-size :weight 'bold)
      doom-big-font (font-spec :family +main-font :size +main-font-size :weight 'regular)
      doom-symbol-font (font-spec :family +main-font :size +main-font-size :weight 'regular)
      doom-serif-font (font-spec :family +main-font :size +main-font-size :weight 'regular)
      )


;;-------------------------------------------------------------------------------
;;-------------------------不同系统下做的配置---------------------------------------
;;-------------------------------------------------------------------------------
(cond
 ((featurep :system 'windows)
  ;; 编码
  ;;(set-terminal-coding-system 'gbk)
  ;;(modify-coding-system-alist 'process "*" 'gbk)
  ;;(setq default-terminal-coding-system 'gbk)
  ;;(setq default-process-coding-system '(gbk . gbk))
  (message "window os")
  )
 ((featurep :system 'linux)
  (message "linux os")
  )
 ((featurep :system 'macos)
  (message "mac os")
  )
 (t
  (message "未知的操作系统"))
 )

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(let ((custom-theme-list (append '(doom-outrun-electric doom-monokai-pro doom-badger doom-solarized-dark doom-old-hope doom-tokyo-night doom-material doom-xcode doom-moonlight doom-one doom-flatwhite dtsdh-light oom-henna doom-dracula doom-snazzy doom-oksolar-dark) (custom-available-themes)))
      )
  (setq custom-theme-list-remove '(light-blue manoj-dark doom-bluloco-light doom-plain-dark doom-gruvbox-light doom-oksolar-light doom-acario-light leuven doom-solarized-light doom-homage-white dichromacy adwaita modus-operandi doom-ayu-light doom-pine))
  (setq custom-theme-list-final (cl-remove-if (lambda (x) (member x custom-theme-list-remove)) custom-theme-list))
  (setq doom-theme (nth (random (length custom-theme-list-final)) custom-theme-list-final))
  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")
(setq org-directory (getenv "ORG_HOME"))

;; org roam 配置
(after! org-roam
  (if (version<= "30.2" emacs-version)
      ;; ----------------------------------------------------------------
      ;; 情况 1: Emacs 版本 >= 30.2
      ;; 直接使用标准写法 (自动加上 #+title:)
      ;; ----------------------------------------------------------------
      (setq org-roam-capture-templates
            `(("d" "default" plain "%?"
               :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                  "#+title: ${title}\n")
               :unnarrowed t)))

    ;; ----------------------------------------------------------------
    ;; 情况 2: Emacs 版本 < 30.2
    ;; 使用你之前的写法 (中文不加 #+title:，英文加)
    ;; ----------------------------------------------------------------
    (setq org-roam-capture-templates
          `(("d" "default" plain "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                ;; 注意：这里的逻辑是你原始代码的逻辑
                                ;; 即：如果是中文，只输出 title (无前缀)
                                ;;     如果是英文，输出 #+title: + title
                                "%(if (string-match-p \"[[:multibyte:]]\" \"${title}\")
                                      \"${title}\\n\"
                                      \"#+title: ${title}\\n\")")
             :unnarrowed t)))))

;; 配置 online 搜索链接
(setq +lookup-provider-url-alist
      (append '(("Doom issues"       "https://github.com/orgs/doomemacs/projects/2/views/30?filterQuery=%s")
                ("Doom discourse"    "https://discourse.doomemacs.org/search?q=%s")
                ("Google"            +lookup--online-backend-google "https://google.com/search?q=%s")
                ("Google images"     "https://www.google.com/images?q=%s")
                ("Google maps"       "https://maps.google.com/maps?q=%s")
                ("Kagi"              "https://kagi.com/search?q=%s")
                ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
                ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
                ("DevDocs.io"        "https://devdocs.io/#q=%s")
                ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
                ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
                ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
                ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
                ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
                ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s")
                ("Internet archive"  "https://web.archive.org/web/*/%s")
                ("Sourcegraph"       "https://sourcegraph.com/search?q=context:global+%s&patternType=literal")
                ("Bing"       "https://cn.bing.com/search?q=%s")
                ("Yandex"            "https://yandex.com/search/?text=%s")
                ("Yandex images"     "https://yandex.com/images/search?text=%s")
                ("Yandex maps"       "https://yandex.com/maps?text=%s"))
              (when (modulep! :lang rust)
                '(("Rust Docs" "https://doc.rust-lang.org/std/?search=%s"))))
      )

;;--------------------------------------------------------------------------
;;-------------------------Custom Face--------------------------------------
;;--------------------------------------------------------------------------
(set-face-foreground 'bold "red")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;--------------------------------------------------------------------
;;-------------------------Java---------------------------------------
;;--------------------------------------------------------------------
;; (setq lsp-java-java-path (concat (getenv "JAVA_HOME") "\\bin\\java"))
(setq lsp-java-jdt-download-url  "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz") ;; 使用旧的版本
;; java classpath
(setenv "CLASSPATH" (if (featurep :system 'windows) (concat ".;" (concat (getenv "JAVA_HOME") "\\lib\\dt.jar;") (concat (getenv "JAVA_HOME") "\\lib\\tools.jar;") (expand-file-name "hutool-all-5.4.1.jar" (concat (getenv "HOME") "\\.m2\\repository\\cn\\hutool\\hutool-all\\5.4.1\\"))) (concat ".:" (concat (getenv "JAVA_HOME") "/lib/dt.jar:") (concat (getenv "JAVA_HOME") "/lib/tools.jar:") (expand-file-name "hutool-all-5.4.1.jar" (concat (getenv "HOME") "/.m2/repository/cn/hutool/hutool-all/5.4.1/")))))
;; org java 参数
(setq org-babel-default-header-args:java `((:results . "output")(:dir . ".")(:imports . "java.lang.reflect.* java.util.stream.* cn.hutool.core.convert.* java.util.* cn.hutool.core.collection.* cn.hutool.core.lang.* cn.hutool.core.util.* cn.hutool.core.io.*") ))


;;--------------------------------------------------------------------
;;------------------------- JavaScript -------------------------------
;;--------------------------------------------------------------------
(setq org-babel-default-header-args:js '((:results . "output")))


;;--------------------------------------------------------------------
;;------------------------- C ----------------------------------------
;;--------------------------------------------------------------------
(setq org-babel-C-compiler "gcc-13")
(setq org-babel-default-header-args:C '((:results . "output")))

;;--------------------------------------------------------------------
;;------------------------- cpp --------------------------------------
;;--------------------------------------------------------------------
(setq org-babel-C++-compiler "g++-13")
;; (setq org-babel-C++-compiler "g++-14")
;; (setq org-babel-C++-compiler "clang++")
(setq org-babel-default-header-args:cpp '((:flags . "-std=c++20") (:results . "output"))) ; C++20核心特性

;;--------------------------------------------------------------------
;;-------------------------Python-------------------------------------
;;--------------------------------------------------------------------
(setq org-babel-default-header-args:python '((:results . "output")))


;;--------------------------------------------------------------------
;;------------------------- Lua --------------------------------------
;;--------------------------------------------------------------------
(setq org-babel-default-header-args:lua '((:results . "output")))
(after! lua-mode
  (setq lua-indent-level 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  )


;;--------------------------------------------------------------------
;;------------------------- Go --------------------------------------
;;--------------------------------------------------------------------
(setq org-babel-default-header-args:go '((:results . "output") (:imports . "fmt")))


;;--------------------------------------------------------------------
;;------------------------- Elisp -------------------------------------
;;--------------------------------------------------------------------
(setq org-babel-default-header-args:elisp '((:results . "output")))


;;--------------------------------------------------------------------
;;-------------------------key map------------------------------------
;;--------------------------------------------------------------------
(map!
 :leader
 ;; :desc "Switch to last buffer"
 ;; "bb"
 ;; (lambda () (interactive) (evil-switch-to-windows-last-buffer))
 :desc "Switch to last buffer"
 "SPC"
 #'evil-switch-to-windows-last-buffer
 :desc "Save all buffers"
 "fs"
 #'evil-write-all
 :desc "Translate word"
 "sw"
 (lambda (str &optional arg)
   (interactive (list (doom-thing-at-point-or-region 'word) current-prefix-arg))
   (evil-ex (format! "!fy %s" (if str str "")))
   )
 :desc "open quick calc" "oc" #'quick-calc
 ;; :desc "open calendar" "oC" #'calendar
 :desc "open calc" "oC" #'calc
 )

;; (global-set-key (kbd "<C-tab>") #'evil-switch-to-windows-last-buffer)

;;--------------------------------------------------------------------
;;-------------------------vim----------------------------------------
;;--------------------------------------------------------------------
;;; jk 配置成 Esc
(setq evil-escape-key-sequence "jk")
(setq evil-escape-delay 0.15)

(map! ;; vim
 ;;:g "SPC SPC" #'evil-switch-to-windows-last-buffer
 :nv "gh" #'evil-first-non-blank
 :nv "gl" #'evil-last-non-blank
 :n "U" #'evil-redo
 ;; 实现 pgvy 的功能
 :v "p" (lambda () (interactive)
          (evil-paste-after nil)
          (evil-yank-characters evil-visual-beginning evil-visual-end)
          (goto-char evil-visual-end)
          )
 ;; j k 使用时, 使行一直保持在屏幕中央
 :n "j" (lambda () (interactive)
          (evil-next-line)
          (evil-scroll-line-to-center nil)
          )
 :n "k" (lambda () (interactive)
          (evil-previous-line)
          (evil-scroll-line-to-center nil)
          )
 :leader :prefix ("e" . "custom") ;; 自定义的快捷键
 :desc "search node roam" "f" #'org-roam-node-find ;; 搜索 roam-node
 :desc "insert node roam" "i" #'org-roam-node-insert ;; 插入 roam-node
 )

(after! evil-org
  (map! :map evil-org-mode-map
        :nv "gh" #'evil-first-non-blank
        :nv "gl" #'evil-last-non-blank)
  )

;; 列的指示线(TODO 暂未生效)
(setq-default display-fill-column-indicator-character ?\ )
(setq-default display-fill-column-indicator-column 120)
(global-display-fill-column-indicator-mode)
(display-fill-column-indicator-mode 1)


;;-------------------------------------------------------------------------
;;------------------------- fly check -------------------------------------
;;-------------------------------------------------------------------------
(after! flycheck
  ;; 1. 关闭全局模式（双重保险）
  (global-flycheck-mode -1)
  (setq flycheck-global-modes nil)
  
  ;; 2. 移除 Doom 添加的自动开启钩子
  ;; Doom 默认会在 prog-mode, text-mode 等模式下自动开启 flycheck
  (remove-hook 'prog-mode-hook #'flycheck-mode)
  (remove-hook 'text-mode-hook #'flycheck-mode)
  (remove-hook 'conf-mode-hook #'flycheck-mode))


;;-------------------------------------------------------------------------
;;-------------------------zen mode----------------------------------------
;;-------------------------------------------------------------------------
(setq-default +zen-text-scale 0) ;; 切换到 zen mode 时, 字体变化的大小
;; (setq-default writeroom-width 100)
(defun set-writeroom-width ()
  "设置 writeroom 的宽度为总宽度的百分比"
  (setq-default writeroom-width (truncate (* (/ +display-pixel-width (frame-char-width)) 0.7)))
  )
(after! writeroom-mode
  (add-hook! 'writeroom-mode-hook
             :append #'set-writeroom-width
             #'writeroom-toggle-mode-line ;显示底部的状态栏
             )
  )


;;--------------------------------------------------------------------------
;;-------------------------mode line----------------------------------------
;;--------------------------------------------------------------------------
;; (use-package! doom-modeline
;;   :custom-face
;;   (mode-line ((t (:height 1.0))))
;;   (mode-line-inactive ((t (:height 0.95))))
;;   :custom
;;   (doom-modeline-height 16)
;;   (doom-modeline-bar-width 4)
;;   (doom-modeline-lsp nil)
;;   (doom-modeline-modal-icon t)
;;   (doom-modeline-minor-modes nil)
;;   (doom-modeline-major-mode-icon t)
;;   (doom-modeline-buffer-file-name-style 'truncate-with-project)
;;   (defun doom-modeline-conditional-buffer-encoding ()
;;     "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
;;     (setq-local doom-modeline-buffer-encoding
;;                 (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
;;                                    '(coding-category-undecided coding-category-utf-8))
;;                              (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
;;                   t)))

;;   (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding) (doom-modeline-buffer-state-icon t))

;; (setq display-time-24hr-format t                ;; Display 24 Hrs rather than 12
;;       display-time-default-load-average nil     ;; Do not display my CPU Load
;;       )
(setq display-time-format "%Y-%m-%d(%V-%u) %H:%M:%S"
      display-time-interval 1
      display-time-default-load-average nil     ;; Do not display my CPU Load
      doom-modeline-buffer-file-name-style 'file-name-with-project
      )
(display-time-mode 1)

;;---------------------------------------------------------------------------
;;-------------------------  Calendar ---------------------------------------
;;---------------------------------------------------------------------------
(setq calendar-week-start-day 1) ;; 从周一开始
(setq calendar-mark-diary-entries-flag 't)

;; 农历
(use-package! cal-china-x
  :after calendar
  :config
  (setq calendar-chinese-all-holidays-flag t)
  )


;;---------------------------------------------------------------------------
;;--------------------------- Agenda ----------------------------------------
;;---------------------------------------------------------------------------
;; agenda 每月任务导出
(defun my/export-done-tasks-with-duration ()
  "遍历所有 Agenda 文件，提取过去 30 天内完成的任务，计算执行跨度并保存。"
  (interactive)
  (let* ((days-back 30)
         (output-file (expand-file-name "~/Downloads/monthly_done_detailed.txt"))
         ;; 计算 30 天前的秒数
         (since-seconds (float-time (time-subtract (current-time) (days-to-time days-back))))
         (tasks '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           ;; 遍历所有标题行
           (while (re-search-forward org-complex-heading-regexp nil t)
             (let* ((todo-state (org-get-todo-state))
                    (heading (org-get-heading t t t t))
                    (closed-str (org-entry-get (point) "CLOSED"))
                    (sched-str (org-entry-get (point) "SCHEDULED")))
               ;; 只有状态是 DONE 且有 CLOSED 时间戳才处理
               (when (and (string-equal todo-state "DONE") closed-str)
                 (let* ((closed-time (org-time-string-to-time closed-str))
                        (closed-seconds (float-time closed-time)))
                   ;; 检查完成时间是否在 30 天内
                   (when (> closed-seconds since-seconds)
                     (let ((duration-info "无计划时间")
                           (sched-out "N/A"))
                       ;; 如果有计划时间，进行计算
                       (when sched-str
                         (let* ((sched-time (org-time-string-to-time sched-str))
                                (diff-hours (/ (- closed-seconds (float-time sched-time)) 3600.0)))
                           (setq sched-out sched-str)
                           (setq duration-info 
                                 (if (>= (abs diff-hours) 24)
                                     (format "执行跨度: %.1f 天" (/ diff-hours 24.0))
                                   (format "执行跨度: %.1f 小时" diff-hours)))))
                       ;; 存入列表
                       (push (format "* 任务: %s\n  - 计划: %s\n  - 完成: %s\n  - %s"
                                     heading sched-out closed-str duration-info)
                             tasks))))))))))
      ;; 写入文件逻辑
      (if tasks
          (with-temp-file output-file
            (insert (format "#+TITLE: 月度任务执行报告\n#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
            (insert "# 说明: 跨度 = 完成时间 - 计划时间\n\n")
            (insert (mapconcat #'identity (reverse tasks) "\n\n")))
        (message "过去 %d 天内没有发现符合条件的 DONE 任务。" days-back))
      (when tasks
        (message "导出完成！共 %d 项，文件: %s" (length tasks) output-file)))))

(defun my/ai-gemini-monthly-report ()
  "导出过去 30 天已完成任务并请求 Gemini 进行总结分析，结果显示在独立 Buffer 中。"
  (interactive)
  ;; 1. 执行导出函数 (确保此函数已定义)
  (my/export-done-tasks-with-duration)
  
  (let* ((file-path (expand-file-name "~/Downloads/monthly_done_detailed.txt"))
         (data-content (if (file-exists-p file-path)
                           (with-temp-buffer
                             (insert-file-contents file-path)
                             (buffer-string))
                         nil))
         ;; 准备输出 Buffer
         (out-buf-name "*Gemini Monthly Review*")
         (output-buffer (get-buffer-create out-buf-name)))
    
    (if data-content
        (progn
          ;; 自动开启代理
          (unless (bound-and-true-p url-proxy-services)
            (my/toggle-proxy))
          
          ;; 准备输出 Buffer 的状态
          (with-current-buffer output-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (org-mode)
              (insert "#+TITLE: Gemini 月度复盘报告\n")
              (insert "#+SUBTITLE: 生成时间: " (format-time-string "%Y-%m-%d %H:%M") "\n\n")
              (insert "正在等待 Gemini 教练的复盘结果...\n\n")))
          
          ;; 弹出窗口显示 Buffer
          (display-buffer output-buffer)

          ;; 发起请求
          (gptel-request
              (format "你是一个专业的生产力专家。请分析以下这份任务执行报告：\n\n%s\n\n请从『时间利用率』、『计划达成度』和『下月改进建议』三个维度给我一个毒舌但有用的复盘。\n\n并通过我这个月完成的任务内容，制定出我下个月的计划。" 
                      data-content)
            :buffer output-buffer
            :position (with-current-buffer output-buffer (point-max)) ;; 强制指定插入到目标 Buffer 的末尾
            :system "你是一个严格的生产力导师，擅长从数据中发现用户拖延的借口。也擅长制定计划。")
          
          (message "已成功发起请求，请查看 %s" out-buf-name))
      (message "错误：找不到导出文件，请检查路径。"))))

(map! :leader
      (:prefix ("n" . "notes")
       :desc "Gemini 月末总结" "z" #'my/ai-gemini-monthly-report))

(after! cal-china-x
  (setq calendar-holidays cal-china-x-chinese-holidays ;; 使用中国的节日来代替原来的默认节日
        )
  )

(after! org
  (setq
   ;; 默认显示周视图
   org-agenda-start-on-weekday 1 ;; 从周一开始
   org-agenda-span 'week
   org-agenda-start-day nil

   ;; org-log-done 'time ;; 当任务状态切换成 done 时，添加一个日期
   org-log-done 'note ;; 记录时间并提示输入备注
   org-log-into-drawer t ;; 将状态变更记录放入 :LOGBOOK: 抽屉
   ))

(after! org
  (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

(setq org-agenda-custom-commands
      '(("w" "周计划 (周一开启)"
         agenda ""
         ((org-agenda-span 'week)
          (org-agenda-start-on-weekday 1)
          (org-agenda-start-day nil))) ;; nil 会自动寻找当前周的周一

        ("m" "月看板 (1号开启)"
         agenda ""
         ((org-agenda-span 'month)
          (org-agenda-start-day (format-time-string "%Y-%m-01"))))))

(setq org-agenda-include-diary t ;; agenda display diary(在 agenda 中显示 diary)
      )

(defun my/filter-chinese-date-str (str)
  "过滤掉字符串中的节气和带括号的星座。"
  (let* (;; 1. 定义要过滤的节气（这里可以根据需要补充）
         (solar-terms-regexp (regexp-opt (append cal-china-x-solar-term-name nil)))
         ;; 2. 定义星座的正则模式：匹配括号及其内部内容
         (horoscope-regexp (regexp-opt (mapcar (lambda (x) (nth 2 x)) cal-china-x-horoscope-name)))
         ;; 执行替换
         (step1 (replace-regexp-in-string solar-terms-regexp "" str))
         (step2 (replace-regexp-in-string "(.*?)" "" step1)))
    ;; trim 掉可能残余的空格
    (string-trim step2)))

(defun my/align-str (str width)
  "将字符串 STR 填充到指定 WIDTH（考虑中文宽度）。"
  (let ((current-width (string-width str)))
    (concat str (make-string (max 0 (- width current-width)) ?\s))))

;; display Chinese date
(after! org
  (setq org-agenda-format-date #'zeroemacs/org-agenda-format-date-aligned)

  (defun zeroemacs/org-agenda-format-date-aligned (date)
    "显示 YYYY-MM-DD 星期 农历月日 以及农历节日。"
    (require 'cal-china-x)
    (let* ((year (nth 2 date))
           (month (car date))
           (day (cadr date))
           ;; 1. 获取星期名
           (dayname (aref cal-china-x-days (calendar-day-of-week date)))
           ;; 2. 获取农历月日字符串
           (cn-date-str (my/filter-chinese-date-str (cal-china-x-chinese-date-string date)))
           ;; 3. 获取节气
           (cn-solar-term (cal-china-x-get-solar-term date))
           ;; 4. 获取星座
           (cn-horoscope (cal-china-x-get-horoscope month day))
           )
      
      ;; 格式化输出
      (format "%04d-%02d-%02d %s %s %s %s"
              year month day
              (my/align-str dayname 4)
              (my/align-str cn-horoscope 6)
              (my/align-str cn-solar-term 6)
              cn-date-str
              ))))

(defun my/org-to-taskwarrior-script ()
  "将 Org-agenda 中【既是 TODO 又有 SCHEDULED】的任务导出为 Taskwarrior 脚本。"
  (interactive)
  (let* ((output-file (expand-file-name "~/Downloads/sync_tasks.sh"))
         (tasks '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-complex-heading-regexp nil t)
             (let* ((element (org-element-at-point))
                    (todo-state (org-element-property :todo-keyword element))
                    (heading (org-element-property :title element))
                    (priority (org-element-property :priority element))
                    (tags (org-element-property :tags element))
                    (sched-prop (org-element-property :scheduled element)))
               
               ;; 核心筛选逻辑：必须是 TODO 状态 且 必须有计划时间
               (when (and (string-equal todo-state "TODO") 
                          sched-prop)
                 (let* ((task-cmd (format "task add %s" (shell-quote-argument heading)))
                        ;; 安全转换时间格式
                        (time-obj (org-timestamp-to-time sched-prop))
                        (date-str (format-time-string "%Y-%m-%d" time-obj)))
                   
                   ;; 1. 添加等待日期 (Taskwarrior 的 wait 属性)
                   (setq task-cmd (concat task-cmd " wait:" date-str))
                   
                   ;; 2. 处理优先级 (Org 65=A, 66=B, 67=C)
                   (when priority
                     (setq task-cmd (concat task-cmd 
                                            (cond ((eq priority 65) " priority:H")
                                                  ((eq priority 66) " priority:M")
                                                  ((eq priority 67) " priority:L")
                                                  (t "")))))
                   
                   ;; 3. 处理标签
                   (when tags
                     (dolist (tag tags)
                       (setq task-cmd (concat task-cmd " +" (shell-quote-argument tag)))))
                   
                   (push task-cmd tasks)))))))))
    
    ;; 写入文件
    (if tasks
        (with-temp-file output-file
          (insert "#!/bin/bash\n")
          (insert "# 自动生成的 Taskwarrior 同步脚本 (仅限有计划的任务)\n\n")
          (insert (mapconcat #'identity (reverse tasks) "\n"))
          (insert "\n\necho '同步完成！'\n"))
      (message "未发现同时符合 TODO 且有 SCHEDULED 的任务。"))
    
    (when tasks
      (chmod output-file #o755)
      (message "已导出 %d 个任务至: %s" (length tasks) output-file))))

(map! :leader
      (:prefix ("n" . "notes")
       :desc "agenda to Taskwarrior" "t" #'my/org-to-taskwarrior-script))

;;---------------------------------------------------------------------------
;;--------------------------- Clock -----------------------------------------
;;---------------------------------------------------------------------------


;;---------------------------------------------------------------------------
;;-------------------------Calculator----------------------------------------
;;---------------------------------------------------------------------------
(after! calc
  ;; 1. 定义一个开关变量，防止你不需要的时候看着眼花
  (defvar my/calc-show-multi-radix nil
    "If non-nil, show Hex and Bin alongside Decimal in Calc.")

  ;; 2. 核心逻辑：拦截 Calc 的显示函数
  (defun my/calc-add-radix-annotation (orig-fun entry)
    "Advice to append Hex/Bin annotations to Calc stack entries."
    ;; 先获取原始的显示结果 (通常是十进制)
    (let ((result (funcall orig-fun entry))
          (val (car entry))) ;; 获取内部数值对象
      ;; 只有当: 开关打开 且 数值是整数 时才处理
      (if (and my/calc-show-multi-radix
               (Math-integerp val))
          (let* (;; 临时切换到 16 进制计算 Hex 字符串
                 (hex (let ((calc-number-radix 16))
                        (math-format-value val)))
                 ;; 临时切换到 2 进制计算 Bin 字符串
                 (bin (let ((calc-number-radix 2))
                        (math-format-value val)))
                 ;; 简单的正则处理，把 "16#" 和 "2#" 前缀去掉，更美观
                 (hex-clean (replace-regexp-in-string "^16#" "" hex))
                 (bin-clean (replace-regexp-in-string "^2#" "" bin)))
            ;; 格式化输出：%-12s 表示左对齐占12格，后面跟注释
            (format "%-12s ;; Hex: %s | Bin: %s" result hex-clean bin-clean))
        ;; 如果不满足条件，就原样返回
        result)))

  ;; 3. 把上面的逻辑“挂”到 Calc 的渲染环节上
  (advice-add 'math-format-stack-value :around #'my/calc-add-radix-annotation)

  ;; 4. 定义一个命令来快速开关这个功能
  (defun toggle-calc-radix-display ()
    "Toggle multi-radix display in Calc."
    (interactive)
    (setq my/calc-show-multi-radix (not my/calc-show-multi-radix))
    (calc-refresh) ;; 强制刷新界面
    (message "Multi-radix display: %s" (if my/calc-show-multi-radix "ON" "OFF")))

  ;; 5. 绑定快捷键 (可选，比如绑定到 'gm')
  (map! :map calc-mode-map
        "g m" #'toggle-calc-radix-display))

;;---------------------------------------------------------------------------
;;-------------------------Dictionary----------------------------------------
;;---------------------------------------------------------------------------
(use-package! wordnut
  :config
  (setq +lookup-dictionary-prefer-offline nil)
  )


;;---------------------------------------------------------------------------
;;-------------------------Beancount-----------------------------------------
;;---------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.bean\\'" . beancount-mode))


;;--------------------------------------------------------------------------
;;-------------------------Org Mode-----------------------------------------
;;--------------------------------------------------------------------------
;; (add-to-list 'load-path )
(setq org-log-time "time"
      org-log-done-with-time 't)



;;---------------------------------------------------------------------------
;;-------------------------Rime Input Method---------------------------------
;;---------------------------------------------------------------------------
;;(use-package! rime
;;  :custom
;;  (default-input-method "rime")
;;  (if (featurep :system 'macos)
;;      (rime-librime-root (expand-file-name "librime/dist" doom-emacs-dir))
;;    )
;;  )


;;---------------------------------------------------------------------------
;;------------------------- PlantUML ----------------------------------------
;;---------------------------------------------------------------------------
;; 将 plantuml.jar 包放置到 doom-data-dir 下面
;; 系统中安装命令，如 sudo apt-get install graphviz
(setq org-babel-default-header-args:plantuml '((:results . "file") (:exports . "code")))

;;---------------------------------------------------------------------------
;;-------------------------Exec Path-----------------------------------------
;;---------------------------------------------------------------------------
(use-package! exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  )


;;---------------------------------------------------------------------------
;;------------------------- lsp mode ----------------------------------------
;;---------------------------------------------------------------------------
(use-package! lsp-mode
  :custom
  (lsp-modeline-code-action-fallback-icon
   (if doom--system-linux-p "󰌵" lsp-modeline-code-action-fallback-icon)
   )
  )


;;---------------------------------------------------------------------------
;;------------------------- ligatures ---------------------------------------
;;---------------------------------------------------------------------------
;; (set-ligatures! 'MAJOR-MODE :true "true" :false "false" )
(plist-put! +ligatures-extra-symbols
            :true "T"
            :false "F"
            :str "str")


;;---------------------------------------------------------------------------
;;------------------------- AI-LLM ------------------------------------------
;;---------------------------------------------------------------------------
(use-package! gptel
  :config
  ;; --- 配置 1：Gemini (使用环境变量中的 Key) ---
  (setq gptel-gemini-backend
        (gptel-make-gemini "Gemini"
          :key (getenv "GEMINI_API_KEY")
          :stream t))

  ;; --- 配置 2：ChatGPT (自定义 Server/中转) ---
  (setq gptel-openai-custom-backend
        (gptel-make-openai "Custom-ChatGPT"
          :host "api.chatanywhere.org"      ; 这里填写你的自定义服务器域名 (不带 http://)
          :endpoint "/v1/chat/completions"  ; 标准 OpenAI 路径
          :stream t
          :key (getenv "CUSTOM_CHAT_GPT_API_KEY")
          :models '(gpt-4o gpt-4-turbo gpt-3.5-turbo))) ; 定义可选模型

  ;; --- 设置默认模型 ---
  (setq-default gptel-backend gptel-gemini-backend
                gptel-model 'gemini-2.5-flash-lite))

;; DeepSeek offers an OpenAI compatible API
;; (gptel-make-openai "DeepSeek"       ;Any name you want
;;   :host "api.deepseek.com"
;;   :endpoint "/chat/completions"
;;   :stream t
;;   :key "sk-0f839c4be8d4448eb5efd08a815684ba"               ;can be a function that returns the key
;;   :models '(deepseek-chat deepseek-coder))

;; OPTIONAL configuration: set as the default gptel backend
;; (setq gptel-model   'deepseek-chat
;;       gptel-backend
;;       (gptel-make-openai "DeepSeek"     ;Any name you want
;;         :host "api.deepseek.com"
;;         :endpoint "/chat/completions"
;;         :stream t
;;         :key "sk-0f839c4be8d4448eb5efd08a815684ba"             ;can be a function that returns the key
;;         :models '(deepseek-chat deepseek-coder)))

;; 绑定一个超级快捷键 SPC n g
(map! :leader
      :prefix ("n" . "notes")
      :desc "AI chat window" "g" #'gptel
      :desc "AI msg send" "h" #'gptel-send
      )

;; Ollama
;; (gptel-make-ollama "Ollama"             ;Any name of your choosing
;;   :host "localhost:11434"               ;Where it's running
;;   :stream t                             ;Stream responses
;;   :models '(mistral:latest))          ;List of models
;; ;; OPTIONAL configuration
;; (setq
;;  gptel-model 'mistral:latest
;;  gptel-backend (gptel-make-ollama "Ollama"
;;                  :host "localhost:11434"
;;                  :stream t
;;                  :models '(mistral:latest)))


;;---------------------------------------------------------------------------
;;------------------------- org-roam-ui -------------------------------------
;;---------------------------------------------------------------------------
(use-package! websocket
  :after org-roam)
(use-package! org-roam-ui
  :after org-roam
  :config
  (setq
   org-roam-ui-sync-theme t
   org-roam-ui-follow nil
   org-roam-ui-update-on-save t
   org-roam-ui-open-on-start t
   )
  )


;;---------------------------------------------------------------------------
;;------------------------- org-align ---------------------------------------
;;---------------------------------------------------------------------------
(use-package! valign
  :hook (org-mode . valign-mode) ; 在 org-mode 中自动开启
  :config
  (setq valign-format-separator-row t) ; 可选，让分隔线看起来更平滑
  )


;;---------------------------------------------------------------------------
;;------------------------- url-proxy ---------------------------------------
;;---------------------------------------------------------------------------
;; 1. 定义代理变量 (提取 IP 与 端口)
(defvar my-proxy-host "192.168.3.2" "代理服务器地址")
(defvar my-proxy-port "22307"      "代理服务器端口")

;; 2. 定义切换函数
(defun my/toggle-proxy ()
  "一键开关 Emacs 代理，使用 my-proxy-host 和 my-proxy-port。"
  (interactive)
  (let ((proxy-addr (format "%s:%s" my-proxy-host my-proxy-port)))
    (if (bound-and-true-p url-proxy-services)
        ;; 关闭代理
        (progn
          (setq url-proxy-services nil
                gptel-backend-proxy nil)
          ;; 同时清理环境变量，确保外部 curl 也能感知
          (setenv "http_proxy"  nil)
          (setenv "https_proxy" nil)
          (message "Proxy is OFF"))
      ;; 开启代理
      (progn
        (setq url-proxy-services `(("http"  . ,proxy-addr)
                                   ("https" . ,proxy-addr)))
        (setq gptel-backend-proxy proxy-addr)
        ;; 设置环境变量，帮助一些外部进程（如 gptel 的 curl 后端）
        (setenv "http_proxy"  (format "http://%s" proxy-addr))
        (setenv "https_proxy" (format "http://%s" proxy-addr))
        (message "Proxy is ON (%s)" proxy-addr)))))

;; 3. 绑定快捷键
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Proxy" "p" #'my/toggle-proxy))
