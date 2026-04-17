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
;; (add-to-list 'org-src-lang-modes '("javascript" . js))
(defalias 'org-babel-execute:javascript 'org-babel-execute:js)
(setq org-babel-default-header-args:javascript '((:results . "output")))


;;--------------------------------------------------------------------
;;------------------------- C ----------------------------------------
;;--------------------------------------------------------------------
(setq org-babel-C-compiler "gcc")
(setq org-babel-default-header-args:C '((:results . "output")))

;;--------------------------------------------------------------------
;;------------------------- cpp --------------------------------------
;;--------------------------------------------------------------------
(setq org-babel-C++-compiler "g++")
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
(setq org-babel-default-header-args:emacs-lisp '((:results . "output")))


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
 #'paw-eudic-search-details
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

(defun my/get-taskwarrior-descriptions ()
  "获取当前 Taskwarrior 中所有待办任务的标题列表。"
  (if (executable-find "task")
      (let ((output (shell-command-to-string "task status:pending or status:waiting export")))
        ;; 解析 JSON 以获取 description 字段
        ;; 这里使用简单的正则匹配来提取，避免引入额外的 json 解析库依赖
        (let ((titles '())
              (start 0))
          (while (string-match "\"description\":\"\\([^\"]+\\)\"" output start)
            (push (match-string 1 output) titles)
            (setq start (match-end 0)))
          titles))
    '()))

(defun my/org-to-taskwarrior-script ()
  "将 Org 任务导出为 Taskwarrior 脚本，自动排除已存在的任务和“循环”标签。"
  (interactive)
  (let* ((output-file (expand-file-name "~/Downloads/sync_tasks.sh"))
         (tasks '())
         (exclude-tag "循环")
         ;; 1. 预先获取 Taskwarrior 中已存在的任务描述
         (existing-tasks (my/get-taskwarrior-descriptions)))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-complex-heading-regexp nil t)
             (let* ((element (org-element-at-point))
                    (todo-state (org-element-property :todo-keyword element))
                    (heading (substring-no-properties (or (org-element-property :title element) "")))
                    (priority (org-element-property :priority element))
                    (tags (org-element-property :tags element))
                    (sched-prop (org-element-property :scheduled element)))
               
               ;; 核心筛选逻辑：
               ;; 1. 状态为 TODO 且有计划时间
               ;; 2. 不含“循环”标签
               ;; 3. 任务标题不在 Taskwarrior 的已有列表中
               (when (and (string-equal todo-state "TODO") 
                          sched-prop
                          (not (member exclude-tag tags))
                          (not (member heading existing-tasks))) ; 查重逻辑
                 
                 (let* ((task-cmd (format "task add %s" (shell-quote-argument heading)))
                        (time-obj (org-timestamp-to-time sched-prop))
                        (date-str (format-time-string "%Y-%m-%d" time-obj)))
                   
                   (setq task-cmd (concat task-cmd " wait:" date-str " scheduled:" date-str))
                   
                   ;; 优先级映射
                   (when priority
                     (setq task-cmd (concat task-cmd 
                                            (cond ((eq priority 65) " priority:H")
                                                  ((eq priority 66) " priority:M")
                                                  ((eq priority 67) " priority:L")
                                                  (t "")))))
                   
                   ;; 标签处理
                   (when tags
                     (dolist (tag tags)
                       (setq task-cmd (concat task-cmd " +" (shell-quote-argument tag)))))
                   
                   (push task-cmd tasks)))))))))
    
    ;; 写入脚本文件
    (if tasks
        (progn
          (with-temp-file output-file
            (insert "#!/bin/bash\n")
            (insert "# 自动生成的增量同步脚本\n\n")
            (insert (mapconcat #'identity (reverse tasks) "\n"))
            (insert "\n\necho '增量同步完成！'\n"))
          (chmod output-file #o755)
          (message "已导出 %d 个新任务至: %s" (length tasks) output-file))
      (message "所有任务均已在 Taskwarrior 中或符合排除条件，无需同步。"))))

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
(setq lsp-beancount-journal-file (concat (getenv "BEANCOUNT_HOME") "main.bean"))


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
  (lsp-modeline-code-action-fallback-icon "󰌵")
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
  ;; Broken Customize can save `gptel-directives' as a string; gptel then errors
  ;; with "Wrong type argument: listp" on any (alist-get _ gptel-directives).
  (when (and (boundp 'gptel-directives) (not (listp gptel-directives)))
    (setq gptel-directives
          '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
            (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
            (writing . "You are a large language model and a writing assistant. Respond concisely.")
            (chat . "You are a large language model and a conversation partner. Respond concisely."))))
  ;; --- 配置 1：Gemini (使用环境变量中的 Key) ---
  (setq gptel-gemini-backend
        (gptel-make-gemini "Gemini"
          :key (getenv "GEMINI_API_KEY")
          :stream t))

  ;; --- 配置 2：ChatGPT (自定义 Server/中转) ---
  (setq gptel-openai-custom-backend
        (gptel-make-openai "Custom-ChatGPT"
          :host "api.chatanywhere.tech"      ; 这里填写你的自定义服务器域名 (不带 http://)
          :endpoint "/v1/chat/completions"  ; 标准 OpenAI 路径
          :stream t
          :key (getenv "CUSTOM_CHAT_GPT_API_KEY")
          ;; :key (getenv "CUSTOM_CHATANYWHERE_PAID_API_KEY")
          :models '(gpt-4o gpt-4-turbo gpt-3.5-turbo))) ; 定义可选模型

  ;; --- 配置3 :Claude ---
  (setq claude-custom-backend
        (gptel-make-anthropic "Custom-Claude"
          :host "api.chatanywhere.tech"
          :endpoint "/v1/chat/completions"
          :stream t
          :key (getenv "CUSTOM_CHAT_GPT_API_KEY")
          ;; :key (getenv "CUSTOM_CHATANYWHERE_PAID_API_KEY")
          ))

  ;; system prompt
  (add-to-list 'gptel-directives '(Wd-Personal . "
你是一名生活在Emacs中的大语言模型，任何回答都要简明扼要。

约束：
1. 自检：提交前核对功能与逻辑正确性。
"))

  (add-to-list 'gptel-directives '(编码助手 . "
职责
你的职责是帮助我完成编写代码、修复代码和理解代码等任务。我会与你分享我的目标和项目，你将协助我编写所需的代码以取得成功。

目标
* 代码创建：尽可能编写能够实现我目标的完整代码。
* 技能培训：教我代码开发的步骤。
* 清晰指导：以易于理解的方式，解释如何实现或编写代码。
* 详尽文档：为每个步骤或代码的每个部分提供清晰的文档说明。

整体方向
* 记住全程保持积极、耐心、支持的语气。 
* 使用清晰、简单的语言，假设我具备基本的代码理解能力。
* 切勿讨论任何与编程无关的话题！如果我提到与编程无关的事物，请道歉并将话题转回编程。
* 在整个对话中记住上下文，确保你的想法和回应与之前的所有对话相关。
* 如果我问候你或问你可以做什么，请简要说明你的职责。保持简洁明了，并给出一些简短的例子。

分步指引
* 了解我的要求：收集编写代码所需的信息。询问有关目的、用途及其他相关细节的澄清问题，以确保你理解我的要求。
* 概略介绍解决方案：清晰概述代码的功能和工作原理。解释开发步骤、假设条件和限制。 
* 展示代码和实现说明：以便于复制粘贴的方式呈现代码，解释你的设计思路以及任何可以调整的变量或参数。提供清晰的代码实现步骤说明。
"))


  ;; --- 设置默认模型 ---
  ;; `gptel-directives' must be an alist; if Customize saved a string (e.g. "en"),
  ;; (alist-get ... gptel-directives) signals wrong-type-argument listp.
  (setq-default gptel-backend gptel-openai-custom-backend
                gptel-model 'gpt-3.5-turbo
                gptel--system-message
                (when (listp gptel-directives)
                  (alist-get 'Wd-Personal gptel-directives)))
  )

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


;;---------------------------------------------------------------------------
;;------------------------- cursor-agent ---------------------------------------
;;---------------------------------------------------------------------------
(after! cursor-agent
  (setq cursor-agent-default-model "Auto")
  ;; Keybindings
  (map! :leader
        (:prefix "c"
                 (:prefix ("A" . "cursor-agent")
                          "a" #'cursor-agent-prompt
                          "i" #'cursor-agent-interactive
                          "r" #'cursor-agent-region
                          "I" #'cursor-agent-install
                          "L" #'cursor-agent-login
                          "s" #'cursor-agent-status))))


;;---------------------------------------------------------------------------
;;------------------------- paw ---------------------------------------------
;;---------------------------------------------------------------------------
(use-package! paw
  :config
  (setq paw-online-word-servers '(eudic))
  (setq paw-dictionary-function 'paw-eudic-search-details)
  (setq paw-authorization-keys `(("eudic" . ,(getenv "EUDIC_API_KEY"))))
  )


;;---------------------------------------------------------------------------
;;------------------------- Eudic daily review ------------------------------
;;---------------------------------------------------------------------------
(after! (request gptel paw)
  (require 'cl-lib)
  (require 'json)
  (require 'subr-x)
  (require 'url)

  (defgroup my/eudic-review nil
    "Daily Eudic review workflow powered by gptel."
    :group 'applications)

  (defcustom my/eudic-review-language "en"
    "Language parameter used for Eudic studylist APIs."
    :type 'string
    :group 'my/eudic-review)

  (defcustom my/eudic-review-studylist-id 0
    "Wordbook id: query `category_id' for GET .../studylist/words, JSON `id' for DELETE."
    :type 'integer
    :group 'my/eudic-review)

  (defcustom my/eudic-review-page-size 200
    "Page size for GET .../studylist/words. Larger = fewer HTTP round-trips."
    :type 'integer
    :group 'my/eudic-review)

  (defcustom my/eudic-review-strict-today-only t
    "If non-nil, only keep words added today.
When the API payload has no time fields, raise an error instead of using all words."
    :type 'boolean
    :group 'my/eudic-review)

  (defcustom my/eudic-review-buffer "*Eudic Daily Review*"
    "Buffer name used to display review progress."
    :type 'string
    :group 'my/eudic-review)

  (defcustom my/eudic-review-interactive-fallback t
    "If non-nil, fallback to interactive word selection when today filtering is ambiguous."
    :type 'boolean
    :group 'my/eudic-review)

  (defcustom my/eudic-review-enable-note-due t
    "If non-nil, include words whose note contains REVIEW_AGAIN:today.
This scans entries not already matched by add-time; each check may call the
note API (see `my/eudic-review-note-due-max-lookups')."
    :type 'boolean
    :group 'my/eudic-review)

  (defcustom my/eudic-review-note-due-max-lookups nil
    "Cap note API calls when building the due-by-note set; nil = unlimited.
Use a number (e.g. 200) if the studylist is huge and review feels too slow."
    :type '(choice (const :tag "Unlimited" nil) (natnum :tag "Max calls"))
    :group 'my/eudic-review)

  (defun my/eudic--auth-token ()
    "Resolve Eudic API token from `paw-authorization-keys'."
    (let ((raw
           (cond ((stringp paw-authorization-keys) paw-authorization-keys)
                 ((and (listp paw-authorization-keys)
                       (or (assoc "eudic" paw-authorization-keys)
                           (assoc 'eudic paw-authorization-keys)))
                  (cdr (or (assoc "eudic" paw-authorization-keys)
                           (assoc 'eudic paw-authorization-keys))))
                 (t ""))))
      (cond ((stringp raw) raw)
            ((and (consp raw) (functionp (car raw))) (eval raw t))
            (t (format "%s" raw)))))

  (defun my/eudic--authorization-value ()
    "Return Authorization header value for Frdic Open API (scheme `NIS')."
    (let* ((raw (my/eudic--auth-token))
           (tok (string-trim (if (stringp raw) raw (format "%s" raw)))))
      ;; Header line must not contain CR/LF/TAB; those confuse proxies and IIS.
      (setq tok (replace-regexp-in-string "[\r\n\t]" "" tok))
      (cond
       ((string-empty-p tok) "")
       ((string-match-p "\\`[Nn][Ii][Ss][[:space:]]+" tok) tok)
       (t (concat "NIS " tok)))))

  (defun my/eudic--headers (method data)
    "Return headers for Eudic OpenAPI.
Do not set User-Agent here: `url-http-create-request' already emits one;
a second User-Agent line makes IIS/HTTP.sys return \"invalid header name\".
Do not send Content-Type on GET; only with body (POST/DELETE/PUT)."
    (let ((method (upcase (or method "GET")))
          (auth (my/eudic--authorization-value)))
      (append
       (unless (string-empty-p auth)
         (list (cons "Authorization" auth)))
       (when (and data (member method '("POST" "DELETE" "PUT")))
         '(("Content-Type" . "application/json"))))))

  (defun my/eudic--build-query-string (params)
    "Build URL query string from PARAMS alist (symbol or string keys).
Each pair is turned into a two-element list (KEY VAL); `url-build-query-string'
requires that shape — dotted pairs (KEY . VAL) break its internal `mapcar'."
    (when params
      (unless (listp params)
        (error "Eudic: query params must be a list, got %S" params))
      (url-build-query-string
       (mapcar (lambda (p)
                 (unless (consp p)
                   (error "Eudic: bad query pair %S" p))
                 (list (format "%s" (car p))
                       (format "%s" (cdr p))))
               params))))

  (defun my/eudic--request-json-sync (url &rest args)
    "HTTP JSON via `url-retrieve-synchronously' (no external curl).
ARGS plist: :type (GET/POST/DELETE), :params query alist for GET, :data JSON string for body."
    (let* ((plist args)
           (method (upcase (or (plist-get plist :type) "GET")))
           (params (plist-get plist :params))
           (data (plist-get plist :data))
           (full-url (if (and params (string= method "GET"))
                         (progn
                           (unless (listp params)
                             (error "Eudic: :params must be an alist for GET, got %S" params))
                           (concat url "?" (my/eudic--build-query-string params)))
                       url))
           (url-request-method method)
           (url-request-extra-headers (my/eudic--headers method data))
           (url-request-data (when (and data (member method '("POST" "DELETE" "PUT")))
                               (if (stringp data)
                                   data
                                 (error "Eudic: :data must be a JSON string")))))
      ;; 4th arg is TIMEOUT in seconds (Emacs 29+); must be a number or nil — `t' is invalid and
      ;; triggers "Invalid time specification" inside `time-less-p'.
      ;; Inhibit cookies: a malformed stored Cookie header also triggers IIS 400.
      (let ((buf (url-retrieve-synchronously full-url t t nil)))
        (unless buf
          (error "Eudic: no response (check network/DNS/HTTPS)"))
        (unwind-protect
            (with-current-buffer buf
              (set-buffer-multibyte t)
              (goto-char (point-min))
              (unless (looking-at "^HTTP/[^ ]+ \\([0-9][0-9][0-9]\\)")
                (error "Eudic: invalid HTTP response"))
              (let ((code (string-to-number (match-string 1))))
                (unless (<= 200 code 299)
                  (goto-char (point-min))
                  (let ((body ""))
                    (when (re-search-forward "\r?\n\r?\n" nil t)
                      (setq body (buffer-substring-no-properties (point) (point-max))))
                    (error "Eudic HTTP %d — %s" code body))))
              (goto-char (point-min))
              (unless (re-search-forward "\r?\n\r?\n" nil t)
                (error "Eudic: empty body"))
              (let ((json-object-type 'alist)
                    (json-array-type 'list)
                    (json-key-type 'symbol)
                    (parsed (condition-case nil
                                (json-read)
                              (error nil))))
                ;; `json-read' can return a string/number; `alist-get' needs an alist.
                (if (and parsed (listp parsed))
                    parsed
                  '((data . ())))))
          (kill-buffer buf)))))

  (defun my/eudic--entry-word (entry)
    "Extract word string from ENTRY."
    (cond ((stringp entry) entry)
          ((listp entry) (format "%s" (or (my/eudic--alist-get 'word entry) "")))
          (t "")))

  (defun my/eudic--same-local-day-p (time-str)
    "Return non-nil when TIME-STR falls on local today."
    (when (and time-str (not (string-empty-p time-str)))
      (let* ((ts (format "%s" time-str))
             ;; Eudic uses ISO8601 like 2026-02-02T03:07:00Z.
             (tm (or (ignore-errors (date-to-time ts))
                     (ignore-errors (parse-time-string ts)))))
        (when tm
          (string=
           (format-time-string "%Y-%m-%d" (current-time))
           (format-time-string "%Y-%m-%d" tm))))))

  (defun my/eudic--extract-date-from-time (time-str)
    "Extract local YYYY-MM-DD date from TIME-STR."
    (when (and time-str (not (string-empty-p (format "%s" time-str))))
      (let ((tm (or (ignore-errors (date-to-time (format "%s" time-str)))
                    (ignore-errors (parse-time-string (format "%s" time-str))))))
        (when tm
          (format-time-string "%Y-%m-%d" tm)))))

  (defun my/eudic--alist-get (key alist)
    "Like `alist-get' but never passes a non-list as ALIST."
    (when (and key (listp alist))
      (alist-get key alist)))

  (defun my/eudic--studylist-entry-p (x)
    "Non-nil if X is a JSON object (alist or plist), not a bare string in `data'."
    (and x (listp x)
         (let ((c (car-safe x)))
           (or (consp c) (keywordp c)))))

  (defun my/eudic--get-word-note (word &optional memo)
    "Fetch note text for WORD from Eudic.
MEMO, if non-nil, is an `eq' hash-table: word string -> note string, to avoid
duplicate HTTP requests in one command."
    (when (and word (not (string-empty-p word)))
      (let ((cached (if memo (gethash word memo :eudic-no-note) :eudic-no-note)))
        (if (not (eq cached :eudic-no-note))
            cached
          (let* ((resp (my/eudic--request-json-sync
                        "https://api.frdic.com/api/open/v1/studylist/note"
                        :type "GET"
                        :params `(("language" . ,my/eudic-review-language)
                                  ("word" . ,word))))
                 (data (my/eudic--alist-get 'data resp))
                 (note
                  (cond ((stringp data) data)
                        ((and (listp data) (my/eudic--studylist-entry-p data))
                         (or (my/eudic--alist-get 'note data)
                             (my/eudic--alist-get 'text data)
                             (format "%s" data)))
                        ((listp data) (format "%s" data))
                        (t (format "%s" data)))))
            (when memo (puthash word note memo))
            note)))))

  (defun my/eudic--due-by-note-entries (data filtered today)
    "From DATA, entries not in FILTERED whose note contains REVIEW_AGAIN:TODAY.
Uses a hash memo and optional `my/eudic-review-note-due-max-lookups'."
    (let ((memo (make-hash-table :test 'equal))
          (api-n 0)
          (cap my/eudic-review-note-due-max-lookups))
      (cl-remove-if-not
       (lambda (item)
         (and (my/eudic--studylist-entry-p item)
              (not (memq item filtered))
              (let ((word (my/eudic--entry-word item)))
                (and (not (string-empty-p word))
                     (if (and cap (>= api-n cap))
                         nil
                       (let ((note
                              (ignore-errors
                                (let ((miss (eq (gethash word memo :eudic-no-note)
                                                :eudic-no-note)))
                                  (when miss (cl-incf api-n))
                                  (my/eudic--get-word-note word memo)))))
                         (and (stringp note)
                              (string-match-p
                               (regexp-quote (format "REVIEW_AGAIN:%s" today))
                               note))))))))
       data)))

  (defun my/eudic--studylist-words--normalize-batch (resp)
    "Return list of word entries from one GET studylist/words response."
    (unless (listp resp)
      (setq resp '((data . ()))))
    (let* ((raw (my/eudic--alist-get 'data resp))
           (batch (cond ((vectorp raw) (append raw nil))
                        ((listp raw) raw)
                        (t '()))))
      ;; API sometimes mixes strings (e.g. language codes) into `data'; skip them.
      (cl-remove-if-not #'my/eudic--studylist-entry-p batch)))

  (defun my/eudic--fetch-all-studylist-words ()
    "Fetch all words: GET .../studylist/words?category_id=&page=&page_size= (OpenAPI doc)."
    (let* ((url "https://api.frdic.com/api/open/v1/studylist/words")
           (page 1)
           (all '())
           (done nil))
      (while (not done)
        (let* ((params `(("language" . ,my/eudic-review-language)
                         ("category_id" . ,(number-to-string my/eudic-review-studylist-id))
                         ("page" . ,(number-to-string page))
                         ("page_size" . ,(number-to-string my/eudic-review-page-size))))
               (resp (my/eudic--request-json-sync url :type "GET" :params params))
               (batch (my/eudic--studylist-words--normalize-batch resp)))
          (setq all (append all batch))
          (if (or (null batch) (= (length batch) 0)
                  (< (length batch) my/eudic-review-page-size))
              (setq done t)
            (setq page (1+ page)))))
      all))

  (defun my/eudic--get-daily-words ()
    "Fetch daily words from default Eudic studylist."
    (let* ((data (my/eudic--fetch-all-studylist-words))
           (time-keys '(created_at create_time add_time added_at date datetime createdAt createTime addTime))
           (filtered
            (cl-remove-if-not
             (lambda (item)
               (and (my/eudic--studylist-entry-p item)
                    (let ((time-str nil))
                      (dolist (k time-keys)
                        (when (and (not time-str) (my/eudic--alist-get k item))
                          (setq time-str (format "%s" (my/eudic--alist-get k item)))))
                      (my/eudic--same-local-day-p time-str))))
             data))
           (today (format-time-string "%Y-%m-%d"))
           (due-by-note
            (if my/eudic-review-enable-note-due
                (my/eudic--due-by-note-entries data filtered today)
              nil))
           (combined (delete-dups (append filtered due-by-note))))
      (cond
       ((not my/eudic-review-strict-today-only) data)
       ((> (length combined) 0) combined)
       (my/eudic-review-interactive-fallback
        (let* ((date->entries (make-hash-table :test 'equal))
               (dates '()))
          (dolist (it data)
            (when (my/eudic--studylist-entry-p it)
              (let ((d (my/eudic--extract-date-from-time
                        (or (my/eudic--alist-get 'add_time it)
                            (my/eudic--alist-get 'created_at it)
                            (my/eudic--alist-get 'create_time it)
                            (my/eudic--alist-get 'added_at it)
                            (my/eudic--alist-get 'date it)
                            (my/eudic--alist-get 'datetime it)
                            (my/eudic--alist-get 'createdAt it)
                            (my/eudic--alist-get 'createTime it)
                            (my/eudic--alist-get 'addTime it)))))
                (when d
                  (unless (member d dates) (push d dates))
                  (puthash d (cons it (gethash d date->entries)) date->entries)))))
          (if (null dates)
              (user-error
               "No dated words found in payload; cannot offer date picker. Run `my/eudic-review-debug-api`")
            (let* ((sorted-dates (sort (copy-sequence dates) #'string<))
                   (selected-date
                    (completing-read
                     "No words for today. Choose a date to review: "
                     sorted-dates nil t nil nil (car (last sorted-dates)))))
              (nreverse (copy-sequence (gethash selected-date date->entries)))))))
       (t
        (let* ((dates (delq nil
                            (mapcar (lambda (it)
                                      (when (my/eudic--studylist-entry-p it)
                                        (my/eudic--extract-date-from-time
                                         (or (my/eudic--alist-get 'add_time it)
                                             (my/eudic--alist-get 'created_at it)
                                             (my/eudic--alist-get 'create_time it)))))
                                    data)))
               (sorted (sort (copy-sequence dates) #'string<))
               (date-min (car sorted))
               (date-max (car (last sorted))))
          (user-error
           "No daily words matched. Date range in API payload: %s ~ %s. Try `my/eudic-review-strict-today-only' nil, disable `my/eudic-review-enable-note-due', or `my/eudic-review-debug-api'."
           (or date-min "N/A") (or date-max "N/A")))))))

  (defun my/eudic--append-buffer (fmt &rest args)
    "Append formatted text into review buffer."
    (with-current-buffer (get-buffer-create my/eudic-review-buffer)
      (goto-char (point-max))
      (insert (apply #'format fmt args) "\n")))

  (defun my/eudic--mark-for-tomorrow (word)
    "Mark WORD in note with tomorrow reminder."
    (let* ((tomorrow (format-time-string "%Y-%m-%d"
                                         (time-add (current-time)
                                                   (days-to-time 1))))
           (note (format "[REVIEW_AGAIN:%s]" tomorrow)))
      (my/eudic--request-json-sync
       "https://api.frdic.com/api/open/v1/studylist/note"
       :type "POST"
       :data (json-encode `(("word" . ,word)
                            ("language" . ,my/eudic-review-language)
                            ("note" . ,note))))))

  (defun my/eudic--delete-word (word)
    "Delete WORD from default Eudic studylist."
    (my/eudic--request-json-sync
     "https://api.frdic.com/api/open/v1/studylist/words"
     :type "DELETE"
     :data (json-encode `(("id" . ,my/eudic-review-studylist-id)
                          ("language" . ,my/eudic-review-language)
                          ("words" . [ ,word ])))))

  (defun my/eudic--ask-gptel (prompt callback)
    "Send PROMPT by gptel and call CALLBACK with response text."
    (gptel-request
        prompt
      :system "You are an English vocabulary tutor. Return concise and structured output."
      :callback (lambda (response &rest _)
                  (funcall callback (if (stringp response)
                                        response
                                      (format "%s" response))))))

  (defun my/eudic--judge-pass-p (prompt callback)
    "Ask gptel to judge PROMPT and call CALLBACK with t/nil."
    (my/eudic--ask-gptel
     prompt
     (lambda (result)
       (funcall callback (string-match-p "PASS" (upcase (string-trim result)))))))

  (defun my/eudic--report-error (err done-callback)
    "Record ERR and continue flow via DONE-CALLBACK."
    (my/eudic--append-buffer "Result: ERROR (%s) -> continue next word." (format "%S" err))
    (funcall done-callback 'error))

  (defun my/eudic--review-one (word done-callback)
    "Review one WORD, then call DONE-CALLBACK with result symbol."
    (let ((analysis-prompt
           (format
            "Analyze this word for quick review: %s\nReturn with sections: Meaning, Spelling Focus, Common Collocations, One Example Sentence."
            word)))
      (my/eudic--ask-gptel
       analysis-prompt
       (lambda (analysis)
         (condition-case err
             (let* ((spelling-answer
                     (progn
                       (my/eudic--append-buffer "\n=== %s ===\n%s\n" word analysis)
                       (read-string
                        (format "Type the exact spelling for this word (hint: %s): "
                                (make-string (max 1 (length word)) ?*)))))
                    (meaning-answer
                     (read-string
                      (format "Explain the meaning of '%s' in your own words: " word)))
                    (sentence-answer
                     (read-string
                      (format "Write a common sentence using '%s': " word))))
               (my/eudic--judge-pass-p
                (format "Target word: %s\nUser spelling: %s\nJudge if spelling is exactly correct. Reply exactly PASS or FAIL."
                        word spelling-answer)
                (lambda (spelling-pass)
                  (my/eudic--judge-pass-p
                   (format "Target word: %s\nUser meaning explanation: %s\nJudge if this explanation is semantically correct for common usage. Reply exactly PASS or FAIL."
                           word meaning-answer)
                   (lambda (meaning-pass)
                     (my/eudic--judge-pass-p
                      (format "Target word: %s\nUser sentence: %s\nJudge if sentence is natural, grammatical, and uses the target word correctly in a common context. Reply exactly PASS or FAIL."
                              word sentence-answer)
                      (lambda (sentence-pass)
                        (condition-case err2
                            (progn
                              (my/eudic--append-buffer
                               "Check -> spelling:%s meaning:%s sentence:%s"
                               (if spelling-pass "PASS" "FAIL")
                               (if meaning-pass "PASS" "FAIL")
                               (if sentence-pass "PASS" "FAIL"))
                              (if (and spelling-pass meaning-pass sentence-pass)
                                  (progn
                                    (my/eudic--delete-word word)
                                    (my/eudic--append-buffer "Result: PASS -> deleted from studylist.")
                                    (funcall done-callback 'pass))
                                (progn
                                  (my/eudic--mark-for-tomorrow word)
                                  (my/eudic--append-buffer "Result: FAIL -> scheduled for tomorrow.")
                                  (funcall done-callback 'fail))))
                          (error
                           (my/eudic--report-error err2 done-callback))))))))))
           (error
            (my/eudic--report-error err done-callback)))))))

  (defun my/eudic-review-daily-words ()
    "Run full daily review flow for default Eudic studylist."
    (interactive)
    (unless (and (my/eudic--auth-token)
                 (not (string-empty-p (my/eudic--auth-token))))
      (user-error "Missing EUDIC API key. Please set EUDIC_API_KEY first"))
    (let* ((words-raw (my/eudic--get-daily-words))
           (words (mapcar #'my/eudic--entry-word words-raw))
           (words (cl-remove-if #'string-empty-p words))
           (total (length words))
           (passed 0)
           (failed 0)
           (errored 0))
      (with-current-buffer (get-buffer-create my/eudic-review-buffer)
        (erase-buffer)
        (insert (format "Eudic Daily Review (%s)\n\n"
                        (format-time-string "%Y-%m-%d"))))
      (pop-to-buffer my/eudic-review-buffer)
      (if (= total 0)
          (my/eudic--append-buffer "No words (category_id=%s)." my/eudic-review-studylist-id)
        (cl-labels
            ((next-word (remaining)
               (if (null remaining)
                   (progn
                     (my/eudic--append-buffer
                      "\nDone. Total: %d, PASS: %d, FAIL: %d, ERROR: %d"
                      total passed failed errored)
                     (message "Eudic review done: total=%d pass=%d fail=%d error=%d"
                              total passed failed errored))
                 (let ((word (car remaining)))
                   (my/eudic--append-buffer "Reviewing: %s" word)
                   (my/eudic--review-one
                    word
                    (lambda (result)
                      (cond ((eq result 'pass)
                             (setq passed (1+ passed)))
                            ((eq result 'fail)
                             (setq failed (1+ failed)))
                            (t
                             (setq errored (1+ errored))))
                      (next-word (cdr remaining))))))))
          (next-word words))))))

(defun my/eudic-review-debug-api ()
  "Dump raw Eudic API payloads for troubleshooting."
  (interactive)
  (let* ((url "https://api.frdic.com/api/open/v1/studylist/words")
         (base-params `(("language" . ,my/eudic-review-language)
                        ("category_id" . ,(number-to-string my/eudic-review-studylist-id))
                        ("page" . "1")
                        ("page_size" . "20")))
         (page1 (my/eudic--request-json-sync url :type "GET" :params base-params))
         (all (my/eudic--fetch-all-studylist-words))
         (buf (get-buffer-create "*Eudic API Debug*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "GET .../studylist/words?language=&category_id=&page=&page_size=\n\n")
      (insert "=== Page 1 ===\n\n")
      (insert (pp-to-string page1))
      (insert (format "\n\n=== Paginated total: %d entries ===\n" (length all))))
    (pop-to-buffer buf)))

(map! :leader
      (:prefix ("n" . "notes")
       :desc "Eudic daily review" "E" #'my/eudic-review-daily-words))

;;---------------------------------------------------------------------------
;;------------------------- Anki + paraOrg workflow -------------------------
;;---------------------------------------------------------------------------
(after! (org gptel)
  (require 'json)
  (require 'subr-x)
  (require 'url)
  (require 'cl-lib)

  (defgroup my/anki nil
    "Anki daily report and paraOrg card automation."
    :group 'applications)

  (defcustom my/anki-connect-url "http://127.0.0.1:8765"
    "Anki-Connect endpoint."
    :type 'string
    :group 'my/anki)

  (defcustom my/anki-default-deck "Default"
    "Fallback deck name for auto-created cards."
    :type 'string
    :group 'my/anki)

  (defcustom my/anki-default-model "Basic"
    "Anki note type used when creating cards."
    :type 'string
    :group 'my/anki)

  (defcustom my/paraorg-directory org-directory
    "Directory containing paraOrg files."
    :type 'directory
    :group 'my/anki)

  (defcustom my/paraorg-file-regexp "\\.org\\'"
    "Regexp used to pick paraOrg files."
    :type 'regexp
    :group 'my/anki)

  (defun my/anki--request (action &optional params)
    "Call Anki-Connect ACTION with PARAMS and return result."
    (let* ((payload (encode-coding-string
                     (json-encode
                      `(("action" . ,action)
                        ("version" . 6)
                        ("params" . ,(or params (make-hash-table)))))
                     'utf-8))
           (url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json; charset=utf-8")))
           (url-request-data payload)
           ;; Always bypass proxy for local Anki-Connect endpoint.
           (url-proxy-services
            (append '(("no_proxy" . "^\\(localhost\\|127\\.0\\.0\\.1\\)$"))
                    (cl-remove-if (lambda (kv)
                                    (equal (car-safe kv) "no_proxy"))
                                  (copy-sequence url-proxy-services))))
           (buf (url-retrieve-synchronously my/anki-connect-url t t 10)))
      (unless buf
        (error "Cannot reach Anki-Connect at %s" my/anki-connect-url))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "Invalid Anki-Connect response"))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (obj (json-read))
                   (err (alist-get 'error obj))
                   (res (alist-get 'result obj)))
              (when err
                (error "Anki-Connect error: %s" err))
              res))
        (kill-buffer buf))))

  (defun my/anki--start-of-today ()
    "Return start of local day as Emacs time object."
    (let* ((dt (decode-time (current-time))))
      (encode-time 0 0 0 (decoded-time-day dt) (decoded-time-month dt) (decoded-time-year dt))))

  (defun my/anki--time-ms (tm)
    "Convert time object TM to unix milliseconds."
    (truncate (* 1000 (float-time tm))))

  (defun my/anki--strip-html (s)
    "Remove HTML tags from S."
    (let ((txt (replace-regexp-in-string "<[^>]*>" "" (or s ""))))
      (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " txt))))

  (defun my/anki--chunk (lst n)
    "Split LST into chunks of N."
    (let ((out '()))
      (while lst
        (push (cl-subseq lst 0 (min n (length lst))) out)
        (setq lst (nthcdr n lst)))
      (nreverse out)))

  (defun my/anki--format-review-type (v)
    "Format review type code V."
    (pcase v
      (0 "learn")
      (1 "review")
      (2 "relearn")
      (3 "cram")
      (_ (format "%s" v))))

  (defun my/anki--normalize-review-row (row)
    "Normalize cardReviews ROW to an alist shape.
Supports both object form and 9-tuple array/list form:
[reviewTime cardId usn ease ivl lastIvl factor duration type]."
    (cond
     ;; Newer/object style.
     ((and (listp row) (assoc 'cardId row))
      `((reviewId . ,(or (alist-get 'reviewId row) (alist-get 'id row) 0))
        (cardId . ,(alist-get 'cardId row))
        (ease . ,(or (alist-get 'ease row) ""))
        (reviewDuration . ,(or (alist-get 'reviewDuration row) (alist-get 'time row) ""))
        (reviewType . ,(or (alist-get 'reviewType row) (alist-get 'type row) ""))))
     ;; Legacy/list style.
     ((and (listp row) (>= (length row) 9))
      `((reviewId . ,(or (nth 0 row) 0))
        (cardId . ,(nth 1 row))
        (ease . ,(or (nth 3 row) ""))
        (reviewDuration . ,(or (nth 7 row) ""))
        (reviewType . ,(or (nth 8 row) ""))))
     ;; Vector style (defensive).
     ((and (vectorp row) (>= (length row) 9))
      `((reviewId . ,(or (aref row 0) 0))
        (cardId . ,(aref row 1))
        (ease . ,(or (aref row 3) ""))
        (reviewDuration . ,(or (aref row 7) ""))
        (reviewType . ,(or (aref row 8) ""))))
     (t nil)))

  (defun my/anki--get-today-reviews ()
    "Return plist with :details and :summary for today's reviews."
    (let* ((start-ms (my/anki--time-ms (my/anki--start-of-today)))
           (end-ms (my/anki--time-ms (time-add (my/anki--start-of-today) (days-to-time 1))))
           (decks (my/anki--request "deckNames"))
           (all-reviews '()))
      (dolist (deck decks)
        (let ((rows (ignore-errors
                      (my/anki--request "cardReviews"
                                        `(("deck" . ,deck) ("startID" . ,start-ms))))))
          (when (listp rows)
            (setq all-reviews (nconc all-reviews (delq nil (mapcar #'my/anki--normalize-review-row rows)))))))
      (setq all-reviews
            (cl-remove-if-not
             (lambda (r)
               (let ((rid (or (alist-get 'reviewId r) 0)))
                 (and (>= rid start-ms) (< rid end-ms))))
             all-reviews))
      (let* ((card-ids (delete-dups
                        (delq nil (mapcar (lambda (r) (alist-get 'cardId r)) all-reviews))))
             (info-map (make-hash-table :test 'equal)))
        (dolist (ids (my/anki--chunk card-ids 300))
          (dolist (ci (my/anki--request "cardsInfo" `(("cards" . ,(vconcat ids)))))
            (puthash (alist-get 'cardId ci) ci info-map)))
        (let ((detail-rows '())
              (summary-map (make-hash-table :test 'equal)))
          (dolist (r (sort (copy-sequence all-reviews)
                           (lambda (a b) (< (alist-get 'reviewId a) (alist-get 'reviewId b)))))
            (let* ((cid (alist-get 'cardId r))
                   (info (gethash cid info-map))
                   (deck (or (alist-get 'deckName info) ""))
                   (front (my/anki--strip-html (or (alist-get 'question info) "")))
                   (ts (seconds-to-time (/ (float (alist-get 'reviewId r)) 1000.0)))
                   (time-str (format-time-string "%Y-%m-%d %H:%M:%S" ts)))
              (push (list cid deck time-str
                          (my/anki--format-review-type (alist-get 'reviewType r))
                          (or (alist-get 'ease r) "")
                          (or (alist-get 'reviewDuration r) "")
                          front)
                    detail-rows)
              (let ((cur (gethash cid summary-map)))
                (if cur
                    (progn
                      (setf (nth 2 cur) (1+ (nth 2 cur)))
                      (setf (nth 4 cur) time-str))
                  (puthash cid (list cid deck 1 time-str time-str front) summary-map)))))
          (list :details (nreverse detail-rows)
                :summary (hash-table-values summary-map))))))

  (defun my/anki--insert-org-table (headers rows)
    "Insert an Org table with HEADERS and ROWS at point."
    (insert "|" (mapconcat (lambda (h) (format " %s " h)) headers "|") "|\n")
    (insert "|" (mapconcat (lambda (_) "---") headers "|") "|\n")
    (dolist (row rows)
      (insert "|" (mapconcat (lambda (cell) (format " %s " (or cell "")))
                             (mapcar (lambda (x) (format "%s" x)) row)
                             "|")
              "|\n"))
    (when (derived-mode-p 'org-mode)
      (org-table-align)))

  (defun my/anki-insert-today-review-report ()
    "Insert today's Anki report at point (detail + unique summary)."
    (interactive)
    (let* ((report (my/anki--get-today-reviews))
           (details (plist-get report :details))
           (summary (plist-get report :summary))
           (today (format-time-string "%Y-%m-%d")))
      (insert (format "* Anki Review Report (%s)\n" today))
      (insert "** Review Detail (revlog-like)\n")
      (my/anki--insert-org-table
       '("cardId" "deck" "reviewTime" "reviewType" "ease" "durationMs" "card")
       details)
      (insert "\n** Unique Cards Summary\n")
      (my/anki--insert-org-table
       '("cardId" "deck" "reviews" "firstReview" "lastReview" "card")
       summary)
      (insert "\n")
      (message "Inserted Anki report: detail=%d unique=%d"
               (length details) (length summary))))

  (defun my/anki--collect-today-paraorg-text ()
    "Collect text from today's modified org files under `my/paraorg-directory'."
    (let* ((start (my/anki--start-of-today))
           (files (directory-files-recursively my/paraorg-directory my/paraorg-file-regexp))
           (chunks '()))
      (dolist (f files)
        (let* ((attr (file-attributes f))
               (mtime (file-attribute-modification-time attr)))
          (when (time-less-p start mtime)
            (with-temp-buffer
              (insert-file-contents f)
              (push (format "### FILE: %s\n%s\n" f (buffer-string)) chunks)))))
      (string-join (nreverse chunks) "\n")))

  (defun my/anki--extract-json-block (s)
    "Extract JSON payload from model response S."
    (let* ((txt (string-trim (or s "")))
           (fenced (when (string-match "```json\\([[:ascii:][:nonascii:]\n\r\t ]*?\\)```" txt)
                     (string-trim (match-string 1 txt)))))
      (or fenced txt)))

  (defun my/anki--build-add-note-obj (item)
    "Convert ITEM alist into Anki addNote object."
    (let* ((front (string-trim (or (alist-get 'front item) "")))
           (back (string-trim (or (alist-get 'back item) "")))
           (deck (string-trim (or (alist-get 'deck item) "")))
           (tags (alist-get 'tags item))
           (tags-list (cond ((vectorp tags) (append tags nil))
                            ((listp tags) tags)
                            (t '()))))
      (when (and (not (string-empty-p front)) (not (string-empty-p back)))
        `(("deckName" . ,(if (string-empty-p deck) my/anki-default-deck deck))
          ("modelName" . ,my/anki-default-model)
          ("fields" . (("Front" . ,front) ("Back" . ,back)))
          ("tags" . ,(vconcat (mapcar (lambda (x) (format "%s" x)) tags-list)))
          ("options" . (("allowDuplicate" . :json-false)))))))

  (defun my/anki--ai-generate-notes (raw-text done)
    "Use gptel to transform RAW-TEXT into card JSON and call DONE."
    (let ((prompt
           (format
            (concat
             "Transform the following Org content into flashcards.\n"
             "Return ONLY JSON array.\n"
             "Each item must be: {\"front\":\"...\",\"back\":\"...\",\"tags\":[...],\"deck\":\"...\"}\n"
             "Rules:\n"
             "1) Build concise, testable Front.\n"
             "2) Back should contain direct answer plus key context.\n"
             "3) Infer tags from topic, language, and source.\n"
             "4) Deck can be empty string if unknown.\n"
             "5) Ignore low-information lines.\n\n"
             "Org content:\n%s")
            raw-text)))
      (gptel-request
          prompt
        :system "You are an expert learning designer and Anki card author. Output valid JSON only."
        :callback
        (lambda (response &rest _)
          (condition-case err
              (let* ((json-str (my/anki--extract-json-block (format "%s" response)))
                     (items (json-parse-string json-str :array-type 'list :object-type 'alist)))
                (funcall done items nil))
            (error
             (funcall done nil (format "%S" err))))))))

  (defun my/anki--submit-notes (items)
    "Send ITEMS to Anki via addNotes."
    (let* ((note-objs (delq nil (mapcar #'my/anki--build-add-note-obj items)))
           (result (my/anki--request "addNotes" `(("notes" . ,(vconcat note-objs)))))
           (ok 0)
           (fail 0))
      (dolist (id result)
        (if id (setq ok (1+ ok)) (setq fail (1+ fail))))
      (list ok fail (length note-objs))))

  (defun my/anki-create-cards-from-region (beg end)
    "Generate cards from current region and submit to Anki."
    (interactive "r")
    (let ((text (string-trim (buffer-substring-no-properties beg end))))
      (if (string-empty-p text)
          (user-error "Empty region")
        (message "Generating cards from region via gptel...")
        (my/anki--ai-generate-notes
         text
         (lambda (items err)
           (if err
               (message "AI parsing failed: %s" err)
             (pcase-let ((`(,ok ,fail ,total) (my/anki--submit-notes items)))
               (message "Anki import done: generated=%d success=%d failed=%d"
                        total ok fail))))))))

  (defun my/anki-create-cards-from-paraorg-today ()
    "Auto-generate cards from today's paraOrg updates and submit to Anki."
    (interactive)
    (let ((text (my/anki--collect-today-paraorg-text)))
      (if (string-empty-p text)
          (message "No paraOrg files changed today in %s" my/paraorg-directory)
        (message "Generating cards from today's paraOrg files via gptel...")
        (my/anki--ai-generate-notes
         text
         (lambda (items err)
           (if err
               (message "AI parsing failed: %s" err)
             (pcase-let ((`(,ok ,fail ,total) (my/anki--submit-notes items)))
               (message "Anki import done: generated=%d success=%d failed=%d"
                        total ok fail))))))))

  (map! :leader
        (:prefix ("n" . "notes")
         :desc "Insert Anki daily report" "k r" #'my/anki-insert-today-review-report
         :desc "Anki cards from paraOrg today" "k p" #'my/anki-create-cards-from-paraorg-today
         :desc "Anki cards from region" "k s" #'my/anki-create-cards-from-region)))


;;---------------------------------------------------------------------------
;;------------------------- alfred workflow ---------------------------------
;;---------------------------------------------------------------------------
;; 确保 Emacs server 在运行（如果还没开的话）
(unless (server-running-p)
  (server-start))

;; Alfred 调用的入口函数
(defun my/alfred-gptel-send (prompt)
  "Send PROMPT to gptel from Alfred.
  Opens or reuses the *Alfred* gptel buffer, ensures Emacs is visible
  in the foreground, and sends the prompt."
  (interactive)
  (require 'gptel)
  (let ((buffer (gptel "Alfred")))
    ;; 1. 如果 Emacs 没有可见窗口（比如后台 daemon），先创建一个
    (unless (visible-frame-list)
      (make-frame))
    ;; 2. 让当前 frame 获取输入焦点
    (select-frame-set-input-focus (selected-frame))
    ;; 3. 显示并切换到 *Alfred* buffer
    (pop-to-buffer buffer '((display-buffer-reuse-window
                             display-buffer-same-window)
                            (inhibit-same-window . nil)))
    ;; 4. 插入 prompt 并发送
    (with-current-buffer buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert prompt)
      (gptel-send))
    ;; 5. 在 macOS 上把 Emacs 应用提到最前台
    (when (memq window-system '(ns mac))
      (cond ((fboundp 'ns-raise-emacs)
             (ns-raise-emacs))
            ((fboundp 'do-applescript)
             (do-applescript "tell application \"Emacs\" to activate"))
            (t
             (call-process "osascript" nil 0 nil
                           "-e" "tell application \"Emacs\" to activate"))))))
