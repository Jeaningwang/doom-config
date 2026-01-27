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
      calendar-holidays cal-china-x-chinese-holidays ;; 使用中国的节日来代替原来的默认节日
      )

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
           ;; 2. 获取农历月日字符串 (例如 "腊月初九")
           ;; substring 3 是为了去掉干支年(如 "乙巳年")
           (cn-date-str (substring (cal-china-x-chinese-date-string date) 0))
           ;; 3. 获取该日期的节日列表
           (holidays (cal-china-x-get-holiday date))
           ;; 将节日列表合并为字符串，如果没有节日则为空
           (holiday-str (if holidays 
                            (concat " [" (mapconcat #'identity holidays " ") "]")
                          "")))
      
      ;; 格式化输出
      (format "%04d-%02d-%02d %s %s%s"
              year month day dayname cn-date-str holiday-str))))


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
;; (use-package! gptel
;;   :config
;;   (setq! gptel-api-key "sk-0f839c4be8d4448eb5efd08a815684ba")
;;   )

;; DeepSeek offers an OpenAI compatible API
;; (gptel-make-openai "DeepSeek"       ;Any name you want
;;   :host "api.deepseek.com"
;;   :endpoint "/chat/completions"
;;   :stream t
;;   :key "sk-0f839c4be8d4448eb5efd08a815684ba"               ;can be a function that returns the key
;;   :models '(deepseek-chat deepseek-coder))

;; OPTIONAL configuration: set as the default gptel backend
(setq gptel-model   'deepseek-chat
      gptel-backend
      (gptel-make-openai "DeepSeek"     ;Any name you want
        :host "api.deepseek.com"
        :endpoint "/chat/completions"
        :stream t
        :key "sk-0f839c4be8d4448eb5efd08a815684ba"             ;can be a function that returns the key
        :models '(deepseek-chat deepseek-coder)))

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
