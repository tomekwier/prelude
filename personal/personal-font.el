;;; Handling font ligatures using an open source package

(use-package ligature
  :load-path "~/code/ligature.el/"
  :config
  ;; Enable the "www" ligature in every possible major mode
  ;; (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  ;; (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 't '(
                               "[ERROR]"
                               "[DEBUG]"
                               "[INFO]"
                               "[WARN]"
                               "[WARNING]"
                               "[ERR]"
                               "[FATAL]"
                               "[TRACE]"
                               "[FIXME]"
                               "[TODO]"
                               "[BUG]"
                               "[NOTE]"
                               "[HACK]"
                               "[MARK]"
                               "# ERROR"
                               "# DEBUG"
                               "# INFO"
                               "# WARN"
                               "# WARNING"
                               "# ERR"
                               "# FATAL"
                               "# TRACE"
                               "# FIXME"
                               "# TODO"
                               "# BUG"
                               "# NOTE"
                               "# HACK"
                               "# MARK"
                               "// ERROR"
                               "// DEBUG"
                               "// INFO"
                               "// WARN"
                               "// WARNING"
                               "// ERR"
                               "// FATAL"
                               "// TRACE"
                               "// FIXME"
                               "// TODO"
                               "// BUG"
                               "// NOTE"
                               "// HACK"
                               "// MARK"
                               "!!"
                               "!="
                               "!=="
                               "!!!"
                               "!≡"
                               "!≡≡"
                               "!>"
                               "!=<"
                               "#("
                               "#_"
                               "#{"
                               "#?"
                               "#>"
                               "##"
                               "#_("
                               "%="
                               "%>"
                               "%>%"
                               "%<%"
                               "&%"
                               "&&"
                               "&*"
                               "&+"
                               "&-"
                               "&/"
                               "&="
                               "&&&"
                               "&>"
                               "$>"
                               "***"
                               "*="
                               "*/"
                               "*>"
                               "++"
                               "+++"
                               "+="
                               "+>"
                               "++="
                               "--"
                               "-<"
                               "-<<"
                               "-="
                               "->"
                               "->>"
                               "---"
                               "-->"
                               "-+-"
                               "-\\/"
                               "-|>"
                               "-<|"
                               ".."
                               "..."
                               "..<"
                               ".>"
                               ".~"
                               ".="
                               "/*"
                               "//"
                               "/>"
                               "/="
                               "/=="
                               "///"
                               "/**"
                               "::"
                               ":="
                               ":≡"
                               ":>"
                               ":=>"
                               ":("
                               ":-("
                               ":)"
                               ":-)"
                               ":/"
                               ":\\"
                               ":3"
                               ":D"
                               ":P"
                               ":>:"
                               ":<:"
                               "<$>"
                               "<*"
                               "<*>"
                               "<+>"
                               "<-"
                               "<<"
                               "<<<"
                               "<<="
                               "<="
                               "<=>"
                               "<>"
                               "<|>"
                               "<<-"
                               "<|"
                               "<=<"
                               "<~"
                               "<~~"
                               "<<~"
                               "<$"
                               "<+"
                               "<!>"
                               "<@>"
                               "<#>"
                               "<%>"
                               "<^>"
                               "<&>"
                               "<?>"
                               "<.>"
                               "</>"
                               "<\\>"
                               "<\">"
                               "<:>"
                               "<~>"
                               "<**>"
                               "<<^"
                               "<!"
                               "<@"
                               "<#"
                               "<%"
                               "<^"
                               "<&"
                               "<?"
                               "<."
                               "</"
                               "<\\"
                               "<\""
                               "<:"
                               "<->"
                               "<!--"
                               "<--"
                               "<~<"
                               "<==>"
                               "<|-"
                               "<<|"
                               "<-<"
                               "<-->"
                               "<<=="
                               "<=="
                               "=<<"
                               "=="
                               "==="
                               "==>"
                               "=>"
                               "=~"
                               "=>>"
                               "=/="
                               "=~="
                               "==>>"
                               "≡≡"
                               "≡≡≡"
                               "≡:≡"
                               ">-"
                               ">="
                               ">>"
                               ">>-"
                               ">>="
                               ">>>"
                               ">=>"
                               ">>^"
                               ">>|"
                               ">!="
                               ">->"
                               "??"
                               "?~"
                               "?="
                               "?>"
                               "???"
                               "?."
                               "^="
                               "^."
                               "^?"
                               "^.."
                               "^<<"
                               "^>>"
                               "^>"
                               "\\\\"
                               "\\>"
                               "\\/-"
                               "@>"
                               "|="
                               "||"
                               "|>"
                               "|||"
                               "|+|"
                               "|->"
                               "|-->"
                               "|=>"
                               "|==>"
                               "|>-"
                               "|<<"
                               "||>"
                               "|>>"
                               "|-"
                               "||-"
                               "~="
                               "~>"
                               "~~>"
                               "~>>"
                               "[["
                               "]]"
                               "\">"
                               "_|_"))

;; Enables ligature checks globally in all buffers. You can also do it
;; per mode with `ligature-mode'.
(global-ligature-mode t))
