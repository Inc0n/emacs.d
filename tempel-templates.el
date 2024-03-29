fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(note comment-start "NOTE ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

latex-mode

(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(frac (tempel-parse-template "\\frac{$p}{$q}"))
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")

lisp-mode emacs-lisp-mode ;; Specify multiple modes

(lambda (tempel-parse-template "(lambda ($p)\n$>$r>)"))

lisp-mode

(defpackage "(defpackage "
  (p
   (concat
	(file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))
	"."
	(file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
   pkg)
  n> "(:use #:cl" p "))"
  n "(in-package :" (s pkg) ")"
  n)

(defsystem
  "(asdf:defsystem "
  (p (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
  n> ":version \"0.0.1\""
  n> ":license \"nil\""
  n> ":author \"" (p "name" name) " <" (p "email" email) ">\""
  n> ":maintainer \"" (s name) " <" (s email) ">\""
  n> ":description \"" p "\""
  n> ";; :homepage \"https://github.com/\""
  n> ";; :depends-on (#:" p ")"
  n> ":components ((:file \"package\")"
  n> "(:file \"" p "\")"
  n> "(:module src"
  n> ":components"
  n> "((:file \"vec3\" :depends-on (\"\")))"
  n> ":serial t)))")

emacs-lisp-mode

(autoload ";;;###autoload")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(iflet* "(if-let* (" p ")" n> r> ")")
(whenlet* "(when-let* (" p ")" n> r> ")")
(andlet* "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(let* "(let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(defun "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)

eshell-mode

(for "for " (p "i") " in " p " { " q " }")
(while "while { " p " } { " q " }")
(until "until { " p " } { " q " }")
(if "if { " p " } { " q " }")
(ife "if { " p " } { " p " } { " q " }")
(unl "unless { " p " } { " q " }")
(unle "unless { " p " } { " p " } { " q " }")

text-mode

(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(asciibox "+-" (make-string (length str) ?-) "-+" n
          "| " (s str)                       " |" n
          "+-" (make-string (length str) ?-) "-+" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))

rst-mode

(title (make-string (length title) ?=) n (p "Title: " title) n (make-string (length title) ?=) n)

java-mode

(class "public class " (p (file-name-base (or (buffer-file-name) (buffer-name))))
       " {" n> r> n "}")

c-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

(i "#include <"
   (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h"))
   ">")
(inc "#include \""
     (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h"))
     "\"")

org-mode

(notebook ":PROPERTIES:
:header-args: :session " (p "work") " :eval never-export :exports results
:END:")

(title "#+title: " p n "#+author: Daniel Mendler" n "#+language: en" n n)
(src "#+begin_src " p n> r> n> "#+end_src")
;; (src "#+begin_src " p n> r> n> "#+end_src" :post (org-edit-src-code))

verilog-mode

(be "begin\n" > p n> "end")
(module "module " p "()" n> "endmodule")
(for "for (; ; ;) begin" n> p n> "end")

nim-mode

(proc "proc " (p "name") "(" (p "args") "): " (p "return-type") "=" n>)
;; (template "template " (p "name"))

zig-mode

(struct "struct {" n>
		(p "field") ": " (p "type") "," n
		"}")

vhdl-mode

(if > "if " p " then" n> p n> "end if;")
(begin > "begin\n" p > "end")
(process "process(" p ")\n" > "begin\n" > "end process;")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
