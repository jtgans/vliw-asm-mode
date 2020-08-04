;;; vliw-asm-mode.el --- mode for editing VLIW assembler files

;; Copyright (C) 2020 June Tate-Gans

;; Author: June Tate-Gans <june@theonelab.com>
;; Keywords: tools, languages

;;; Commentary:

;; This mode was written by June Tate-Gans in a fit of rage while attempting to
;; write VLIW assembler, and dealing with brain damaged functionality in the
;; original asm-mode's main function.

;; This works for me but don't expect it to work for your use cases.

;; TODO(jtg): Rewrite this to be more respectful of modern emacs settings such
;; as tab-modes, etc.

;; This minor mode is based on text mode.  It defines a private abbrev table
;; that can be used to save abbrevs for assembler mnemonics.

;; This mode runs one hook:
;;   1) a vliw-asm-mode-hook at the end of initialization.

;;; Code:

(require 'subr-x)

(eval-when-compile
  (require 'cl))

(defgroup vliw-asm nil
  "Mode for editing assembler code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defvar vliw-asm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\; "> b" st)
    (modify-syntax-entry ?\# "< b" st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    st)
  "Syntax table used while in Vliw-Asm mode.")

(defvar vliw-asm-mode-abbrev-table nil
  "Abbrev table used while in Vliw-Asm mode.")
(define-abbrev-table 'vliw-asm-mode-abbrev-table ())

(defvar vliw-asm-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Vliw-Asm mode.")

(defun vliw-asm-generate-font-lock-register-string (register-list)
  "Converts the shorthand representation of registers into a list of strings.

Elements of REGISTER-LIST are either strings or a three-element
list of (stem min max). The format of the list is the arglist expected in
`vliw-asm-generate-register-set'."
  (sort
   (loop for ele in register-list append
         (cond ((stringp ele) (list ele))
               ((listp ele) (apply 'vliw-asm-generate-register-set ele))
               (t "")))
   (lambda (a b) (> (length a) (length b)))))

(defun vliw-asm-generate-register-set (stem min max)
  "Generates a list of register name strings of the form STEMn, where n is
between MIN and MAX, inclusive.

For example, with a stem of \"a\" and a min of 0 and a max of 3, this will
generate the list (\"a0\" \"a1\" \"a2\" \"a3\")."
  (mapcar (lambda (n) (concatenate 'string stem n))
            (loop for n from min to max collect (number-to-string n))))

(defvar vliw-asm-register-list
  '(("x" 0 31)
    "zero" "ra" "sp" "gp" "tp"
    ("t" 0 6)
    ("s" 0 11)
    ("a" 0 7)
    "fp")
  "This is a list of lists or strings that define the registers in the ISA. Each
element can be a string of the actual register name, or a list of the form
(stem min max), where stem is a string of the base part of the register name,
and min and max are the minimum number and maximum number. This allows for
compact definitions of arrays of registers.

Ie: (\"x 0 3\") will be transformed into x0 x1 x2 and x3.")

(defvar vliw-asm-opcode-list
  (append '(lb lh lw lbu lhu)
          '(sb sh sw)
          '(sll slli srl srli sra srai)
          '(add addi sub lui auipc)
          '(xor xori or ori and andi)
          '(slt slti sltu sltiu)
          '(beq bne blt bge bltu bgeu)
          '(jal jalr)
          '(fence fence.i)
          '(scall sbreak)
          '(rdcycle rdcycleh rdtime rdtimeh rdinstret rdinstreth)
          '(csrrw csrrs csrrc csrrwi csrrsi csrrci)
          '(ecall ebreak eret)
          '(mrts mrth hrts)
          '(wfi sfence.vm))
  "A list of strings of opcode names in the ISA.")

(defun vliw-asm-generate-opcode-list (opcode-list)
  "Generates a sorted list of strings from the symbols defined in OPCODE-LIST."
  (sort (mapcar 'symbol-name opcode-list)
        (lambda (a b) (> (length a) (length b)))))

(defun vliw-asm-generate-font-lock-keywords ()
  "Generates the font-lock-keywords needed for syntax highlighting. Called by `vliw-asm-mode'."
  (list (append
   `(;; registers
     (,(concatenate 'string "\\<\\("
                    (string-join (vliw-asm-generate-font-lock-register-string vliw-asm-register-list) "\\|")
                    "\\)\\>")
      (1 font-lock-constant-face))
     ;; beginning_of_line_labels:
     ("^\\(\\.?\\(\\sw\\|\\s_\\)+\\):"
      (1 font-lock-function-name-face))
     ;; .pseudo ops
     ("^\\s *\\(\\.\\(\\sw\\|\\s_\\)+\\)"
      (1 font-lock-constant-face))
     ;; opcode
     (,(concatenate 'string "\\<\\(" (string-join (vliw-asm-generate-opcode-list vliw-asm-opcode-list) "\\|") "\\)\\>")
      (1 font-lock-keyword-face))
   cpp-font-lock-keywords))))

;;;###autoload
(define-derived-mode vliw-asm-mode prog-mode "VLIW"
  "Major mode for editing typical VLIW assembler code.
Features a private abbrev table and special handling for grouping operators like
open-close curly braces. 

Turning on Vliw-Asm mode runs the hook `vliw-asm-mode-hook' at the end of initialization.

Special commands:
\\{vliw-asm-mode-map}"
  (setq local-abbrev-table vliw-asm-mode-abbrev-table)
  (set (make-local-variable 'font-lock-defaults) (vliw-asm-generate-font-lock-keywords))
  (set (make-local-variable 'tab-always-indent) nil)
  (set-syntax-table (make-syntax-table vliw-asm-mode-syntax-table)))

(provide 'vliw-asm-mode)

;;; vliw-asm-mode.el ends here
