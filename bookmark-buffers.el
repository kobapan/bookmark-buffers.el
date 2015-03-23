;; -*-mode: Emacs-Lisp; tab-width: 4;-*- .

;; Information: <bookmark-buffers.el>
;;
;; bookmark buffer-list
;;
;; Last Modified: <2015/03/23 19:11:47>
;; Auther: <kobapan>
;;

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;; Installation
;;
;; Add bookmark-buffers.el to your load path
;; add your .emacs
;;
;; (autoload 'bookmark-buffers-save "bookmark-buffers" nil t)
;; (autoload 'bookmark-buffers-call "bookmark-buffers" nil t)
;; (global-set-key (kbd "C-c b s") 'bookmark-buffers-save)
;; (global-set-key (kbd "C-c b c") 'bookmark-buffers-call)
;;

;; Usage
;;
;; Ctrl+c b s to save buffers list with a key name
;;
;; Ctrl+c b c to call bookmark list
;; type ENTER or double left click on a bookmark name in the list, and files and directories which are listed in the bookmark will be open.
;;
;;

; (setq debug-on-error t)
; M-x edebug-defun


;;;;;; custom variables

(defcustom blist-save-append nil
"custom variable used in bookmark-buffers.el
t : save buffers list appending current buffers
nil : overwite buffers list with current buffers")

;;;;;; private variables

(defvar blist-file "~/.emacs.d/.blist")

;;;;;; interactive functions

(defun bookmark-buffers-save ()
  "「現在バッファに開いているファイルとディレクトリのパス」のリストをブックマークする"
  (interactive)
  (let ((bookmark-list (load-bookmark-list))
        blist-key
        this-blist
        (completion-ignore-case t))
    (setq blist-key (read-something-with bookmark-list))
    (if (setq this-blist (assoc blist-key bookmark-list))
        (progn
          (setf (cadr this-blist)
                (if blist-save-append
                    (delete-dups (append (buffer-list-real) (cadr this-blist))) ;; 追加
                  (buffer-list-real)))                                          ;; 上書き
          (setq bookmark-list (sort-bookmark-list this-blist bookmark-list)))
      (setq this-blist (list blist-key (buffer-list-real)))
      (setq bookmark-list (cons this-blist bookmark-list)))
    (save-bookmark-list bookmark-list)))

(defun bookmark-buffers-call ()
  "ブックマーク一覧モード
 一覧の中のブックマークをひとつポイントし、
 [enter]: 現在開いているファイルを全て閉じて、選択したブックマークに登録してあったファイルをすべて開く。
 [d]: ブックマークを削除。y or n。
 [q]: ブックマーク一覧モード終了
 [e]: ブックマーク編集。ブックマークの中に登録してあるファイルを [d] で削除。 y or n。 [q] でブックマーク一覧に戻る。"
  (interactive)
  (let ((blist-buffer "*blist*")
        (bookmark-list (load-bookmark-list))
        blist-key
        this-blist
        (map (make-sparse-keymap)))
     (switch-to-buffer blist-buffer)
     (setq buffer-read-only nil) ; unlock
     (erase-buffer)
     (insert "Type ENTER, or Double Click, on a bookmark name to open it.\n")
     (insert "Type `d' to delete a bookmark on a bookmark name.\n")
     (insert "Type `e' to edit a bookmark on a bookmark name.\n")
     (insert "Type `q' to cancel.\n\n")
     (put-bookmark-list bookmark-list)
     (save-excursion
       (mapcar (lambda (this-blist)
                 (insert (car this-blist))
                 (put-this-blist this-blist)
                 (insert "\n"))
               bookmark-list))
     (setq buffer-read-only t) ; lock
     (hl-line-mode 1)
     (setq mode-name "blist-mode")
     (define-key map [double-mouse-1] 'bookmark-buffers-open)
     (define-key map [return] 'bookmark-buffers-open)
     (define-key map "d" 'bookmark-buffers-delete)
     (define-key map "e" 'bookmark-buffers-edit)
     (define-key map "q" 'bookmark-buffers-quit)
     (use-local-map map)))

(defun bookmark-buffers-edit ()
  "edit a bookmark"
  (interactive)
  (let ((this-blist (get-this-blist))
        (bookmark-list (get-bookmark-list))
        (map (make-sparse-keymap)))
     (setq buffer-read-only nil) ; unlock
     (erase-buffer)
     (insert (concat "Editing a bookmark [" (car this-blist) "]\n"))
     (insert "Type `d' on a file to delete from the bookmark.\n")
     (insert "Type `q' to cancel.\n\n")
     (put-bookmark-list bookmark-list)
     (save-excursion
       (mapcar (lambda (file)
                 (insert file)
                 (put-this-blist this-blist)
                 (insert "\n"))
               (cadr this-blist)))
     (setq buffer-read-only t)   ; lock
     (define-key map "d" 'bookmark-buffers-save-edit)
     (define-key map "q" 'bookmark-buffers-call)
     (use-local-map map)))

(defun bookmark-buffers-save-edit ()
  "save edited bookmark"
  (interactive)
  (let ((file (get-one-file))
        (this-blist (get-this-blist)))
    (when (y-or-n-p (concat "delete " file " ? "))
      (setf (cadr this-blist) (delete file (cadr this-blist)))
      (save-bookmark-list
       (sort-bookmark-list this-blist (get-bookmark-list)))
      (bookmark-buffers-edit))))

(defun bookmark-buffers-open ()
  "open files and directories in a bookmark"
  (interactive)
  (let ((this-blist (get-this-blist)))
    ;; カレントなバッファリストを先頭に並べ替え
    (save-bookmark-list
     (sort-bookmark-list this-blist (get-bookmark-list)))
    (kill-all-buffers)
    (mapcar (lambda (file) (find-file file))
            (reverse (cadr this-blist)))))

(defun bookmark-buffers-delete ()
  "delete a bookmark on the point"
  (interactive)
  (when (y-or-n-p (concat "delete " (get-blist-key) " ? "))
    (save-bookmark-list (delq (get-this-blist) (get-bookmark-list)))
    (bookmark-buffers-call)))

(defun bookmark-buffers-quit ()
  "kill blist buffer"
  (interactive)
  (kill-buffer (current-buffer)))



;;;;;; private functions

(defun load-bookmark-list ()
  ".blistからバッファリストのリストを読み込む"
  (let (res)
    (with-temp-buffer
      (set-buffer (find-file-noselect blist-file))
      (widen)
      (goto-char 1)
      (when (buffer-size)
        (setq res (read (current-buffer))))
      (kill-buffer))
    res))

(defun save-bookmark-list (bookmark-list)
  ".blistにバッファリストのリストを保存する"
  (with-temp-file blist-file
    (let ((standard-output (current-buffer)))
      (prin1 bookmark-list))))

(defun get-blist-key ()
  "*blist*で現在ポイントされている行を読み込む"
  (with-start-end-of-line
   (buffer-substring start end)))

(defun get-bookmark-list ()
  "*blist*の先頭行のプロパティリストからバッファリストのリストを読み込む"
  (get-text-property 1 'bookmark-list))

(defun put-bookmark-list (bookmark-list)
  "*blist*の先頭行のプロパティリストにバッファリストのリストを書き込む"
  (put-text-property 1 2 'bookmark-list bookmark-list))

(defun get-this-blist ()
  "*blist*で現在ポイントされている行のプロパティリストからバッファリストを読み込む"
  (get-text-property (progn (beginning-of-line) (point)) 'this-blist))

(defun put-this-blist (this-blist)
  "*blist*で現在ポイントされている行のプロパティリストにバッファリストを書き込む"
  (with-start-end-of-line
   (put-text-property start end 'this-blist this-blist)
   (put-text-property start end 'mouse-face 'highlight)))

(defalias 'get-one-file 'get-blist-key
  "*blist*で現在ポイントされている行を読み込む")

(defmacro with-start-end-of-line (&rest body)
  "with start and end of line"
  `(let ((start (progn (beginning-of-line) (point)))
         (end (progn (end-of-line) (point))))
     ,@body))

(defun read-something-with (alist)
  "dont save with 0byte key name"
  (let ((res (completing-read
              "bookmark buffers list with Key Name: "
              (mapcar (lambda (slot) (car slot)) alist))))
    (or (if (string< "" res) res)
        (read-something-with alist))))

(defun sort-bookmark-list (this src)
  "this を先頭に"
  (cons this (delq this src)))

(defun buffer-list-real ()
  "list up files and directories with `full path` from buffer list
カレントなバッファをリストの先頭に"
  (delq nil (mapcar
             (lambda (x)
               (set-buffer x)
               (unless (string= (file-name-nondirectory blist-file) (buffer-name)) ;exclude .blist
                 (or (buffer-file-name) list-buffers-directory)))
             (buffer-list))))

(defun kill-all-buffers ()
  "kill all buffers"
  (let ((exclude '("*scratch*" "*Messages*")))
    (mapcar '(lambda (b)
               (let ((buf (buffer-name b)))
                 (unless (member buf exclude)
                   (kill-buffer buf))))
            (buffer-list))))

(provide 'bookmark-buffers)
