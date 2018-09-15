;; -*-mode: Emacs-Lisp; tab-width: 4;-*- .

;; Information: <bookmark-buffers.el>
;;
;; bookmark buffer-list
;;
;; Last Modified: <2018/09/15 23:56:26>
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
;; C-c b s to name a buffers list and save in the buffers' bookmarks
;;
;; C-c b c to call the buffers' bookmarks
;; 1. Type ENTER, or Double Click, on a bookmark name to open it.
;; 2. Type `d' on a bookmark name to delete the bookmark.
;; 3. Type `q' to cancel
;; 4. Type `e' on a bookmark name to edit the bookmark.(goto Editing mode)
;;    4.1. Type `d' on a file to delete from the bookmark.
;;    4.2. Type `q' to cancel Editing mode.



;;;;;; custom variables

(defcustom bookmark-buffers-save-append nil
"custom variable used in bookmark-buffers.el
t : save bookmark-buffers appending currently opened buffers
nil : overwite bookmark-buffers with only current buffers")

;;;;;; private variables

(defvar bookmark-buffers-list-file "~/.emacs.d/.bblist")

;;;;;; interactive functions

;;;###autoload
(defun bookmark-buffers-save ()
  "「現在バッファに開いているファイルとディレクトリのパス」のリストをブックマークする"
  (interactive)
  (let* ((bookmark-list (bb:load-bookmark-list))
         (bookmark-key (bb:read-something-with bookmark-list))
         (this-bookmark (assoc bookmark-key bookmark-list))
         (completion-ignore-case t))
    (if this-bookmark
        (progn
          (setf (cadr this-bookmark)
                (if bookmark-buffers-save-append
                    (delete-dups (append (bb:buffer-list-real) (cadr this-bookmark))) ;; 追加
                  (bb:buffer-list-real)))                                          ;; 上書き
          (setq bookmark-list (bb:sort-bookmark-list this-bookmark bookmark-list)))
      (setq bookmark-list (cons (list bookmark-key (bb:buffer-list-real)) bookmark-list))) ;; 新規
    (bb:save-bookmark-list bookmark-list)))

;;;###autoload
(defun bookmark-buffers-call ()
  "ブックマーク一覧モード
 一覧の中のブックマークをひとつポイントし、
 [enter]: 現在開いているファイルを全て閉じて、選択したブックマークに登録してあったファイルをすべて開く。
 [d]: ブックマークを削除。y or n。
 [q]: ブックマーク一覧モード終了
 [e]: ブックマーク編集。ブックマークの中に登録してあるファイルを [d] で削除。 y or n。 [q] でブックマーク一覧に戻る。"
  (interactive)
  (let ((bookmark-buffer "*bookmark-buffers*")
        (bookmark-list (bb:load-bookmark-list))
        (i -1)
        bookmark-key
        this-bookmark
        (map (make-sparse-keymap)))
     (switch-to-buffer bookmark-buffer)
     (setq mode-name "bookmark-buffers-mode")
     (setq buffer-read-only nil) ; unlock
     (erase-buffer)
     (insert "Type ENTER, or Double Click, on a bookmark name to open it.\n")
     (insert "Type `d' to delete a bookmark on a bookmark name.\n")
     (insert "Type `e' to edit a bookmark on a bookmark name.\n")
     (insert "Type `q' to cancel.\n\n")
     (bb:put-bookmark-list bookmark-list)
     (save-excursion
       (mapcar (lambda (this-bookmark)
                 (when (< (set 'i (1+ i)) 10)
                   (insert "[" (number-to-string i) "] ")
                   (define-key map
                     (number-to-string i)
                     `(lambda ()
                        (interactive)
                        (goto-char ,(point))
                        (bookmark-buffers-open))))
                 (insert (car this-bookmark))
                 (bb:put-this-bookmark this-bookmark)
                 (insert "\n"))
               bookmark-list))
     (setq buffer-read-only t) ; lock
     (hl-line-mode 1)
     (define-key map [double-mouse-1] 'bookmark-buffers-open)
     (define-key map [return] 'bookmark-buffers-open)
     (define-key map "d" 'bookmark-buffers-delete)
     (define-key map "e" 'bookmark-buffers-edit)
     (define-key map "q" 'bookmark-buffers-quit)
     (use-local-map map)
     ;(isearch-forward)
     ))

(defun bookmark-buffers-edit ()
  "edit a bookmark"
  (interactive)
  (let ((this-bookmark (bb:get-this-bookmark))
        (bookmark-list (bb:get-bookmark-list))
        (map (make-sparse-keymap)))
     (setq buffer-read-only nil) ; unlock
     (erase-buffer)
     (insert (concat "Editing a bookmark [" (car this-bookmark) "]\n"))
     (insert "Type `d' on a file to delete from the bookmark.\n")
     (insert "Type `q' to cancel Editing mode.\n\n")
     (bb:put-bookmark-list bookmark-list)
     (save-excursion
       (mapcar (lambda (file)
                 (insert file)
                 (bb:put-this-bookmark this-bookmark)
                 (insert "\n"))
               (cadr this-bookmark)))
     (setq buffer-read-only t)   ; lock
     (define-key map "d" 'bookmark-buffers-edit-save)
     (define-key map "q" 'bookmark-buffers-call)
     (use-local-map map)))

(defun bookmark-buffers-edit-save ()
  "save edited bookmark"
  (interactive)
  (let ((file (bb:get-one-file))
        (this-bookmark (bb:get-this-bookmark)))
    (when (y-or-n-p (concat "delete " file " ? "))
      (setf (cadr this-bookmark) (delete file (cadr this-bookmark)))
      (bb:save-bookmark-list
       (bb:sort-bookmark-list this-bookmark (bb:get-bookmark-list)))
      (bookmark-buffers-edit))))

(defun bookmark-buffers-open ()
  "open files and directories in a bookmark"
  (interactive)
  (let ((this-bookmark (bb:get-this-bookmark)))
    ;; カレントなバッファリストを先頭に並べ替え
    (bb:save-bookmark-list
     (bb:sort-bookmark-list this-bookmark (bb:get-bookmark-list)))
    (bb:kill-all-buffers)
    (mapcar (lambda (file) (find-file file))
            (reverse (cadr this-bookmark)))))

(defun bookmark-buffers-delete ()
  "delete a bookmark on the point"
  (interactive)
  (when (y-or-n-p (concat "delete " (bb:get-bookmark-key) " ? "))
    (bb:save-bookmark-list (delq (bb:get-this-bookmark) (bb:get-bookmark-list)))
    (bookmark-buffers-call)))

(defun bookmark-buffers-quit ()
  "kill bookmark buffer"
  (interactive)
  (kill-buffer (current-buffer)))



;;;;;; private functions

(defun bb:load-bookmark-list ()
  ".bblistからバッファリストのリストを読み込む"
  (let (res)
    (with-temp-buffer
      (set-buffer (find-file-noselect bookmark-buffers-list-file))
      (widen)
      (goto-char 1)
      (when (< 0 (buffer-size))
        (setq res (read (current-buffer))))
      (kill-buffer))
    res))

(defun bb:save-bookmark-list (bookmark-list)
  ".bblistにバッファリストのリストを保存する"
  (with-temp-file bookmark-buffers-list-file
    (let ((standard-output (current-buffer)))
      (prin1 bookmark-list)))
  (message "bookmark-buffers: done !"))

(defun bb:get-bookmark-key ()
  "*bookmark-buffers*で現在ポイントされている行を読み込む"
  (bb:with-start-end-of-line
   (buffer-substring start end)))

(defun bb:get-bookmark-list ()
  "*bookmark-buffers*の先頭行のプロパティリストからバッファリストのリストを読み込む"
  (get-text-property 1 'bookmark-list))

(defun bb:put-bookmark-list (bookmark-list)
  "*bookmark-buffers*の先頭行のプロパティリストにバッファリストのリストを書き込む"
  (put-text-property 1 2 'bookmark-list bookmark-list))

(defun bb:get-this-bookmark ()
  "*bookmark-buffers*で現在ポイントされている行のプロパティリストからバッファリストを読み込む"
  (get-text-property (progn (beginning-of-line) (point)) 'this-bookmark))

(defun bb:put-this-bookmark (this-bookmark)
  "*bookmark-buffers*で現在ポイントされている行のプロパティリストにバッファリストを書き込む"
  (bb:with-start-end-of-line
   (put-text-property start end 'this-bookmark this-bookmark)
   (put-text-property start end 'mouse-face 'highlight)))

(defalias 'bb:get-one-file 'bb:get-bookmark-key
  "*bookmark-buffers*で現在ポイントされている行を読み込む")

(defmacro bb:with-start-end-of-line (&rest body)
  "with start and end of line"
  `(let ((start (progn (beginning-of-line) (point)))
         (end (progn (end-of-line) (point))))
     ,@body))

(defun bb:read-something-with (alist)
  "dont save with 0byte bookmark name"
  (let* ((default (caar alist))
         (res (completing-read
               (concat
                "[save] bookmark-buffers with name (default " default " ): ")
               (mapcar (lambda (slot) (car slot)) alist))))
    (or (if (string< "" res) res)
        default)))

(defun bb:sort-bookmark-list (this src)
  "this を先頭に"
  (cons this (delq this src)))

(defun bb:buffer-list-real ()
  "list up files and directories with `full path` from buffer list
カレントなバッファをリストの先頭に"
  (delq nil (mapcar
             (lambda (x)
               (set-buffer x)
               (unless (string= (file-name-nondirectory bookmark-buffers-list-file) (buffer-name)) ;exclude .bblist
                 (or (buffer-file-name) list-buffers-directory)))
             (buffer-list))))

(defun bb:kill-all-buffers ()
  "kill all buffers"
  (let ((exclude '("*scratch*" "*Messages*")))
    (mapcar (lambda (b)
              (let ((buf (buffer-name b)))
                (unless (member buf exclude)
                  (kill-buffer buf))))
            (buffer-list))))

(provide 'bookmark-buffers)
