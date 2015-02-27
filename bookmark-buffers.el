;; -*-mode: Emacs-Lisp; tab-width: 4;-*- .

;; Information: <bookmark-buffers.el>
;;
;; bookmark buffers list and open buffers list
;;
;; Last Modified: <2015/02/28 04:29:30>
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

;; TODO
;; - save with default , last visited blist-key
;; - call with default , last visited blist-key
;; - sort bookmark list as now open/save blist-key first when visit it
;; - edit bookmark list
;; - edit file list in a bookmark

; (setq debug-on-error t)
; M-x edebug-defun


;;;;;; private variables

(defvar blist-file "~/.blist")


;;;;;; interactive functions

(defun bookmark-buffers-save ()
  "「現在バッファに開いているファイルとディレクトリのパス」をblist-keyというブックマークで保存する"
  (interactive)
  (let (blist-key
        all-blists-alist
        this-blist-alist
        copy
        (files (buffer-list-real)))
    (set-buffer (find-file-noselect blist-file))
    (widen)
    (goto-char (point-min))
    (condition-case err
        (setq all-blists-alist (read (current-buffer))) ;; .blistからブックマークのリストを読み込む
      (error (message "init .blist")))
    (setq blist-key (completing-read  "bookmark buffers list with Key Name: "
                                      (mapcar (lambda (x) (car x)) all-blists-alist)))
    (if (setq this-blist-alist (assoc blist-key all-blists-alist)) ;; ブックマークのリストから連想配列のキーがblist-keyの要素を取り出す
        (progn
          (setq copy (copy-alist (cadr this-blist-alist)))
          (setcdr (cadr this-blist-alist) (append files copy))
          (delete-dups (cadr this-blist-alist)))
      (setq this-blist-alist (list blist-key files))
      (setq all-blists-alist (cons this-blist-alist all-blists-alist)))
    (erase-buffer)
    (prin1 all-blists-alist (current-buffer))
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun bookmark-buffers-call ()
  "ブックマーク一覧モード
 一覧の中のブックマークをひとつポイントし、
 [enter]: 現在開いているファイルを全て閉じて、選択したブックマークに登録してあったファイルをすべて開く。
 [d]: ブックマークを削除。y or n。
 [q]: ブックマーク一覧モード終了
 [e]: ブックマーク編集。ブックマークの中に登録してあるファイルを [d] で削除。 y or n。 [q] でブックマーク一覧に戻る。"
  (interactive)
  (let ((blist-buffer "*blist*") (map (make-sparse-keymap)))
    (set-buffer (find-file-noselect blist-file))
    (widen)
    (goto-char (point-min))
    (setq all-blists-alist (read (current-buffer)))
    (switch-to-buffer blist-buffer)
    (setq buffer-read-only nil) ; unlock
    (erase-buffer)
    (insert (mapconcat 'identity
                       (mapcar 
                        (lambda (x)
                          (car x))
                        all-blists-alist)
                       "\n"))
    (setq buffer-read-only t)   ; lock
    (setq mode-name "blist-mode")
    (define-key map [double-mouse-1] 'bookmark-buffers-open)
    (define-key map [return] 'bookmark-buffers-open)
    (define-key map "q" 'bookmark-buffers-quit)
    (use-local-map map)))

(defun bookmark-buffers-open ()
  "open files and directories in a bookmark"
  (interactive)
  (let ((blist-key (buffer-substring
                    (progn (beginning-of-line) (point))
                    (progn (end-of-line) (point))))
        (buffer-blist-file (find-file blist-file))
        (bookmark-list (progn (widen)
                              (goto-char (point-min))
                              (read (current-buffer)))))
    (kill-all-buffers)
    (mapcar '(lambda (file) (find-file file))
            (reverse (cadr (assoc blist-key bookmark-list))))
    (kill-buffer buffer-blist-file)))

(defun bookmark-buffers-quit ()
  "kill blist buffer"
  (interactive)
  (kill-buffer (current-buffer)))


;;;;;; private functions

;(defun sort


(defun buffer-list-real ()
  "list up files and directories `full path` from buffer list"
  (delq nil (mapcar
   (lambda (x)
     (set-buffer x)
     (or (buffer-file-name) list-buffers-directory))
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
