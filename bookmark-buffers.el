;; -*-mode: Emacs-Lisp; tab-width: 4;-*- .

;; bookmark-buffers.el 
;; bookmark buffers list and open buffers list
;;
;; Last Modified: <2015/02/24 22:28:47>
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
;; (global-set-key [(control f3)] 'bookmark-buffers-save)
;; (global-set-key [(control f4)] 'bookmark-buffers-call)
;;

;; Usage
;;
;; Ctrl + F3 to save buffers which are now open
;;
;; Ctrl + F4 to open bookmark list
;; type ENTER or double left click on a bookmark name in the list, and files and directories which are listed in the bookmark will be open. 
;;
;;

;; TODO
;; apropos to save
;; auto save with default blist-key
;; auto call with default blist-key
;; edit bookmark list
;; edit file list in a bookmark

;(setq debug-on-error t)
; M-x edebug-defun

(defvar blist-file "~/.blist")

(defun bookmark-buffers-save ()
  "「現在バッファに開いているファイルとディレクトリのパス」をblist-keyというブックマークで保存する"
  (interactive)
  (let (blist-key
        all-blists-alist
        this-blist-alist
        completion-alist
        (files (buffer-list-real)))
    (set-buffer (find-file-noselect blist-file))
    (widen)
    (goto-char (point-min))
    (condition-case err
        (and (setq all-blists-alist (read (current-buffer))) ;; .blistからブックマークのリストを読み込む
             (setq completion-alist (mapcar (lambda (x) (car x)) all-blists-alist)))
      (error (message "init .blist")))
    (setq blist-key (completing-read  "BListKey: " completion-alist))
    (if (setq this-blist-alist (assoc blist-key all-blists-alist)) ;; ブックマークのリストから連想配列のキーがblist-keyの要素を取り出す
        (progn
          (setcdr (cadr this-blist-alist)
                  (cons (caadr this-blist-alist) files)) ;; 追加登録 TODO 上書きモードをオプションで選べるようにする
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
    (define-key map [double-mouse-1] 'blist-open)
    (define-key map [return] 'blist-open)
    (define-key map "q" 'blist-quit)
    (use-local-map map)))


(defun blist-open ()
  "open listed files"
  (interactive)
  (let ((blist-key (buffer-substring (progn (beginning-of-line) (point)) (progn (end-of-line) (point))))
        buffer-blist-file)
    (kill-all-buffers)
    (setq buffer-blist-file (set-buffer (find-file-noselect blist-file)))
    (widen)
    (goto-char (point-min))
    (mapcar '(lambda (file) (find-file file))
            (cadr (assoc blist-key (read (current-buffer)))))
    (kill-buffer buffer-blist-file)))


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
