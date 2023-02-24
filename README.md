# bookmark-buffers.el

プロジェクト管理用 emacs lisp

「複数開いているファイル、ディレクトリ、ウィンドウの状態」にプロジェクト名をつけて保存しておいて、いつでも開き直せる。ウィンドウの状態も復元する。

## インストール

Add bookmark-buffers.el to your load path

Add your .emacs
```cl
(autoload 'bookmark-buffers-save "bookmark-buffers" nil t)
(autoload 'bookmark-buffers-call "bookmark-buffers" nil t)
(global-set-key (kbd "C-c b s") 'bookmark-buffers-save)
(global-set-key (kbd "C-c b c") 'bookmark-buffers-call)
```

## 使い方

### C-c b s
「現在開いているファイルとディレクトリ」にプロジェクト名をつけて保存する。

### C-c b c
プロジェクト一覧モードへ

### 一覧の中のプロジェクトをひとつポイントし、_enter_ / _double left click_
現在開いているファイルを全て閉じて、選択したプロジェクトに登録されているファイル・ディレクトリをすべて開く。
一覧の表示順は、最近見た順になっている。直近の0～9番目までは数字を入力するだけで飛べる。

### _d_
プロジェクトを削除。y or n。

### _q_
プロジェクト一覧モード終了

### _e_
プロジェクト編集モードへ。プロジェクトの中に登録してあるファイルを [d] で削除。 y or n。 [q] でプロジェクト一覧に戻る。


## custom variables

bookmark-buffers-save-append

default nil

t : プロジェクトに現在開いているバッファを追加

nil : プロジェクトの中身を現在開いているバッファと入れ替える
