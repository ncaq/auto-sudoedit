[![MELPA](https://melpa.org/packages/auto-sudoedit-badge.svg)](https://melpa.org/#/auto-sudoedit)

auto-sudoedit

# English

## Summary

A package that automatically reopens files with sudo.

When opening a file that requires root authority,
you can open the file via sudo using tramp,
but it is troublesome to input it one by one.

This is a package that will automatically reopen with sudo if it cannot write to the opened file or directory.

## How to install

Place it in melpa or directly downloaded by PATH.

If downloaded directly

~~~ el
(require 'auto-sudoedit)
~~~

auto-sudoedit works when minor mode is enabled.

~~~ el
(auto-sudoedit-mode 1)
~~~

If you enable minor mode,
When you open a file that requires root privileges,
It will be reopened automatically.

## Recommend

If you find the following message depressing

~~~~
Autosave file on local temporary directory, do you want to continue? (y or n) y
~~~~

On a non-shared PC, you can allow some security risk and use.
Set `tramp-allow-unsafe-temporary-files` to `t`.

# Japanese

## 概要

自動的にsudoでファイルを開き直すパッケージです。

root権限が必要なファイルを開く場合。
trampを使ってsudo経由でファイルを開くことが出来ますが、
一々入力するのは面倒くさいです。

これは開いたファイルかディレクトリに書き込みができない場合、
自動的にsudoで開き直してくれるパッケージです。

## How to install

melpaか直接ダウンロードでPATHが通った場所に置きます。

直接ダウンロードした場合は、

~~~el
(require 'auto-sudoedit)
~~~

auto-sudoeditはマイナーモードが有効の時に動きます。

~~~el
(auto-sudoedit-mode 1)
~~~

マイナーモードを有効にしておけば、
root権限が必要なファイルを開いたときに、
自動で開き直されます。

## 推奨

もし以下のメッセージが鬱陶しいならば、

~~~
Autosave file on local temporary directory, do you want to continue? (y or n) y
~~~

共有してないPCでは多少のセキュリティリスクを許容して、
`tramp-allow-unsafe-temporary-files`
を`t`にするのをオススメします。
