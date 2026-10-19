# diagram-viewer

Emacs の md-mode バッファ内にある Mermaid ソースから、専用の SVG ビューアを開くパッケージです。初期 MVP は **コマンド方式**です。右上ボタンや overlay は作りません。

Edit view と Rendered view の両方から、非同期描画、SVG 検査・静止化、専用キャッシュ、image-mode ベースのビューアを利用できます。md-mode の inline PNG、placeholder、render hook、PNG キャッシュは変更・置換しません。

## 導入

現時点の検証環境は **Emacs 30.2、SVG 対応 GUI、md-mode 0.5.0** です。md-mode は revision `d9e7e1d83a8601a9bda2fd604f6a0881a4f9a838` で検証しています。他の Emacs 版・md-mode revision の互換性は未確定です。標準の `markdown-mode` は別パッケージであり、今回の source adapter の対象ではありません。

必要な外部依存は Mermaid CLI (`mmdc`) と、それが利用する browser です。これらのインストールは行いません。まず通常の環境で mmdc が動くよう設定してください。

```elisp
(add-to-list 'load-path "/path/to/diagram-viewer")
(require 'diagram-viewer-adapter-mermaid)
(require 'diagram-viewer-adapter-md-mode)

;; 必要な場合だけ、既存 browser の絶対パスを指定します。
;; (setq diagram-viewer-mermaid-browser-executable "/absolute/path/to/chrome")

(diagram-viewer-mermaid-enable)
(diagram-viewer-md-mode-enable)
```

登録は一度だけ行います。設定を再評価して同じ ID を置き換える場合は、各 enable に明示的に `t` を渡してください。Core や Adapter を読み込むだけでは、外部 process の起動は行いません。

この環境では `mmdc 12.0.0` と既存 Google Chrome の明示指定で実描画を確認しました。browser 未指定時は Puppeteer の既定 browser 不在で失敗する環境だったため、明示指定しています。browser の自動探索、インストール、sandbox の緩和は行いません。

## 使い方

[サンプル文書](<examples/mermaid.md>)などを md-mode で開いて操作します。

| 表示状態 | point を置く位置 | 実行するコマンド |
|---|---|---|
| Edit view | 完全な Mermaid fence 内（開始・終了行も可） | `M-x diagram-viewer-view-at-point` |
| Rendered view | Mermaid 画像または対応 placeholder 内 | `M-x diagram-viewer-md-mode-view-rendered-at-point` |

起動コマンドは利用者が任意のキーへ割り当てられます。既存の md-mode keymap は自動変更しません。Edit コマンドに prefix を付けると applicable な source adapter を選べます。

### ビューアのキー

| キー | 動作 |
|---|---|
| `q` | window を閉じる。buffer と実行中の要求は残す |
| `g` | 現在の元本文を再取得し、保存済みキャッシュを無視して再描画 |
| `o` | 元バッファの図の範囲へ移動 |
| `s` | 表示中の検査済み SVG を保存。上書き時は確認 |
| `+` / `-` | 拡大 / 縮小 |
| `0` | 元の倍率・回転へ戻す |
| `f` | 現在の window に収める |

同じ元範囲を再度開くとビューアを再利用します。buffer を kill すると、そのビューア自身の request をキャンセルし、所有 marker を解放します。他のビューアが同じ描画 job を待っていれば、その処理は続きます。

描画中・失敗・ソース失効は状態画面で説明します。画像がない状態では倍率・保存コマンドを実行できません。ソース変更後の `g` は現在本文を再抽出し、対象が消えた場合に古い本文を再描画することはありません。Edit/Rendered を切り替えた後は、現在の表示状態からコマンドで開き直してください。

### ログとキャッシュ

- `M-x diagram-viewer-show-log`：失敗画面の診断 ID、または直近の診断を表示。履歴は最大20件です。外部 stderr にソース断片が含まれる場合があるため、共有前に確認してください。
- `M-x diagram-viewer-cache-clear`：確認後に専用 SVG キャッシュを全削除。
- ビューアで `C-u M-x diagram-viewer-cache-clear`：現在の項目だけ削除。

キャッシュを削除しても表示済み SVG の保存は可能です。実行中 job は削除で停止せず、完了後にキャッシュを再作成し得ます。md-mode の PNG キャッシュは削除しません。

## 対応範囲と制限

- Edit view は backtick/tilde の完全な top-level fence に対応します。info string の最初の token は小文字 `mermaid` が必要で、本文の末尾改行を保持します。
- list、blockquote、4 space/tab のインデント、曖昧な container 文脈は拒否します。HTML 風の行、link reference definition、先頭 YAML front matter を検出すると、その後の正当な fence も安全側で対象外とします。汎用 Markdown parser ではありません。
- Rendered view は対象 revision の4文字 placeholder 契約を検査します。PNG が未生成でも起動可能です。文字変更後は旧 descriptor を失効させ、同じ placeholder の再生成で復活させません。
- SVG の受動的な部分集合だけを受理し、能動要素・外部参照・未対応 CSS 等を拒否します。CSS animation は除去して静止化します。全 Mermaid 図種・全 theme を保証せず、HTML label や外部画像を必要とする図が拒否されることがあります。
- `diagram-viewer-mermaid-extra-arguments` は初期版では非 nil を全て拒否します。
- TTY や SVG 非対応環境では画像表示不可の理由を示します。GUI の複数 OS・複数 Emacs 版・高 DPI などを網羅した検証はしていません。
- mmdc/browser 実行中の通信・ファイルアクセスを制限する OS sandbox は提供しません。画像 decoder の脆弱性・資源使用の完全な制御も保証しません。

## 設計と検証記録

- [設計方針](<docs/DESIGN.org>) — コマンド方式 MVP と受け入れ条件
- [詳細設計](<docs/DETAIL-DESIGN.org>) — API、状態遷移、所有権、検証結果
- [依存調査記録](<docs/DEPENDENCY-NOTES.org>) — md-mode の revision・property と mmdc の調査履歴
- [SVG ポリシー](<docs/SVG-POLICY.org>) — 許可範囲、拒否、静止化、資源上限と限界
- [配置試作記録](<docs/LAYOUT-PROTOTYPE.org>) — 将来の右上ボタンに関する過去の実測

## 開発用検査

```sh
make check          # 通常の ERT、compiled ERT、厳格コンパイル、checkdoc、文書検査

# 任意の実 browser 検査
DIAGRAM_VIEWER_BROWSER=/absolute/path/to/chrome make test-real

# 実 md-mode 契約を含める場合
DIAGRAM_VIEWER_REAL_MD_MODE=1 make test \
  MD_MODE_DIR=/path/to/md-mode TEXTUI_DIR=/path/to/textui

# 独立 Wayland GUI で native 表示・倍率・保存を検証
make test-viewer-gui

# 実 PNG、Edit/Rendered コマンド、実 SVG、保存、source 復元をまとめて検証
DIAGRAM_VIEWER_BROWSER=/absolute/path/to/chrome make test-ui-gui \
  MD_MODE_DIR=/path/to/md-mode TEXTUI_DIR=/path/to/textui
```

GUI runner は自分の Emacs process を終了します。既存 server へ `emacsclient` で送らず、上記の独立プロセス用ターゲットを使ってください。Wayland 以外の環境では起動オプションの調整が必要です。

[Makefile](<Makefile>) に実行手順を定義し、生成物は `build/` に分離しています。通常検査で外部依存・GUI が必要な項目は明示的に skip します。[固定した実 Mermaid fixture](<test/fixtures/README.md>) の SVG 検査には browser は不要です。右上配置の失敗・未検証環境・将来拡張を、現在の MVP で達成したものとして扱いません。
