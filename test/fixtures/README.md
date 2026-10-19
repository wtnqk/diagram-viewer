# Mermaid SVG fixtures

2026-10-16 にローカルの Mermaid CLI 12.0.0 と既存 Google Chrome を使って生成した、SVG validator の互換性検査用データです。browser の明示パスは `/usr/sbin/google-chrome`、追加インストールと `--no-sandbox` 等の sandbox 緩和は行っていません。

| Fixture | 図種 | Theme |
|---|---|---|
| [flowchart](<mermaid-flowchart.svg>) | flowchart | default |
| [sequence](<mermaid-sequence.svg>) | sequence | dark |
| [class](<mermaid-class.svg>) | class | neutral |

生成時の内部設定は root/flowchart の `htmlLabels: false`、`securityLevel: strict` と、安全設定の source init override を抑止する `secure` の指定です。詳細は [Mermaid renderer](<../../diagram-viewer-adapter-mermaid.el>) と [SVG ポリシー](<../../docs/SVG-POLICY.org>) を参照してください。

これらは **正規化前の実 CLI 出力**です。既定 CSS には未使用の animation/keyframes、drop-shadow、空 declaration/属性も含まれます。[SVG ERT](<../diagram-viewer-svg-test.el>) は、この出力を制限付き validator が受理して静止化できること、正規化が冪等であることを検査します。SVG をそのまま image decoder へ渡すテストではありません。

SVG のバイト列全体を golden 値として比較せず、全 Mermaid 図種・全 theme の互換性を代表するとは主張しません。生成元の一時 `.mmd` と Chrome の詳細版は保存していないため、元のバイト列を完全再生成する記録ではありません。固定 fixture の検査と、その時点の CLI/browser を使う任意の実描画 ERT を分けています。
