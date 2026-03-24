module
public import LeanBook.html
public import LeanBook.doc

/-- Typeclass for rendering a value into a target representation inside RenderM. -/
public class Handle (α : Type) (β : outParam Type) where
  handle : α → RenderM β

-- ---------------------------------------------------------------------------
-- Utility helpers
-- ---------------------------------------------------------------------------

/-- HTML-escape a plain-text string (for text nodes). -/
def escapeHtml (s : String) : String :=
  s.replace "&" "&amp;" |>.replace "<" "&lt;" |>.replace ">" "&gt;"

/-- Map an EnvType to its CSS class suffix. -/
def envClass : EnvType → String
  | .thm        => "thm"
  | .lem        => "lem"
  | .prop       => "prop"
  | .cor        => "cor"
  | .defn       => "defn"
  | .exampleEnv => "example"
  | .remark     => "remark"
  | .proof      => "proof"

/-- Map an EnvType to its display name. -/
def envName : EnvType → String
  | .thm        => "Theorem"
  | .lem        => "Lemma"
  | .prop       => "Proposition"
  | .cor        => "Corollary"
  | .defn       => "Definition"
  | .exampleEnv => "Example"
  | .remark     => "Remark"
  | .proof      => "Proof"

/-- True for environments that carry a sequential number. -/
def envIsNumbered : EnvType → Bool
  | .proof  => false
  | .remark => false
  | _       => true

/-- Extract plain text from a list of inlines (for HTML title attributes). -/
public def inlinesToPlainText : List Inline → String
  | []                   => ""
  | (.text s)   :: rest  => s ++ inlinesToPlainText rest
  | (.emph cs)  :: rest  => inlinesToPlainText cs ++ inlinesToPlainText rest
  | (.bold cs)  :: rest  => inlinesToPlainText cs ++ inlinesToPlainText rest
  | (.code s)   :: rest  => s ++ inlinesToPlainText rest
  | (.math _)   :: rest  => inlinesToPlainText rest
  | (.link cs _):: rest  => inlinesToPlainText cs ++ inlinesToPlainText rest
  | (.ref l)    :: rest  => s!"[{l}]" ++ inlinesToPlainText rest
  | (.footnote cs) :: rest => inlinesToPlainText cs ++ inlinesToPlainText rest

-- ---------------------------------------------------------------------------
-- Core renderers (partial, to handle recursive structures)
-- ---------------------------------------------------------------------------

/-- Render an Inline element to Html. -/
public partial def renderInline : Inline → RenderM Html
  | .text s => pure (Html.text (escapeHtml s))
  | .emph content => do
      let cs ← content.mapM renderInline
      return Html.element "em" [] cs
  | .bold content => do
      let cs ← content.mapM renderInline
      return Html.element "strong" [] cs
  | .code s =>
      return Html.element "code" [] [Html.text (escapeHtml s)]
  | .math tex =>
      return Html.element "span"
        [("class", "math-inline"), ("data-source", s!"${tex}$")]
        [Html.text s!"\\({tex}\\)"]
  | .link content url => do
      let cs ← content.mapM renderInline
      return Html.element "a" [("href", url)] cs
  | .ref label =>
      return Html.element "a"
        [("href", s!"#{label}"), ("class", "xref")]
        [Html.text s!"[{label}]"]
  | .footnote content => do
      let cs ← content.mapM renderInline
      return Html.element "span" [("class", "footnote")] cs

/-- Render a Block element to Html. -/
public partial def renderBlock : Block → RenderM Html
  | .para inlines => do
      let cs ← inlines.mapM renderInline
      return Html.element "p" [("class", "col-main")] cs

  | .heading level title label => do
      -- Increment section counter only for top-level headings (level 1)
      if level == 1 then
        modify (fun st => { st with sectionNum := st.sectionNum + 1 })
      let cs ← title.mapM renderInline
      let tag := s!"h{Nat.min (level + 1) 6}"
      let baseAttrs := [("class", s!"col-main heading-lv{level}")]
      let attrs := match label with
        | some l => ("id", l) :: baseAttrs
        | none   => baseAttrs
      return Html.element tag attrs cs

  | .displayMath tex label => do
      modify (fun st => { st with equationNum := st.equationNum + 1 })
      let st ← get
      let numStr := s!"({st.chapterNum}.{st.equationNum})"
      let baseAttrs := [("class", "math-display col-main"),
                        ("data-source", s!"$${tex}$$")]
      let attrs := match label with
        | some l => ("id", l) :: baseAttrs
        | none   => baseAttrs
      return Html.element "div" attrs [
        Html.element "span" [("class", "math-content")]
          [Html.text s!"\\[{tex}\\]"],
        Html.element "span" [("class", "eq-number")] [Html.text numStr]
      ]

  | .codeBlock lang source => do
      let langAttrs := if lang.isEmpty then []
                       else [("class", s!"language-{lang}")]
      return Html.element "pre" [("class", "col-main code-block")] [
        Html.element "code" langAttrs [Html.text (escapeHtml source)]
      ]

  | .list items ordered => do
      let renderItem (blocks : List Block) : RenderM Html := do
        let cs ← blocks.mapM renderBlock
        return Html.element "li" [] cs
      let itemsHtml ← items.mapM renderItem
      let tag := if ordered then "ol" else "ul"
      return Html.element tag [("class", "col-main")] itemsHtml

  | .blockquote blocks => do
      let cs ← blocks.mapM renderBlock
      return Html.element "blockquote" [("class", "col-main")] cs

  | .figure url caption label => do
      modify (fun st => { st with figureNum := st.figureNum + 1 })
      let st ← get
      let numStr := s!"Figure {st.chapterNum}.{st.figureNum}"
      let capHtml ← match caption with
        | some inlines => do
            let cs ← inlines.mapM renderInline
            pure [Html.element "figcaption" []
              (Html.element "strong" [] [Html.text numStr] ::
               Html.text ": " :: cs)]
        | none => pure []
      let baseAttrs := [("class", "col-main figure")]
      let attrs := match label with
        | some l => ("id", l) :: baseAttrs
        | none   => baseAttrs
      return Html.element "figure" attrs
        (Html.element "img" [("src", url), ("alt", numStr)] [] :: capHtml)

  | .diagram svg caption label => do
      modify (fun st => { st with figureNum := st.figureNum + 1 })
      let st ← get
      let numStr := s!"Figure {st.chapterNum}.{st.figureNum}"
      let capHtml ← match caption with
        | some inlines => do
            let cs ← inlines.mapM renderInline
            pure [Html.element "figcaption" []
              (Html.element "strong" [] [Html.text numStr] ::
               Html.text ": " :: cs)]
        | none => pure []
      let baseAttrs := [("class", "col-main diagram")]
      let attrs := match label with
        | some l => ("id", l) :: baseAttrs
        | none   => baseAttrs
      return Html.element "figure" attrs (Html.text svg :: capHtml)

  | .env envType title label blocks => do
      -- Increment theorem counter for numbered environments
      let numStr ← if envIsNumbered envType then do
          modify (fun st => { st with theoremNum := st.theoremNum + 1 })
          let st ← get
          pure s!" {st.chapterNum}.{st.theoremNum}"
        else
          pure ""
      -- Render optional title
      let titleHtml ← match title with
        | some ts => do
            let cs ← ts.mapM renderInline
            pure [Html.element "span" [("class", "env-title")]
              (Html.text " (" :: cs ++ [Html.text ")"])]
        | none => pure []
      -- Build header
      let headerHtml := Html.element "div" [("class", "env-header")] (
        Html.element "span" [("class", "env-type-name")]
          [Html.text (envName envType)] ::
        Html.element "span" [("class", "env-number")]
          [Html.text numStr] ::
        titleHtml
      )
      -- Render body blocks
      let bodyHtml ← blocks.mapM renderBlock
      let baseAttrs := [("class", s!"env env-{envClass envType} col-main")]
      let attrs := match label with
        | some l => ("id", l) :: baseAttrs
        | none   => baseAttrs
      return Html.element "div" attrs (headerHtml :: bodyHtml)

  | .marginNote blocks => do
      let cs ← blocks.mapM renderBlock
      return Html.element "aside" [("class", "col-margin margin-note")] cs

-- ---------------------------------------------------------------------------
-- Typeclass instances
-- ---------------------------------------------------------------------------

public instance : Handle Inline Html where
  handle := renderInline

public instance : Handle Block Html where
  handle := renderBlock

-- ---------------------------------------------------------------------------
-- Chapter page rendering
-- ---------------------------------------------------------------------------

/-- Build a full standalone HTML page for one chapter.
    docTitle   – plain-text book title (for <title> tag and navigation)
    chNum      – 1-based chapter number
    total      – total number of chapters (for next/prev links)
    ch         – the Chapter to render -/
public def renderChapterPage
    (docTitle : String) (chNum : Nat) (total : Nat) (ch : Chapter)
    : RenderM Html := do
  -- Reset per-chapter counters and set chapter number
  modify (fun st => { st with
    chapterNum  := chNum
    sectionNum  := 0
    equationNum := 0
    figureNum   := 0
    theoremNum  := 0
  })
  -- Chapter title
  let titleInlines ← ch.title.mapM renderInline
  let chTitleAttrs : List (String × String) :=
    [("class", "col-main chapter-title")] ++
    (match ch.label with | some l => [("id", l)] | none => [])
  let chTitleHtml := Html.element "h1" chTitleAttrs (
    Html.element "span" [("class", "chapter-number")]
      [Html.text s!"Chapter {chNum}"] ::
    Html.text ". " ::
    titleInlines
  )
  -- Content blocks
  let blocksHtml ← ch.blocks.mapM renderBlock
  -- Prev / next navigation
  let prevLink : List Html :=
    if chNum > 1 then
      [Html.element "a"
        [("href", s!"chapter-{chNum - 1}.html"), ("class", "nav-link nav-prev")]
        [Html.text "← Previous Chapter"]]
    else []
  let nextLink : List Html :=
    if chNum < total then
      [Html.element "a"
        [("href", s!"chapter-{chNum + 1}.html"), ("class", "nav-link nav-next")]
        [Html.text "Next Chapter →"]]
    else []
  let navHtml := Html.element "nav" [("class", "chapter-nav")]
    (prevLink ++ nextLink)
  -- Help button (fixed corner, pure CSS/HTML)
  let helpBtn := Html.element "button"
    [("class", "help-btn"), ("aria-label", "Help")]
    [Html.text "?"]
  -- Page <title>
  let plainTitle := inlinesToPlainText ch.title
  let pageTitle  := s!"Chapter {chNum}: {plainTitle} — {docTitle}"
  -- MathJax configuration: keep source in data-source for clipboard copying
  let mathJaxCfg := Html.element "script" [] [Html.text
    "window.MathJax = { options: { skipHtmlTags: ['script','noscript','style','textarea','pre'] } };"]
  -- <head>
  let head := Html.element "head" [] [
    Html.element "meta" [("charset", "UTF-8")] [],
    Html.element "meta"
      [("name", "viewport"), ("content", "width=device-width, initial-scale=1.0")] [],
    Html.element "title" [] [Html.text pageTitle],
    Html.element "link" [("rel", "stylesheet"), ("href", "style.css")] [],
    mathJaxCfg,
    Html.element "script" [
      ("id", "MathJax-script"), ("async", ""),
      ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
    ] []
  ]
  -- <body>
  let body := Html.element "body" [] [
    Html.element "div" [("class", "chapter-grid")]
      (chTitleHtml :: blocksHtml),
    navHtml,
    helpBtn
  ]
  return Html.element "html" [("lang", "en")] [head, body]

public instance : Handle Chapter Html where
  handle ch := renderChapterPage "" 1 1 ch

/-- Handle Document produces a single combined HTML (all chapters in sequence).
    For per-chapter file output, use renderChapterPage in a loop (see Main.lean). -/
public instance : Handle Document Html where
  handle doc := do
    let total := doc.chapters.length
    let plainDocTitle := inlinesToPlainText doc.title
    -- Render each chapter as a page section, threading state through
    let pagesHtml ← doc.chapters.mapIdxM (fun i ch =>
      renderChapterPage plainDocTitle (i + 1) total ch)
    -- Combine into one HTML document
    let head := Html.element "head" [] [
      Html.element "meta" [("charset", "UTF-8")] [],
      Html.element "title" [] [Html.text plainDocTitle],
      Html.element "link" [("rel", "stylesheet"), ("href", "style.css")] [],
      Html.element "script" [
        ("id", "MathJax-script"), ("async", ""),
        ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
      ] []
    ]
    let body := Html.element "body" [] pagesHtml
    return Html.element "html" [("lang", "en")] [head, body]
