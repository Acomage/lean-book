module
import LeanBook.doc
import LeanBook.html
import LeanBook.utils
import LeanBook.pass1

-- ---------------------------------------------------------------------------
-- Typeclass for rendering a value into a target representation inside RenderM.
-- ---------------------------------------------------------------------------

/-- Typeclass for rendering a value into a target representation inside RenderM. -/
public class Handle (α : Type) (β : outParam Type) where
  handle : α → RenderM β

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
  | .ref label => do
      let st ← get
      match lookupLabel label st.labels with
      | some (url, displayText) =>
        return Html.element "a"
          [("href", url), ("class", "xref")]
          [Html.text displayText]
      | none =>
        return Html.element "a"
          [("href", s!"#{label}"), ("class", "xref-broken")]
          [Html.text s!"??[{label}]??"]
  | .footnote content => do
      let cs ← content.mapM renderInline
      return Html.element "span" [("class", "footnote")] cs

/-- Render a Block element to Html. -/
public partial def renderBlock : Block → RenderM Html
  | .para inlines => do
      let cs ← inlines.mapM renderInline
      return Html.element "p" [("class", "col-main")] cs

  | .heading level title label => do
      if level == 1 then
        modify (fun st => { st with sectionNum := st.sectionNum + 1 })
      let cs ← title.mapM renderInline
      let tag := s!"h{Nat.min (level + 1) 6}"
      let attrs := addLabelAttr label [("class", s!"col-main heading-lv{level}")]
      return Html.element tag attrs cs

  | .displayMath tex label => do
      modify (fun st => { st with equationNum := st.equationNum + 1 })
      let st ← get
      let numStr := s!"({st.chapterNum}.{st.equationNum})"
      let attrs := addLabelAttr label [("class", "math-display col-main"),
                                       ("data-source", s!"$${tex}$$")]
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
      let attrs := addLabelAttr label [("class", "col-main figure")]
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
      let attrs := addLabelAttr label [("class", "col-main diagram")]
      return Html.element "figure" attrs (Html.text svg :: capHtml)

  | .env envType title label blocks => do
      let numStr ← if envIsNumbered envType then do
          modify (fun st => { st with theoremNum := st.theoremNum + 1 })
          let st ← get
          pure s!" {st.chapterNum}.{st.theoremNum}"
        else
          pure ""
      let titleHtml ← match title with
        | some ts => do
            let cs ← ts.mapM renderInline
            pure [Html.element "span" [("class", "env-title")]
              (Html.text " (" :: cs ++ [Html.text ")"])]
        | none => pure []
      let headerHtml := Html.element "div" [("class", "env-header")] (
        Html.element "span" [("class", "env-type-name")]
          [Html.text (envName envType)] ::
        Html.element "span" [("class", "env-number")]
          [Html.text numStr] ::
        titleHtml
      )
      let bodyHtml ← blocks.mapM renderBlock
      let attrs := addLabelAttr label
        [("class", s!"env env-{envClass envType} col-main")]
      return Html.element "div" attrs (headerHtml :: bodyHtml)

  | .marginNote blocks => do
      let cs ← blocks.mapM renderBlock
      return Html.element "aside" [("class", "col-margin margin-note")] cs

-- ---------------------------------------------------------------------------
-- Typeclass instances for Inline and Block
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
  modify (resetChapterCounters chNum)
  let titleInlines ← ch.title.mapM renderInline
  let chTitleAttrs := addLabelAttr ch.label [("class", "col-main chapter-title")]
  let chTitleHtml := Html.element "h1" chTitleAttrs (
    Html.element "span" [("class", "chapter-number")]
      [Html.text s!"Chapter {chNum}"] ::
    Html.text ". " ::
    titleInlines
  )
  let blocksHtml ← ch.blocks.mapM renderBlock
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
  let helpBtn := Html.element "button"
    [("class", "help-btn"), ("aria-label", "Help")]
    [Html.text "?"]
  let plainTitle := inlinesToPlainText ch.title
  let pageTitle  := s!"Chapter {chNum}: {plainTitle} — {docTitle}"
  let mathJaxCfg := Html.element "script" [] [Html.text
    "window.MathJax = { options: { skipHtmlTags: ['script','noscript','style','textarea','pre'] } };"]
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
  let body := Html.element "body" [] [
    Html.element "div" [("class", "chapter-grid")]
      (chTitleHtml :: blocksHtml),
    navHtml,
    helpBtn
  ]
  return Html.element "html" [("lang", "en")] [head, body]

-- ---------------------------------------------------------------------------
-- Typeclass instances for Chapter and Document
-- ---------------------------------------------------------------------------

public instance : Handle Chapter Html where
  handle ch := renderChapterPage "" 1 1 ch

/-- Handle Document produces a single combined HTML (all chapters in sequence).
    For per-chapter file output, use renderChapterPage in a loop (see Main.lean). -/
public instance : Handle Document Html where
  handle doc := do
    let total := doc.chapters.length
    let plainDocTitle := inlinesToPlainText doc.title
    let pagesHtml ← doc.chapters.mapIdxM (fun i ch =>
      renderChapterPage plainDocTitle (i + 1) total ch)
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
