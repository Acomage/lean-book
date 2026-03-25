module
public import LeanBook.doc
public import LeanBook.html
public import LeanBook.utils
public import LeanBook.pass1

-- ---------------------------------------------------------------------------
-- Typeclass for rendering a value into a target representation inside RenderM.
-- ---------------------------------------------------------------------------

/-- Typeclass for rendering a value into a target representation inside RenderM. -/
public class Handle (α : Type) (β : outParam Type) where
  handle : α → RenderM β

-- ---------------------------------------------------------------------------
-- Core renderer for Inline elements (partial, to handle recursive structure)
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

-- ---------------------------------------------------------------------------
-- BlockNode instances for the built-in concrete block types
--
-- Each instance provides:
--   toHtml        – renders to Html via renderInline / AnyBlock.toHtml
--   toSource      – recovers source text (for future copy interception)
--   collectLabels – mirrors the counter logic from Pass 1
-- ---------------------------------------------------------------------------

public instance : BlockNode ParaBlock where
  toHtml b := do
    let cs ← b.inlines.mapM renderInline
    return Html.element "p" [("class", "col-main")] cs
  toSource b := inlinesToSource b.inlines

public instance : BlockNode HeadingBlock where
  toHtml h := do
    if h.level == 1 then
      modify (fun st => { st with sectionNum := st.sectionNum + 1 })
    let cs ← h.title.mapM renderInline
    let tag := s!"h{Nat.min (h.level + 1) 6}"
    let attrs := addLabelAttr h.label [("class", s!"col-main heading-lv{h.level}")]
    return Html.element tag attrs cs
  toSource h :=
    -- level is 0-indexed below the chapter title (level 1 → ## section, level 2 → ### subsection)
    String.mk (List.replicate (h.level + 1) '#') ++ " " ++ inlinesToSource h.title
  collectLabels h chNum fileUrl := do
    if h.level == 1 then
      modify (fun st => { st with sectionNum := st.sectionNum + 1 })
    match h.label with
    | none   => pure ()
    | some l => do
        let st ← get
        let displayText := s!"Section {chNum}.{st.sectionNum}"
        modify (fun st => { st with
          labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })

public instance : BlockNode DisplayMathBlock where
  toHtml d := do
    modify (fun st => { st with equationNum := st.equationNum + 1 })
    let st ← get
    let numStr := s!"({st.chapterNum}.{st.equationNum})"
    let attrs := addLabelAttr d.label [("class", "math-display col-main"),
                                       ("data-source", s!"$${d.tex}$$")]
    return Html.element "div" attrs [
      Html.element "span" [("class", "math-content")]
        [Html.text s!"\\[{d.tex}\\]"],
      Html.element "span" [("class", "eq-number")] [Html.text numStr]
    ]
  toSource d := s!"$${d.tex}$$"
  collectLabels d chNum fileUrl := do
    modify (fun st => { st with equationNum := st.equationNum + 1 })
    match d.label with
    | none   => pure ()
    | some l => do
        let st ← get
        let displayText := s!"({chNum}.{st.equationNum})"
        modify (fun st => { st with
          labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })

public instance : BlockNode CodeBlock where
  toHtml c := do
    let langAttrs := if c.lang.isEmpty then []
                     else [("class", s!"language-{c.lang}")]
    return Html.element "pre" [("class", "col-main code-block")] [
      Html.element "code" langAttrs [Html.text (escapeHtml c.source)]
    ]
  toSource c :=
    "```" ++ c.lang ++ "\n" ++ c.source ++ "\n```"

public instance : BlockNode ListBlock where
  toHtml l := do
    let renderItem (blocks : List AnyBlock) : RenderM Html := do
      let cs ← blocks.mapM AnyBlock.toHtml
      return Html.element "li" [] cs
    let itemsHtml ← l.items.mapM renderItem
    let tag := if l.ordered then "ol" else "ul"
    return Html.element tag [("class", "col-main")] itemsHtml
  toSource l :=
    String.join (l.items.map (fun blks =>
      (if l.ordered then "1. " else "- ") ++
      String.join (blks.map AnyBlock.toSource) ++ "\n"))
  collectLabels l chNum fileUrl :=
    l.items.forM (fun blks => blks.forM (fun b => b.collectLabels chNum fileUrl))

public instance : BlockNode BlockquoteBlock where
  toHtml bq := do
    let cs ← bq.blocks.mapM AnyBlock.toHtml
    return Html.element "blockquote" [("class", "col-main")] cs
  toSource bq :=
    String.join (bq.blocks.map (fun b => "> " ++ AnyBlock.toSource b ++ "\n"))
  collectLabels bq chNum fileUrl :=
    bq.blocks.forM (fun b => b.collectLabels chNum fileUrl)

public instance : BlockNode FigureBlock where
  toHtml f := do
    modify (fun st => { st with figureNum := st.figureNum + 1 })
    let st ← get
    let numStr := s!"Figure {st.chapterNum}.{st.figureNum}"
    let capHtml ← match f.caption with
      | some inlines => do
          let cs ← inlines.mapM renderInline
          pure [Html.element "figcaption" []
            (Html.element "strong" [] [Html.text numStr] ::
             Html.text ": " :: cs)]
      | none => pure []
    let attrs := addLabelAttr f.label [("class", "col-main figure")]
    return Html.element "figure" attrs
      (Html.element "img" [("src", f.url), ("alt", numStr)] [] :: capHtml)
  toSource f := s!"![{f.caption.map inlinesToSource |>.getD ""}]({f.url})"
  collectLabels f chNum fileUrl := do
    modify (fun st => { st with figureNum := st.figureNum + 1 })
    match f.label with
    | none   => pure ()
    | some l => do
        let st ← get
        let displayText := s!"Figure {chNum}.{st.figureNum}"
        modify (fun st => { st with
          labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })

public instance : BlockNode DiagramBlock where
  toHtml d := do
    modify (fun st => { st with figureNum := st.figureNum + 1 })
    let st ← get
    let numStr := s!"Figure {st.chapterNum}.{st.figureNum}"
    let capHtml ← match d.caption with
      | some inlines => do
          let cs ← inlines.mapM renderInline
          pure [Html.element "figcaption" []
            (Html.element "strong" [] [Html.text numStr] ::
             Html.text ": " :: cs)]
      | none => pure []
    let attrs := addLabelAttr d.label [("class", "col-main diagram")]
    return Html.element "figure" attrs (Html.text d.svg :: capHtml)
  toSource d := d.svg
  collectLabels d chNum fileUrl := do
    modify (fun st => { st with figureNum := st.figureNum + 1 })
    match d.label with
    | none   => pure ()
    | some l => do
        let st ← get
        let displayText := s!"Figure {chNum}.{st.figureNum}"
        modify (fun st => { st with
          labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })

public instance : BlockNode EnvBlock where
  toHtml e := do
    let numStr ← if envIsNumbered e.type then do
        modify (fun st => { st with theoremNum := st.theoremNum + 1 })
        let st ← get
        pure s!" {st.chapterNum}.{st.theoremNum}"
      else
        pure ""
    let titleHtml ← match e.title with
      | some ts => do
          let cs ← ts.mapM renderInline
          pure [Html.element "span" [("class", "env-title")]
            (Html.text " (" :: cs ++ [Html.text ")"])]
      | none => pure []
    let headerHtml := Html.element "div" [("class", "env-header")] (
      Html.element "span" [("class", "env-type-name")]
        [Html.text (envName e.type)] ::
      Html.element "span" [("class", "env-number")]
        [Html.text numStr] ::
      titleHtml
    )
    let bodyHtml ← e.blocks.mapM AnyBlock.toHtml
    let attrs := addLabelAttr e.label
      [("class", s!"env env-{envClass e.type} col-main")]
    return Html.element "div" attrs (headerHtml :: bodyHtml)
  toSource e :=
    envName e.type ++ ".\n" ++
    String.join (e.blocks.map (fun b => AnyBlock.toSource b ++ "\n"))
  collectLabels e chNum fileUrl := do
    if envIsNumbered e.type then
      modify (fun st => { st with theoremNum := st.theoremNum + 1 })
    match e.label with
    | none   => pure ()
    | some l => do
        let st ← get
        let displayText :=
          if envIsNumbered e.type
            then s!"{envName e.type} {chNum}.{st.theoremNum}"
            else envName e.type
        modify (fun st => { st with
          labels := (l, (s!"{fileUrl}#{l}", displayText)) :: st.labels })
    e.blocks.forM (fun b => b.collectLabels chNum fileUrl)

public instance : BlockNode MarginNoteBlock where
  toHtml m := do
    let cs ← m.blocks.mapM AnyBlock.toHtml
    return Html.element "aside" [("class", "col-margin margin-note")] cs
  toSource m :=
    String.join (m.blocks.map (fun b => AnyBlock.toSource b ++ "\n"))
  collectLabels m chNum fileUrl :=
    m.blocks.forM (fun b => b.collectLabels chNum fileUrl)

-- ---------------------------------------------------------------------------
-- Handle instance: dispatch AnyBlock → Html via the stored closure
-- ---------------------------------------------------------------------------

public instance : Handle AnyBlock Html where
  handle b := b.toHtml

-- ---------------------------------------------------------------------------
-- Handle instance for Inline (unchanged)
-- ---------------------------------------------------------------------------

public instance : Handle Inline Html where
  handle := renderInline

-- ---------------------------------------------------------------------------
-- Convenience functions: smart constructors that mirror the old `Block` API
--
-- These functions wrap a concrete block value into an `AnyBlock`, using the
-- same argument lists as the removed `Block` inductive constructors.
-- Existing document definitions that used `Block.para`, `Block.env`, etc.
-- continue to work without modification.
-- ---------------------------------------------------------------------------

public def Block.para (inlines : List Inline) : AnyBlock :=
  AnyBlock.mk (ParaBlock.mk inlines)
public def Block.heading (level : Nat) (title : List Inline) (label : Option String) : AnyBlock :=
  AnyBlock.mk (HeadingBlock.mk level title label)
public def Block.displayMath (tex : String) (label : Option String) : AnyBlock :=
  AnyBlock.mk (DisplayMathBlock.mk tex label)
public def Block.codeBlock (lang : String) (source : String) : AnyBlock :=
  AnyBlock.mk (CodeBlock.mk lang source)
public def Block.list (items : List (List AnyBlock)) (ordered : Bool) : AnyBlock :=
  AnyBlock.mk (ListBlock.mk items ordered)
public def Block.blockquote (blocks : List AnyBlock) : AnyBlock :=
  AnyBlock.mk (BlockquoteBlock.mk blocks)
public def Block.figure (url : String) (caption : Option (List Inline)) (label : Option String) : AnyBlock :=
  AnyBlock.mk (FigureBlock.mk url caption label)
public def Block.diagram (svg : String) (caption : Option (List Inline)) (label : Option String) : AnyBlock :=
  AnyBlock.mk (DiagramBlock.mk svg caption label)
public def Block.env (type : EnvType) (title : Option (List Inline)) (label : Option String)
    (blocks : List AnyBlock) : AnyBlock :=
  AnyBlock.mk (EnvBlock.mk type title label blocks)
public def Block.marginNote (blocks : List AnyBlock) : AnyBlock :=
  AnyBlock.mk (MarginNoteBlock.mk blocks)

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
  let blocksHtml ← ch.blocks.mapM AnyBlock.toHtml
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
