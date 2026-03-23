module
public import LeanBook.html
public import LeanBook.doc

public class Handle (α : Type) (β : outParam Type) where
  handle : α → RenderM β

public instance : Handle Box (Html → Html) where
  handle b := do
    let st ← get
    let id := st.nextId
    let cName := s!"box-{id}"
    let rule : CssRule := {
      selector := s!".{cName}",
      properties := [("width", s!"{b.wide}ch"), ("background-color", "var(--bg-color)")]
    }
    set { st with rules := st.rules.push rule, nextId := id + 1 }
    return fun childHtml => Html.element "div" [("class", cName)] [childHtml]

namespace CodeBlock

public scoped instance : Handle Box (Html → Html) where
  handle b := do
    let st ← get
    let id := st.nextId
    let cName := s!"code-box-{id}"
    let rule : CssRule := {
      selector := s!".{cName}",
      properties := [
        ("width", s!"{b.wide}ch"),
        ("background-color", "#282c34"),
        ("color", "#abb2bf"),
        ("padding", "1rem"),
        ("border-radius", "6px"),
        ("overflow-x", "auto")
      ]
    }
    set { st with rules := st.rules.push rule, nextId := id + 1 }
    return fun childHtml => Html.element "div" [("class", cName)] [childHtml]

public instance : Handle CodeBlock Html where
  handle cb := do
    let boxWrapper ← Handle.handle (β := Html → Html) cb.box
    let codeHtml := Html.element "pre" [] [Html.element "code" [] [Html.text cb.code]]
    return boxWrapper codeHtml
end CodeBlock

public instance : Handle Inline Html where
  handle
    | .text s => pure <| Html.element "span" [] [Html.text s]
    | .math tex => pure <| Html.element "span" [("class", "math inline"), ("data-source", s!"${tex}$")] [Html.text s!"\\({tex}\\)"]

public instance : Handle Block Html where
  handle
    | .para inlines => do
        let children ← inlines.mapM (Handle.handle (β := Html))
        return Html.element "p" [] children
    | .math tex => pure <| Html.element "div" [("class", "math display"), ("data-source", s!"$${tex}$$")] [Html.text s!"\\[{tex}\\]"]
    | .code cb => Handle.handle cb

public instance : Handle Chapter Html where
  handle ch := do
    let titleHtml := Html.element "h2" [("class", "chapter-title")] [Html.text ch.title]
    let blocksHtml ← ch.blocks.mapM (Handle.handle (β := Html))
    return Html.element "section" [("class", "chapter")] (titleHtml :: blocksHtml)

public instance : Handle Document Html where
  handle doc := do
    let chaptersHtml ← doc.chapters.mapM (Handle.handle (β := Html))
    let head := Html.element "head" [] [
      Html.element "meta" [("charset", "UTF-8")] [],
      Html.element "title" [] [Html.text doc.title],
      Html.element "link" [("rel", "stylesheet"), ("href", "style.css")] [],
      Html.element "script" [("id", "MathJax-script"), ("async", "true"), ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")] []
    ]
    let body := Html.element "body" [] [
      Html.element "main" [("class", "main-content")] chaptersHtml,
      Html.element "script" [("src", "main.js")] []
    ]
    return Html.element "html" [("lang", "en")] [head, body]
