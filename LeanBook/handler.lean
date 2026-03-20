module
public import LeanBook.html
public import LeanBook.doc

public class Handle (α : Type) (β : outParam Type) where
  handle : α → β

public instance : Handle Inline Html where
  handle
    | .text s => Html.element "span" [] [Html.text s]
    | .math tex => Html.element "span" [("class", "math inline"), ("data-source", s!"${tex}$")] [Html.text s!"\\({tex}\\)"]

public instance : Handle Block Html where
  handle
    | .para inlines =>
        let children := inlines.map (Handle.handle (β := Html))
        Html.element "p" [] children
    | .math tex =>
        Html.element "div" [("class", "math display"), ("data-source", s!"$${tex}$$")] [Html.text s!"\\[{tex}\\]"]
    | .code cb =>
        let boxHtml (child : Html) :=
          Html.element "div"
            [("class", "code-box"),
             ("style", s!"width: {cb.box.wide}ch; background-color: #000000; color: #FFFFFF;"),
             ("data-source", cb.code)]
            [child]
        let codeHtml := Html.element "pre" [] [Html.element "code" [] [Html.text cb.code]]
        boxHtml codeHtml

public instance : Handle Chapter Html where
  handle ch :=
    let titleHtml := Html.element "h2" [("class", "chapter-title")] [Html.text ch.title]
    let blocksHtml := ch.blocks.map (Handle.handle (β := Html))
    Html.element "section" [("class", "chapter")] (titleHtml :: blocksHtml)

public instance : Handle Document Html where
  handle doc :=
    let head := Html.element "head" [] [
      Html.element "meta" [("charset", "UTF-8")] [],
      Html.element "title" [] [Html.text doc.title],
      Html.element "link" [("rel", "stylesheet"), ("href", "style.css")] [],
      -- 引入 MathJax 用于公式渲染
      Html.element "script" [("id", "MathJax-script"), ("async", "true"), ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")] []
    ]
    let body := Html.element "body" [] [
      Html.element "main" [("class", "main-content")] (doc.chapters.map (Handle.handle (β := Html))),
      Html.element "script" [("src", "main.js")] []
    ]
    Html.element "html" [("lang", "en")] [head, body]
